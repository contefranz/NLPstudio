# Internal topic coherence metrics
#
# Implements UMass (Mimno et al. 2011) and NPMI (Aletras & Stevenson 2013)
# coherence. Both are computed from the training document-term matrix and the
# stored TWW. Not exported; called from evaluate_topic_model().
#
# References:
# Mimno, D., Wallach, H., Talley, E., Leenders, M., & McCallum, A. (2011).
#   Optimizing semantic coherence in topic models. EMNLP, 262-272.
#
# Aletras, N., & Stevenson, M. (2013). Evaluating topic coherence using
#   distributional semantics. EACL, 13-22.

#' Binarize and align a training matrix to a fitted vocabulary
#'
#' Converts the training input to a binary presence/absence dgCMatrix whose
#' columns correspond exactly to `vocab`. Terms present in `training` but
#' absent from `vocab` are dropped; terms in `vocab` absent from `training`
#' become all-zero columns (contributing zero to all co-occurrence counts).
#'
#' @param training dgCMatrix / dfm / DocumentTermMatrix.
#' @param vocab Character vector of terms (the fitted vocabulary).
#' @returns A binary dgCMatrix with one row per document and one column per
#'   vocabulary term, in the same order as `vocab`.
#' @keywords internal
#' @noRd
.coherence_prepare_training <- function(training, vocab) {
  mat <- .as_topic_dgCMatrix(training)
  if (nrow(mat) == 0L) stop("'training' has no documents.", call. = FALSE)

  train_terms <- colnames(mat)
  if (is.null(train_terms)) {
    stop("'training' must have column names (term names).", call. = FALSE)
  }

  # Warn when many vocabulary terms are absent (likely wrong training corpus)
  common    <- intersect(vocab, train_terms)
  n_missing <- length(vocab) - length(common)
  if (n_missing > 0L) {
    pct <- 100 * n_missing / length(vocab)
    if (pct > 10) {
      warning(sprintf(
        "%d of %d vocabulary terms (%.0f%%) are absent from 'training'. Ensure 'training' is the corpus used to fit the model.",
        n_missing, length(vocab), pct
      ), call. = FALSE)
    }
  }

  # Build vocabulary-aligned sparse matrix (D x V)
  if (identical(train_terms, vocab)) {
    aligned <- mat
  } else if (length(common) == length(vocab)) {
    aligned <- mat[, match(vocab, train_terms), drop = FALSE]
  } else {
    aligned <- Matrix::sparseMatrix(
      i     = integer(0L),
      j     = integer(0L),
      x     = numeric(0L),
      dims  = c(nrow(mat), length(vocab)),
      dimnames = list(rownames(mat), vocab)
    )
    cv <- match(common, vocab)
    ct <- match(common, train_terms)
    aligned[, cv] <- mat[, ct, drop = FALSE]
  }

  # Binarize in-place (1 where any count, 0 elsewhere)
  aligned@x[] <- 1
  aligned
}

#' Compute per-topic UMass and NPMI coherence
#'
#' @param tww_matrix Numeric matrix (\eqn{K} x V): rows = topics, cols = terms,
#'   values = word probabilities phi. Rownames are Topic### identifiers;
#'   colnames are vocabulary terms.
#' @param training Unprocessed training input (dgCMatrix/dfm/DocumentTermMatrix).
#' @param top_n Number of top terms per topic used for coherence computation.
#' @param epsilon Small positive constant for numerical stability.
#' @returns A named list with elements `umass` and `npmi`, each a numeric
#'   vector of length \eqn{K} (one value per topic).
#' @keywords internal
#' @noRd
.compute_coherence <- function(tww_matrix, training, top_n, epsilon) {
  k     <- nrow(tww_matrix)
  vocab <- colnames(tww_matrix)
  V     <- length(vocab)

  binary_dtm <- .coherence_prepare_training(training, vocab)
  doc_freq   <- Matrix::colSums(binary_dtm)  # D(w), length V
  D_total    <- nrow(binary_dtm)

  # Top-N term positions per topic (indices into vocab columns)
  # Sorted by decreasing phi so that position 1 = most probable term.
  top_n_eff <- min(as.integer(top_n), V)
  top_idx_list <- lapply(seq_len(k), function(t) {
    order(tww_matrix[t, ], decreasing = TRUE)[seq_len(top_n_eff)]
  })

  # Co-occurrence is only needed for the union of all top-N terms.
  all_top <- sort(unique(unlist(top_idx_list, use.names = FALSE)))
  binary_sub <- binary_dtm[, all_top, drop = FALSE]
  # cooc_sub[i, j] = number of docs containing both terms all_top[i] and all_top[j]
  cooc_sub <- Matrix::crossprod(binary_sub)

  # Reverse lookup: vocab index -> position in all_top (0 = not in union)
  sub_from_vocab <- integer(V)
  sub_from_vocab[all_top] <- seq_along(all_top)

  umass_per_topic <- numeric(k)
  npmi_per_topic  <- numeric(k)

  for (t in seq_len(k)) {
    top_idx <- top_idx_list[[t]]
    n_t     <- length(top_idx)

    if (n_t < 2L) {
      umass_per_topic[t] <- NA_real_
      npmi_per_topic[t]  <- NA_real_
      next
    }

    # All unordered pairs (hi, lo): hi = lower index = higher rank = more probable
    pairs   <- utils::combn(n_t, 2L)  # 2 x n_pairs matrix
    n_pairs <- ncol(pairs)

    # Position in cooc_sub for each side of each pair
    sub_hi <- sub_from_vocab[top_idx[pairs[1L, ]]]
    sub_lo <- sub_from_vocab[top_idx[pairs[2L, ]]]

    # Fetch co-occurrence counts and marginal document frequencies
    d_hij <- as.numeric(cooc_sub[cbind(sub_hi, sub_lo)])
    d_hi  <- doc_freq[top_idx[pairs[1L, ]]]   # D(w_hi): more probable term
    d_lo  <- doc_freq[top_idx[pairs[2L, ]]]   # D(w_lo): less probable term

    # UMass: mean over pairs of log( (D(w_lo, w_hi) + ε) / (D(w_hi) + ε) )
    # The denominator uses w_hi (higher-ranked) as in Mimno et al. (2011).
    umass_per_topic[t] <- mean(log((d_hij + epsilon) / (d_hi + epsilon)))

    # NPMI: mean over pairs of PMI(w_i,w_j) / (-log P(w_i,w_j)), clamped to [-1, 1]
    log_p_hij <- log(d_hij / D_total + epsilon)
    log_p_hi  <- log(d_hi  / D_total + epsilon)
    log_p_lo  <- log(d_lo  / D_total + epsilon)

    npmi_num   <- log_p_hij - log_p_hi - log_p_lo
    npmi_denom <- -log_p_hij
    # Clamp denominator away from zero; when terms always co-occur NPMI -> 1
    npmi_vals  <- ifelse(abs(npmi_denom) < epsilon, 1, npmi_num / npmi_denom)
    npmi_vals  <- pmax(-1, pmin(1, npmi_vals))

    npmi_per_topic[t] <- mean(npmi_vals)
  }

  list(umass = umass_per_topic, npmi = npmi_per_topic)
}
