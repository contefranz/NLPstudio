if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("metric", "scope", "supported", "value"))
}

#' Evaluate a Fitted Topic Model
#'
#' Compute a standardized set of quality metrics for an object returned by
#' [fit_topic_model()]. Metrics are returned in a single long-format
#' [data.table][data.table::data.table] so that results from different
#' engines and different values of k can be compared directly.
#'
#' @param fit An object of class `nlp_topic_fit` returned by [fit_topic_model()].
#' @param training The document-feature matrix used to train `fit`. Required for
#'   coherence metrics (`"coherence_npmi"`, `"coherence_umass"`). Accepted
#'   classes are [dgCMatrix-class][Matrix::dgCMatrix-class],
#'   [dfm][quanteda::dfm], and `DocumentTermMatrix`. Defaults to `NULL`.
#' @param newdata A held-out document-feature matrix. Required for predictive
#'   metrics (`"held_out_nll"`, `"perplexity"`). Accepted classes are the same
#'   as `training`. Defaults to `NULL`.
#' @param metrics Character vector of metrics to compute. Defaults to all six
#'   supported metrics (alphabetical):
#'   \describe{
#'     \item{`"coherence_npmi"`}{Normalized Pointwise Mutual Information
#'       coherence per topic (Aletras & Stevenson, 2013). Requires `training`.}
#'     \item{`"coherence_umass"`}{UMass coherence per topic
#'       (Mimno et al., 2011). Requires `training`.}
#'     \item{`"diversity"`}{Proportion of unique top-N terms across all topics.
#'       Engine-agnostic; no extra data required.}
#'     \item{`"exclusivity"`}{STM-style per-topic exclusivity: mean share of
#'       each top-N term's probability belonging to that topic. Engine-agnostic;
#'       no extra data required.}
#'     \item{`"held_out_nll"`}{Mean negative log-likelihood per token on
#'       `newdata`. Requires `newdata`.}
#'     \item{`"perplexity"`}{Held-out perplexity. Equal to
#'       `exp(held_out_nll)`. Requires `newdata`.}
#'   }
#' @param top_n Integer. Number of top terms per topic used by coherence,
#'   diversity, and exclusivity. Defaults to `10L`.
#' @param epsilon Small positive constant for numerical stability in logarithm
#'   computations. Defaults to `1e-12`.
#'
#' @returns A [data.table][data.table::data.table] with columns:
#'   \describe{
#'     \item{`metric`}{Metric name (one of the values in `metrics`).}
#'     \item{`scope`}{`"overall"` for corpus-level scalars, `"per_topic"` for
#'       topic-level values.}
#'     \item{`topic_id`}{`Topic###` identifier for `"per_topic"` rows;
#'       `NA` for `"overall"` rows.}
#'     \item{`value`}{Numeric metric value. `NA` when `supported = FALSE`.}
#'     \item{`supported`}{`TRUE` when the metric was computed; `FALSE` when
#'       the required data is missing or the metric is unsupported for the
#'       given engine.}
#'   }
#'   Rows are ordered by `metric` then `scope` then `topic_id`.
#'
#' @details
#' **Coherence** metrics require `training` to be the same corpus used to fit
#' the model. They are computed in-package using sparse co-occurrence statistics
#' from `training`, so results are directly comparable across all supported
#' engines. Terms in `fit$vocab` that are absent from `training` contribute
#' zero to all co-occurrence counts.
#'
#' **Diversity** is the proportion of unique terms among all available
#' top-`top_n` terms across topics:
#' `length(unique_top_terms) / (k * min(top_n, vocabulary_size))`. A value of
#' 1 means no term appears in more than one topic's top list; a low value
#' indicates topics that share high-probability terms.
#'
#' **Exclusivity** (per topic `t`) is the mean, over the top-`top_n` terms of
#' `t`, of `phi[t, w] / sum_j phi[j, w]`. High exclusivity means those terms
#' are concentrated in that topic rather than spread across topics.
#'
#' **Perplexity** and **held-out NLL** align `newdata` to the fitted vocabulary,
#' obtain document-topic weights, then combine those weights with `fit$tww` to
#' reconstruct per-token log-likelihoods. Documents in `newdata` whose terms
#' are all outside the fitted vocabulary are dropped with a warning (they carry
#' no information under the fitted model). Tokens outside the fitted vocabulary
#' are excluded from the token count, matching the convention used by
#' `topicmodels::perplexity()`.
#'
#' @references
#' Aletras, N., & Stevenson, M. (2013). Evaluating topic coherence using
#' distributional semantics. *EACL*, 13-22.
#'
#' Mimno, D., Wallach, H., Talley, E., Leenders, M., & McCallum, A. (2011).
#' Optimizing semantic coherence in topic models. *EMNLP*, 262-272.
#'
#' Roberts, M. E., Stewart, B. M., Tingley, D., Lucas, C., Leder-Luis, J.,
#' Gadarian, S. K., Albertson, B., & Rand, D. G. (2014). Structural topic
#' models for open-ended survey responses. *American Journal of Political
#' Science*, 58(4), 1064-1082.
#'
#' @seealso [fit_topic_model()], [predict_topic_model()], [select_k_topics()]
#'
#' @examplesIf requireNamespace("text2vec", quietly = TRUE)
#' dtm <- methods::as(
#'   Matrix::Matrix(
#'     matrix(c(2, 1, 0, 0,  1, 1, 1, 0,  0, 1, 2, 1,  0, 0, 1, 2),
#'            nrow = 4, byrow = TRUE),
#'     sparse = TRUE
#'   ),
#'   "dgCMatrix"
#' )
#' rownames(dtm) <- paste0("doc", 1:4)
#' colnames(dtm) <- paste0("term", 1:4)
#'
#' fit <- fit_topic_model(
#'   dtm, engine = "text2vec", model = "lda", k = 2,
#'   control = list(fit = list(n_iter = 25, progressbar = FALSE))
#' )
#'
#' # Engine-agnostic metrics only (no extra data needed)
#' evaluate_topic_model(fit, metrics = c("diversity", "exclusivity"))
#'
#' # Coherence with training data
#' evaluate_topic_model(fit, training = dtm,
#'                      metrics = c("coherence_npmi", "coherence_umass"))
#'
#' @export
evaluate_topic_model <- function(
  fit,
  training = NULL,
  newdata  = NULL,
  metrics  = c("coherence_npmi", "coherence_umass",
               "diversity", "exclusivity",
               "held_out_nll", "perplexity"),
  top_n   = 10L,
  epsilon = 1e-12
) {
  if (!inherits(fit, "nlp_topic_fit")) {
    stop("'fit' must be an nlp_topic_fit object returned by fit_topic_model().",
         call. = FALSE)
  }
  if (!is.numeric(top_n) || length(top_n) != 1L || is.na(top_n) ||
      !is.finite(top_n) || top_n < 1L ||
      top_n != as.integer(top_n)) {
    stop("'top_n' must be a single positive integer.", call. = FALSE)
  }
  if (!is.numeric(epsilon) || length(epsilon) != 1L || is.na(epsilon) ||
      !is.finite(epsilon) || epsilon <= 0) {
    stop("'epsilon' must be a single positive number.", call. = FALSE)
  }

  metrics <- .validate_topic_eval_metrics(metrics)

  top_n <- as.integer(top_n)

  # Warn early when required data is absent for requested metrics
  needs_training <- intersect(metrics, c("coherence_npmi", "coherence_umass"))
  needs_newdata  <- intersect(metrics, c("held_out_nll", "perplexity"))

  if (length(needs_training) && is.null(training)) {
    warning(sprintf(
      "Metrics %s require 'training' but none was supplied; they will be marked unsupported.",
      paste(needs_training, collapse = ", ")
    ), call. = FALSE)
  }
  if (length(needs_newdata) && is.null(newdata)) {
    warning(sprintf(
      "Metrics %s require 'newdata' but none was supplied; they will be marked unsupported.",
      paste(needs_newdata, collapse = ", ")
    ), call. = FALSE)
  }

  results <- vector("list", length(metrics))
  names(results) <- metrics

  # ---- Coherence ----
  coherence_metrics <- intersect(metrics, c("coherence_npmi", "coherence_umass"))
  if (length(coherence_metrics)) {
    if (is.null(training)) {
      for (m in coherence_metrics) {
        results[[m]] <- .eval_unsupported(m, topic_ids = .eval_topic_ids(fit))
      }
    } else {
      tww_mat <- .eval_tww_matrix(fit)
      coh     <- .compute_coherence(tww_mat, training, top_n, epsilon)
      tids    <- rownames(tww_mat)
      if ("coherence_npmi" %in% coherence_metrics) {
        results[["coherence_npmi"]] <- .eval_per_topic_with_overall(
          "coherence_npmi", tids, coh$npmi
        )
      }
      if ("coherence_umass" %in% coherence_metrics) {
        results[["coherence_umass"]] <- .eval_per_topic_with_overall(
          "coherence_umass", tids, coh$umass
        )
      }
    }
  }

  # Diversity
  if ("diversity" %in% metrics) {
    results[["diversity"]] <- .metric_diversity(fit, top_n)
  }

  # Exclusivity
  if ("exclusivity" %in% metrics) {
    results[["exclusivity"]] <- .metric_exclusivity(fit, top_n)
  }

  # Held-out NLL + Perplexity (computed together)
  pnll_metrics <- intersect(metrics, c("held_out_nll", "perplexity"))
  if (length(pnll_metrics)) {
    if (is.null(newdata)) {
      for (m in pnll_metrics) {
        results[[m]] <- .eval_unsupported_overall(m)
      }
    } else {
      pnll <- .metric_perplexity_nll(fit, newdata, epsilon, pnll_metrics)
      for (m in pnll_metrics) {
        results[[m]] <- pnll[[m]]
      }
    }
  }

  out <- data.table::rbindlist(results[metrics])
  data.table::setorder(out, metric, scope, topic_id)
  out[]
}


#' @keywords internal
.topic_eval_metrics <- function() {
  c("coherence_npmi", "coherence_umass",
    "diversity", "exclusivity",
    "held_out_nll", "perplexity")
}

#' @keywords internal
.validate_topic_eval_metrics <- function(metrics) {
  valid_metrics <- .topic_eval_metrics()

  if (!is.character(metrics) || length(metrics) == 0L ||
      anyNA(metrics) || any(!nzchar(metrics))) {
    stop("'metrics' must be a non-empty character vector of valid metric names.",
         call. = FALSE)
  }

  metrics <- unique(metrics)
  bad <- setdiff(metrics, valid_metrics)
  if (length(bad)) {
    stop(sprintf(
      "Unknown metric(s): %s. Valid choices are: %s.",
      paste(bad, collapse = ", "),
      paste(valid_metrics, collapse = ", ")
    ), call. = FALSE)
  }

  metrics
}

#' @keywords internal
.eval_tww_matrix <- function(fit) {
  if (!is.null(fit$tww)) return(fit$tww)
  tww_dt <- get_tww(fit)
  mat <- as.matrix(tww_dt[, -1L])
  rownames(mat) <- tww_dt$topic_id
  mat
}

#' @keywords internal
.eval_topic_ids <- function(fit) {
  if (!is.null(fit$tww)) return(rownames(fit$tww))
  if (!is.null(fit$dtw)) return(colnames(fit$dtw))
  character(0L)
}

#' @keywords internal
.eval_per_topic_with_overall <- function(metric_name, topic_ids, values) {
  per_topic <- data.table::data.table(
    metric    = metric_name,
    scope     = "per_topic",
    topic_id  = topic_ids,
    value     = values,
    supported = TRUE
  )
  overall <- data.table::data.table(
    metric    = metric_name,
    scope     = "overall",
    topic_id  = NA_character_,
    value     = mean(values, na.rm = TRUE),
    supported = TRUE
  )
  data.table::rbindlist(list(per_topic, overall))
}

#' @keywords internal
.eval_unsupported <- function(metric_name, topic_ids = character(0L)) {
  rows <- list()
  if (length(topic_ids)) {
    rows[[1L]] <- data.table::data.table(
      metric    = metric_name,
      scope     = "per_topic",
      topic_id  = topic_ids,
      value     = NA_real_,
      supported = FALSE
    )
  }
  rows[[length(rows) + 1L]] <- data.table::data.table(
    metric    = metric_name,
    scope     = "overall",
    topic_id  = NA_character_,
    value     = NA_real_,
    supported = FALSE
  )
  data.table::rbindlist(rows)
}

#' @keywords internal
.eval_unsupported_overall <- function(metric_name) {
  data.table::data.table(
    metric    = metric_name,
    scope     = "overall",
    topic_id  = NA_character_,
    value     = NA_real_,
    supported = FALSE
  )
}

#' @keywords internal
.metric_diversity <- function(fit, top_n) {
  tww       <- .eval_tww_matrix(fit)
  top_terms <- get_top_terms(fit, n = top_n, format = "long")
  k         <- nrow(tww)
  top_slots <- min(top_n, ncol(tww))
  n_unique  <- data.table::uniqueN(top_terms$term)
  data.table::data.table(
    metric    = "diversity",
    scope     = "overall",
    topic_id  = NA_character_,
    value     = n_unique / (k * top_slots),
    supported = TRUE
  )
}

#' @keywords internal
.metric_exclusivity <- function(fit, top_n) {
  tww <- .eval_tww_matrix(fit)
  k   <- nrow(tww)
  V   <- ncol(tww)

  # Column sums: \sum_j phi[j,w]
  col_sums <- colSums(tww)
  # Guard against all-zero columns (shouldn't happen with fitted models)
  col_sums <- pmax(col_sums, .Machine$double.eps)

  # Exclusivity matrix: phi[t, w] / \sum_j phi[j,w]
  excl_mat <- sweep(tww, 2L, col_sums, "/")

  top_n_eff <- min(top_n, V)
  excl_per_topic <- vapply(seq_len(k), function(t) {
    top_idx <- order(tww[t, ], decreasing = TRUE)[seq_len(top_n_eff)]
    mean(excl_mat[t, top_idx])
  }, numeric(1L))

  .eval_per_topic_with_overall("exclusivity", rownames(tww), excl_per_topic)
}

#' @keywords internal
.metric_perplexity_nll <- function(fit, newdata, epsilon, which_metrics) {
  # phi matrix (K x V)
  phi_mat <- .eval_tww_matrix(fit)  # K x V
  vocab   <- colnames(phi_mat)

  aligned <- .align_topic_input_to_vocab(
    newdata,
    vocab = vocab,
    vocab_label = "fitted vocabulary",
    context = "prediction vocabulary alignment"
  )
  counts_aligned <- aligned$sparse

  theta_mat <- .predict_topic_matrix(
    fit = fit,
    newdata_aligned = aligned,
    control = list()
  )
  theta_mat <- theta_mat[, rownames(phi_mat), drop = FALSE]

  # Compute log-likelihood efficiently using only non-zero token positions
  # Converts to triplet form: (i=doc_row, j=term_col, x=count)
  coo <- Matrix::summary(methods::as(counts_aligned, "TsparseMatrix"))
  if (nrow(coo) == 0L) {
    stop("'newdata' has no tokens within the fitted vocabulary.", call. = FALSE)
  }

  # For each non-zero (d, w): word_prob = sum_t theta[d, t] * phi[t, w]
  # theta_mat rows are already aligned to counts_aligned rows (same doc order)
  theta_at_i <- theta_mat[coo$i, , drop = FALSE]        # NNZ x K
  phi_at_j   <- t(phi_mat)[coo$j, , drop = FALSE]       # NNZ x K
  word_prob   <- rowSums(theta_at_i * phi_at_j)          # NNZ

  log_wp  <- log(word_prob + epsilon)
  contrib <- coo$x * log_wp  # weighted log-likelihood per non-zero

  nll_total    <- -sum(contrib)
  total_tokens <- sum(coo$x)
  nll_per_token <- nll_total / total_tokens

  out <- list()
  if ("held_out_nll" %in% which_metrics) {
    out[["held_out_nll"]] <- data.table::data.table(
      metric    = "held_out_nll",
      scope     = "overall",
      topic_id  = NA_character_,
      value     = nll_per_token,
      supported = TRUE
    )
  }
  if ("perplexity" %in% which_metrics) {
    out[["perplexity"]] <- data.table::data.table(
      metric    = "perplexity",
      scope     = "overall",
      topic_id  = NA_character_,
      value     = exp(nll_per_token),
      supported = TRUE
    )
  }
  out
}
