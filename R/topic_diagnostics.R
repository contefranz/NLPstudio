if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      "aggregate_stability", "candidate_band", "diversity", "doc_id", "exclusivity",
      "k", "level", "matched_topic_id", "metric", "probability",
      "representative_doc_ids", "representative_documents", "representative_text",
      "run_id", "run_stability", "seed",
      "similarity", "supported", "term", "topic", "topic_id",
      "topic_max_id", "topic_rank", "topic_stability", "value"
    )
  )
}

#' Assess Topic Stability Across Repeated Fits
#'
#' Repeatedly fit the same topic-model specification across multiple seeds and
#' compare the resulting topics after label matching. The function is a
#' transparent wrapper around [fit_topic_model()]: unless `resampling` is
#' supplied, each run uses the same data, backend, model, topic count, method,
#' controls, and additional arguments, changing only the random seed.
#'
#' Topic labels are arbitrary across model runs. `assess_topic_stability()`
#' therefore extracts standardized topic-word weights with [get_tww()], aligns
#' vocabularies, matches topics from each run to the first run, and reports
#' matched-topic cosine similarities.
#'
#' @param x Either a document-feature input accepted by [fit_topic_model()], or
#'   a list of pre-fitted `nlp_topic_fit` objects.
#' @param engine Backend package forwarded to [fit_topic_model()] when `x` is
#'   model input.
#' @param model Model family forwarded to [fit_topic_model()] when `x` is model
#'   input.
#' @param k Number of topics forwarded to [fit_topic_model()] when `x` is model
#'   input.
#' @param seeds Integer vector of seeds. In repeated-fit mode this is required
#'   and must contain at least two unique integer seeds. In list-of-fits mode it
#'   is optional and, when supplied, must match the number of fits.
#' @param method Fitting method forwarded to [fit_topic_model()].
#' @param control Backend controls forwarded to [fit_topic_model()]. For
#'   `engine = "topicmodels"`, each seed is also written to
#'   `control$fit$seed` before fitting so backend-native seeding is explicit.
#' @param resampling Optional list with `fraction`, a number in `(0, 1]`. When
#'   supplied, each seed also draws that fraction of documents without
#'   replacement before fitting. Defaults to `NULL`, which means no resampling.
#' @param ncores Integer. Number of PSOCK workers for repeated fitting.
#'   Defaults to `1L`.
#' @param return_fits Logical. Should fitted models be attached as
#'   `attr(result, "fits")`? Defaults to `FALSE`.
#' @param ... Additional arguments forwarded to [fit_topic_model()] in
#'   repeated-fit mode.
#'
#' @returns An object of class `c("nlp_topic_stability", "data.table")` with one
#'   row per reference-topic/run comparison. Topics from all non-reference runs
#'   are matched to the first run. Columns include run metadata, matched topic
#'   IDs, cosine similarity, per-topic stability, per-run stability, aggregate
#'   stability, and model metadata.
#'
#' @examplesIf requireNamespace("topicmodels", quietly = TRUE)
#' dtm <- methods::as(
#'   Matrix::Matrix(
#'     matrix(c(2, 1, 0, 0,  1, 1, 1, 0,  0, 1, 2, 1,
#'              0, 0, 1, 2,  1, 0, 1, 1,  1, 2, 0, 1),
#'            nrow = 6, byrow = TRUE),
#'     sparse = TRUE
#'   ),
#'   "dgCMatrix"
#' )
#' rownames(dtm) <- paste0("doc", 1:6)
#' colnames(dtm) <- paste0("term", 1:4)
#'
#' assess_topic_stability(
#'   dtm,
#'   engine = "topicmodels",
#'   model = "lda",
#'   k = 2,
#'   method = "Gibbs",
#'   seeds = 1:2,
#'   control = list(fit = list(iter = 50, burnin = 0, thin = 1))
#' )
#'
#' fits <- lapply(1:2, function(s) {
#'   fit_topic_model(
#'     dtm,
#'     engine = "topicmodels",
#'     model = "lda",
#'     k = 2,
#'     method = "Gibbs",
#'     control = list(fit = list(seed = s, iter = 50, burnin = 0, thin = 1))
#'   )
#' })
#' assess_topic_stability(fits, seeds = 1:2)
#'
#' @export
assess_topic_stability <- function(x, engine = NULL, model = NULL, k = NULL,
                                   seeds = NULL, method = NULL,
                                   control = list(), resampling = NULL,
                                   ncores = 1L, return_fits = FALSE, ...) {
  list_mode <- .is_topic_fit_list(x)

  if (!is.logical(return_fits) || length(return_fits) != 1L || is.na(return_fits)) {
    stop("'return_fits' must be a single TRUE/FALSE value.", call. = FALSE)
  }

  if (list_mode) {
    fits <- x
    seeds <- .validate_stability_fit_list_seeds(seeds, length(fits), fits)
  } else {
    seeds <- .validate_stability_seeds(seeds, required = TRUE)
    .validate_parallel_args(ncores, nchunks = length(seeds))
    control <- .normalize_topic_control(control)
    resampling <- .validate_stability_resampling(resampling)
    fits <- .fit_topic_seed_grid(
      x = x,
      engine = engine,
      model = model,
      k = k,
      method = method,
      control = control,
      seeds = seeds,
      resampling = resampling,
      ncores = as.integer(ncores),
      ...
    )
  }

  out <- .topic_stability_from_fits(fits, seeds = seeds)
  data.table::setattr(out, "class", c("nlp_topic_stability", "data.table", "data.frame"))

  if (return_fits) {
    data.table::setattr(out, "fits", fits)
  }

  out[]
}

#' Summarize Topics for Interpretation
#'
#' Build a compact one-row-per-topic interpretation table from an
#' `nlp_topic_fit`. The table combines top terms, prevalence, available
#' evaluation metrics, and representative documents.
#'
#' @param fit An `nlp_topic_fit` object returned by [fit_topic_model()].
#' @param training Optional training document-feature matrix. When supplied,
#'   coherence metrics are included.
#' @param doc_data Optional document metadata or text source forwarded to
#'   [get_dtw()] and [get_representative_candidates()].
#' @param top_n Integer. Number of top terms per topic. Defaults to `10L`.
#' @param representative_n Integer. Number of representative documents to
#'   retain per topic. Defaults to `3L`.
#' @param include_text Logical. Should representative text be included when
#'   available? Defaults to `FALSE`.
#' @param docvars Logical. Should stored document variables be available for
#'   representative selection output? Defaults to `FALSE`.
#' @param doc_id_col Document-ID column name when `doc_data` is tabular.
#'   Defaults to `"doc_id"`.
#' @param text_col Text column name when `doc_data` is tabular. Defaults to
#'   `"text"`.
#'
#' @returns A [data.table][data.table::data.table] with one row per topic.
#'
#' @examplesIf requireNamespace("topicmodels", quietly = TRUE)
#' dtm <- methods::as(
#'   Matrix::Matrix(
#'     matrix(c(2, 1, 0, 0,  1, 1, 1, 0,  0, 1, 2, 1,
#'              0, 0, 1, 2,  1, 0, 1, 1,  1, 2, 0, 1),
#'            nrow = 6, byrow = TRUE),
#'     sparse = TRUE
#'   ),
#'   "dgCMatrix"
#' )
#' rownames(dtm) <- paste0("doc", 1:6)
#' colnames(dtm) <- paste0("term", 1:4)
#' fit <- fit_topic_model(
#'   dtm,
#'   engine = "topicmodels",
#'   model = "lda",
#'   k = 2,
#'   method = "Gibbs",
#'   control = list(fit = list(seed = 1, iter = 50, burnin = 0, thin = 1))
#' )
#' summarize_topics(fit, training = dtm, top_n = 3)
#'
#' @export
summarize_topics <- function(fit, training = NULL, doc_data = NULL, top_n = 10L,
                             representative_n = 3L, include_text = FALSE,
                             docvars = FALSE, doc_id_col = "doc_id",
                             text_col = "text") {
  if (!inherits(fit, "nlp_topic_fit")) {
    stop("'fit' must be an nlp_topic_fit object returned by fit_topic_model().",
         call. = FALSE)
  }
  top_n <- .validate_positive_integer(top_n, "top_n")
  representative_n <- .validate_nonnegative_integer(representative_n, "representative_n")
  if (!is.logical(include_text) || length(include_text) != 1L || is.na(include_text)) {
    stop("'include_text' must be a single TRUE/FALSE value.", call. = FALSE)
  }
  if (!is.logical(docvars) || length(docvars) != 1L || is.na(docvars)) {
    stop("'docvars' must be a single TRUE/FALSE value.", call. = FALSE)
  }

  tww <- get_tww(fit)
  topic_ids <- tww$topic_id

  out <- data.table::data.table(
    topic_id = topic_ids,
    topic_int = as.integer(sub("^Topic", "", topic_ids))
  )

  out <- .add_topic_top_terms(out, fit, top_n)
  out <- .add_topic_prevalence(out, fit, doc_data, docvars, include_text,
                               doc_id_col, text_col)
  out <- .add_topic_metric_columns(out, fit, training, top_n)
  out <- .add_representative_documents(
    out = out,
    fit = fit,
    doc_data = doc_data,
    docvars = docvars,
    include_text = include_text,
    doc_id_col = doc_id_col,
    text_col = text_col,
    representative_n = representative_n
  )

  out[]
}

#' Print Topic Stability Results
#'
#' @param x An `nlp_topic_stability` object.
#' @param ... Unused.
#' @returns Invisibly returns `x`.
#' @export
print.nlp_topic_stability <- function(x, ...) {
  cat("<nlp_topic_stability>\n")
  if (!nrow(x)) {
    cat("  No stability comparisons.\n")
    return(invisible(x))
  }
  cat(sprintf("  K: %s\n", paste(unique(x$k), collapse = ", ")))
  cat(sprintf("  runs compared: %d\n", length(unique(x$run_id))))
  cat(sprintf("  aggregate stability: %.4f\n", unique(x$aggregate_stability)[1L]))
  invisible(x)
}

.is_topic_fit_list <- function(x) {
  is.list(x) &&
    !inherits(x, "nlp_topic_fit") &&
    length(x) >= 2L &&
    all(vapply(x, inherits, logical(1L), "nlp_topic_fit"))
}

.validate_stability_seeds <- function(seeds, required) {
  if (is.null(seeds)) {
    if (required) {
      stop("'seeds' must contain at least two integer seeds.", call. = FALSE)
    }
    return(NULL)
  }
  if (!is.numeric(seeds) || length(seeds) < 2L || anyNA(seeds) ||
      any(!is.finite(seeds)) || any(seeds != as.integer(seeds))) {
    stop("'seeds' must contain at least two integer seeds.", call. = FALSE)
  }
  seeds <- as.integer(seeds)
  if (anyDuplicated(seeds)) {
    stop("'seeds' must be unique.", call. = FALSE)
  }
  seeds
}

.validate_stability_fit_list_seeds <- function(seeds, n_fits, fits) {
  if (is.null(seeds)) {
    return(vapply(fits, .infer_fit_seed, integer(1L)))
  }
  if (!is.numeric(seeds) || length(seeds) != n_fits || anyNA(seeds) ||
      any(!is.finite(seeds)) || any(seeds != as.integer(seeds))) {
    stop("'seeds' must be NULL or an integer vector with one seed per fit.",
         call. = FALSE)
  }
  as.integer(seeds)
}

.infer_fit_seed <- function(fit) {
  seed <- fit$backend_control$fit$seed
  if (is.null(seed) || length(seed) != 1L || is.na(seed) ||
      !is.finite(seed) || seed != as.integer(seed)) {
    return(NA_integer_)
  }
  as.integer(seed)
}

.validate_stability_resampling <- function(resampling) {
  if (is.null(resampling)) {
    return(NULL)
  }
  if (!is.list(resampling) || !"fraction" %in% names(resampling)) {
    stop("'resampling' must be NULL or a list containing 'fraction'.",
         call. = FALSE)
  }
  fraction <- resampling$fraction
  if (!is.numeric(fraction) || length(fraction) != 1L || is.na(fraction) ||
      !is.finite(fraction) || fraction <= 0 || fraction > 1) {
    stop("'resampling$fraction' must be a single number in (0, 1].",
         call. = FALSE)
  }
  extra <- setdiff(names(resampling), "fraction")
  if (length(extra)) {
    stop(sprintf(
      "Unknown resampling entries: %s.",
      paste(extra, collapse = ", ")
    ), call. = FALSE)
  }
  list(fraction = fraction)
}

.fit_topic_seed_grid <- function(x, engine, model, k, method, control, seeds,
                                 resampling, ncores, ...) {
  if (is.null(engine) || is.null(model) || is.null(k)) {
    stop("'engine', 'model', and 'k' must be supplied in repeated-fit mode.",
         call. = FALSE)
  }
  extra_args <- list(...)

  worker <- function(seed) {
    x_run <- .stability_resample_input(x, resampling, seed)
    control_run <- .stability_control_for_seed(control, engine, seed)
    if (identical(engine, "topicmodels.etm") && .has_namespace("torch")) {
      .get_exported_value("torch", "torch_manual_seed")(seed)
    }
    set.seed(seed)
    do.call(
      fit_topic_model,
      c(
        list(
          x = x_run,
          engine = engine,
          model = model,
          k = k,
          method = method,
          control = control_run
        ),
        extra_args
      )
    )
  }

  if (ncores <= 1L) {
    return(lapply(seeds, worker))
  }

  cl <- tryCatch(
    parallel::makeCluster(ncores),
    error = function(e) {
      warning(sprintf(
        "PSOCK cluster could not be created; falling back to sequential: %s",
        conditionMessage(e)
      ), call. = FALSE)
      NULL
    }
  )
  if (is.null(cl)) {
    return(lapply(seeds, worker))
  }
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterEvalQ(cl, library(NLPstudio, quietly = TRUE))
  parallel::clusterExport(
    cl,
    varlist = c(
      "x", "engine", "model", "k", "method", "control", "resampling",
      "extra_args"
    ),
    envir = environment()
  )
  parallel::clusterApplyLB(cl, seeds, worker)
}

.stability_control_for_seed <- function(control, engine, seed) {
  control <- .normalize_topic_control(control)
  if (identical(engine, "topicmodels")) {
    control$fit$seed <- seed
  }
  control
}

.stability_resample_input <- function(x, resampling, seed) {
  if (is.null(resampling)) {
    return(x)
  }
  mat <- .as_topic_dgCMatrix(x)
  n_docs <- nrow(mat)
  n_sample <- max(1L, as.integer(round(n_docs * resampling$fraction)))
  n_sample <- min(n_sample, n_docs)
  set.seed(seed)
  idx <- sort(sample.int(n_docs, size = n_sample, replace = FALSE))
  if (inherits(x, "dfm") || methods::is(x, "DocumentTermMatrix")) {
    return(x[idx, ])
  }
  mat[idx, , drop = FALSE]
}

.topic_stability_from_fits <- function(fits, seeds) {
  tww_mats <- lapply(fits, .stability_tww_matrix)
  meta <- .stability_fit_metadata(fits, tww_mats)
  aligned <- .align_stability_tww_matrices(tww_mats)

  reference <- aligned[[1L]]
  reference_topics <- rownames(reference)
  rows <- vector("list", max(0L, length(aligned) - 1L))

  for (i in seq_along(aligned)[-1L]) {
    sim <- .topic_cosine_similarity(reference, aligned[[i]])
    assignment <- .optimal_topic_assignment(sim)
    rows[[i - 1L]] <- data.table::data.table(
      run_id = i,
      seed = seeds[i],
      reference_run_id = 1L,
      reference_seed = seeds[1L],
      topic_id = reference_topics,
      matched_topic_id = colnames(sim)[assignment],
      similarity = sim[cbind(seq_along(assignment), assignment)]
    )
  }

  out <- data.table::rbindlist(rows)
  out[, topic_stability := mean(similarity), by = topic_id]
  out[, run_stability := mean(similarity), by = run_id]
  out[, aggregate_stability := mean(similarity)]
  out[, `:=`(
    k = meta$k,
    engine = meta$engine,
    model = meta$model,
    method = meta$method
  )]
  data.table::setcolorder(
    out,
    c(
      "run_id", "seed", "reference_run_id", "reference_seed",
      "topic_id", "matched_topic_id", "similarity",
      "topic_stability", "run_stability", "aggregate_stability",
      "k", "engine", "model", "method"
    )
  )
  data.table::setorder(out, run_id, topic_id)
  out[]
}

.stability_tww_matrix <- function(fit) {
  tww <- get_tww(fit)
  term_cols <- setdiff(names(tww), "topic_id")
  mat <- as.matrix(tww[, term_cols, with = FALSE])
  storage.mode(mat) <- "double"
  rownames(mat) <- tww$topic_id
  colnames(mat) <- term_cols
  mat
}

.stability_fit_metadata <- function(fits, tww_mats) {
  k_vals <- vapply(tww_mats, nrow, integer(1L))
  if (length(unique(k_vals)) != 1L) {
    stop("All fitted models must have the same number of topics.", call. = FALSE)
  }

  engines <- vapply(fits, function(x) .null_to_na_character(x$engine), character(1L))
  models <- vapply(fits, function(x) .null_to_na_character(x$model), character(1L))
  methods <- vapply(fits, function(x) .null_to_na_character(x$method), character(1L))

  if (length(unique(engines)) != 1L ||
      length(unique(models)) != 1L ||
      length(unique(methods)) != 1L) {
    stop("All fitted models must share the same engine, model, method, and K.",
         call. = FALSE)
  }

  list(k = k_vals[1L], engine = engines[1L], model = models[1L], method = methods[1L])
}

.null_to_na_character <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }
  as.character(x)[1L]
}

.align_stability_tww_matrices <- function(mats) {
  vocab <- sort(unique(unlist(lapply(mats, colnames), use.names = FALSE)))
  lapply(mats, function(mat) {
    out <- matrix(
      0,
      nrow = nrow(mat),
      ncol = length(vocab),
      dimnames = list(rownames(mat), vocab)
    )
    out[, colnames(mat)] <- mat
    .l2_normalize_rows(out)
  })
}

.l2_normalize_rows <- function(mat) {
  denom <- sqrt(rowSums(mat * mat))
  denom[!is.finite(denom) | denom <= 0] <- 1
  sweep(mat, 1L, denom, "/")
}

.topic_cosine_similarity <- function(reference, candidate) {
  sim <- reference %*% t(candidate)
  sim <- as.matrix(sim)
  rownames(sim) <- rownames(reference)
  colnames(sim) <- rownames(candidate)
  sim
}

.optimal_topic_assignment <- function(similarity) {
  cost <- max(similarity, na.rm = TRUE) - similarity
  .hungarian_min_assignment(cost)
}

.hungarian_min_assignment <- function(cost) {
  cost <- as.matrix(cost)
  if (nrow(cost) != ncol(cost)) {
    stop("Topic matching requires a square similarity matrix.", call. = FALSE)
  }
  n <- nrow(cost)
  m <- ncol(cost)
  u <- numeric(n)
  v <- numeric(m + 1L)
  p <- integer(m + 1L)
  way <- integer(m + 1L)

  for (i in seq_len(n)) {
    p[1L] <- i
    j0 <- 1L
    minv <- rep(Inf, m + 1L)
    used <- rep(FALSE, m + 1L)

    repeat {
      used[j0] <- TRUE
      i0 <- p[j0]
      delta <- Inf
      j1 <- 1L

      for (j in 2L:(m + 1L)) {
        if (!used[j]) {
          cur <- cost[i0, j - 1L] - u[i0] - v[j]
          if (cur < minv[j]) {
            minv[j] <- cur
            way[j] <- j0
          }
          if (minv[j] < delta) {
            delta <- minv[j]
            j1 <- j
          }
        }
      }

      for (j in seq_len(m + 1L)) {
        if (used[j]) {
          if (p[j] > 0L) {
            u[p[j]] <- u[p[j]] + delta
          }
          v[j] <- v[j] - delta
        } else {
          minv[j] <- minv[j] - delta
        }
      }

      j0 <- j1
      if (p[j0] == 0L) {
        break
      }
    }

    repeat {
      j1 <- way[j0]
      p[j0] <- p[j1]
      j0 <- j1
      if (j0 == 1L) {
        break
      }
    }
  }

  assignment <- integer(n)
  for (j in 2L:(m + 1L)) {
    if (p[j] > 0L) {
      assignment[p[j]] <- j - 1L
    }
  }
  assignment
}

.validate_positive_integer <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) ||
      !is.finite(x) || x < 1L || x != as.integer(x)) {
    stop(sprintf("'%s' must be a single positive integer.", name),
         call. = FALSE)
  }
  as.integer(x)
}

.validate_nonnegative_integer <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) ||
      !is.finite(x) || x < 0L || x != as.integer(x)) {
    stop(sprintf("'%s' must be a single non-negative integer.", name),
         call. = FALSE)
  }
  as.integer(x)
}

.add_topic_top_terms <- function(out, fit, top_n) {
  top_terms <- get_top_terms(fit, n = top_n, format = "long")
  collapsed <- top_terms[
    ,
    .(
      top_terms = paste(term, collapse = ", "),
      top_term_probabilities = paste(signif(probability, 6L), collapse = ", ")
    ),
    by = .(topic_id = topic)
  ]
  merge(out, collapsed, by = "topic_id", all.x = TRUE, sort = FALSE)
}

.add_topic_prevalence <- function(out, fit, doc_data, docvars, include_text,
                                  doc_id_col, text_col) {
  dtw <- get_dtw(
    fit,
    doc_data = doc_data,
    docvars = docvars,
    include_text = include_text,
    doc_id_col = doc_id_col,
    text_col = text_col
  )
  topic_cols <- .find_topic_columns(dtw, id_col = "doc_id")
  prevalence <- data.table::data.table(
    topic_id = topic_cols,
    prevalence = as.numeric(colMeans(as.matrix(dtw[, topic_cols, with = FALSE])))
  )
  merge(out, prevalence, by = "topic_id", all.x = TRUE, sort = FALSE)
}

.add_topic_metric_columns <- function(out, fit, training, top_n) {
  metrics <- if (is.null(training)) {
    c("diversity", "exclusivity")
  } else {
    c("coherence_npmi", "coherence_umass", "diversity", "exclusivity")
  }

  eval <- suppressWarnings(evaluate_topic_model(
    fit,
    training = training,
    metrics = metrics,
    top_n = top_n,
    level = "all"
  ))

  for (metric_name in c("coherence_npmi", "coherence_umass", "diversity", "exclusivity")) {
    out[, (metric_name) := NA_real_]
  }

  topic_eval <- eval[level == "topic" & supported, .(topic_id, metric, value)]
  if (nrow(topic_eval)) {
    wide <- data.table::dcast(topic_eval, topic_id ~ metric, value.var = "value")
    metric_cols <- setdiff(names(wide), "topic_id")
    out <- merge(out, wide, by = "topic_id", all.x = TRUE, sort = FALSE,
                 suffixes = c("", ".metric"))
    for (metric_name in metric_cols) {
      metric_col <- paste0(metric_name, ".metric")
      if (metric_col %in% names(out)) {
        out[, (metric_name) := get(metric_col)]
        out[, (metric_col) := NULL]
      }
    }
  }

  aggregate_eval <- eval[level == "aggregate" & supported, .(metric, value)]
  if ("diversity" %in% aggregate_eval$metric) {
    out[, diversity := aggregate_eval[metric == "diversity", value][1L]]
  }
  out[]
}

.add_representative_documents <- function(out, fit, doc_data, docvars,
                                          include_text, doc_id_col, text_col,
                                          representative_n) {
  if (representative_n == 0L) {
    out[, representative_doc_ids := NA_character_]
    out[, representative_documents := replicate(.N, data.table::data.table(), simplify = FALSE)]
    if (include_text) {
      out[, representative_text := NA_character_]
    }
    return(out[])
  }

  reps <- get_representative_candidates(
    fit,
    doc_data = doc_data,
    docvars = docvars,
    include_text = include_text,
    doc_id_col = doc_id_col,
    text_col = text_col
  )
  if (!nrow(reps)) {
    out[, representative_doc_ids := NA_character_]
    out[, representative_documents := replicate(.N, data.table::data.table(), simplify = FALSE)]
    if (include_text) {
      out[, representative_text := NA_character_]
    }
    return(out[])
  }

  reps <- reps[order(topic_max_id, topic_rank)]
  reps <- reps[, utils::head(.SD, representative_n), by = topic_max_id]
  collapsed <- reps[
    ,
    .(representative_doc_ids = paste(doc_id, collapse = ", ")),
    by = .(topic_id = topic_max_id)
  ]
  doc_cols <- setdiff(
    names(reps),
    c(.representative_candidates_output_columns(reps), "topic_max_id")
  )
  doc_tables <- reps[
    ,
    .(representative_documents = list(data.table::copy(.SD))),
    by = .(topic_id = topic_max_id),
    .SDcols = doc_cols
  ]
  collapsed <- merge(collapsed, doc_tables, by = "topic_id", all = TRUE, sort = FALSE)
  if (include_text && "text" %in% names(reps)) {
    text_collapsed <- reps[
      ,
      .(representative_text = paste(text, collapse = " || ")),
      by = .(topic_id = topic_max_id)
    ]
    collapsed <- merge(collapsed, text_collapsed, by = "topic_id", all = TRUE, sort = FALSE)
  }
  merge(out, collapsed, by = "topic_id", all.x = TRUE, sort = FALSE)
}
