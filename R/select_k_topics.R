if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("k"))
}

#' Select the Number of Topics by Grid Search
#'
#' Fit a topic model for each value in a grid of topic counts and evaluate each
#' fit with [evaluate_topic_model()]. The result provides the information needed
#' to compare candidate values of \eqn{K} on multiple quality metrics
#' simultaneously.
#'
#' @param x Document-feature input for fitting. Accepted classes are
#'   [dgCMatrix-class][Matrix::dgCMatrix-class], [dfm][quanteda::dfm], and
#'   `DocumentTermMatrix`. A corpus is **not** accepted; convert to a
#'   document-feature matrix first with e.g. [quanteda::dfm()].
#' @param engine Backend package. Forwarded to [fit_topic_model()].
#' @param model Model family. Forwarded to [fit_topic_model()].
#' @param k_grid Integer vector of topic counts \eqn{K} to evaluate. Defaults to
#'   `5:15`.
#' @param metrics Character vector of metrics to compute for each candidate
#'   \eqn{K}. Defaults to all eight metrics supported by
#'   [evaluate_topic_model()].
#' @param level Reporting level forwarded to [evaluate_topic_model()]. One of
#'   `"aggregate"` (default), `"topic"`, or `"all"`.
#' @param control Named list of backend controls forwarded to
#'   [fit_topic_model()] for every candidate \eqn{K}. Defaults to `list()`.
#' @param holdout Fraction of documents held out for `held_out_nll` and
#'   `held_out_perplexity` metrics. Must be in `[0, 1)`. Defaults to `0.2`.
#'   When `holdout > 0`, the remaining fraction is used as `training` for
#'   coherence and training likelihood metrics. When `holdout = 0`, coherence
#'   and training likelihood metrics are computed on the full fitting input and
#'   held-out metrics are marked unsupported because no held-out data is
#'   available.
#'   If none of `"held_out_nll"`, `"held_out_perplexity"`, `"train_nll"`,
#'   `"train_perplexity"`, `"coherence_npmi"`, or `"coherence_umass"` is in
#'   `metrics`, the holdout split is skipped and the full `x` is used for
#'   fitting.
#' @param ncores Number of parallel workers. Defaults to `1L` (sequential).
#'   Each candidate \eqn{K} is fit independently, so parallelization scales
#'   linearly with `length(k_grid)`. Uses `"PSOCK"` sockets; `"FORK"` is not used to
#'   preserve quanteda/C++ stability.
#' @param seed Integer vector of length `length(k_grid)` used to seed each
#'   candidate \eqn{K}'s fit reproducibly. If a single integer is supplied it is
#'   expanded to a length-`length(k_grid)` vector starting from that value.
#'   `NULL` means no seeding. Defaults to `NULL`.
#' @param return_fits Logical. Should the fitted models be returned as an
#'   attribute of the result? Defaults to `FALSE`. Fits can be large; set
#'   `TRUE` only when you need to inspect or reuse them.
#' @param top_n Integer. Forwarded to [evaluate_topic_model()]. Defaults to
#'   `10L`.
#' @param epsilon Numeric. Forwarded to [evaluate_topic_model()]. Defaults to
#'   `1e-12`.
#' @param method Fitting method forwarded to [fit_topic_model()]. Defaults to
#'   `NULL`.
#' @param ... Additional arguments forwarded to [fit_topic_model()].
#'
#' @returns An object of class `c("nlp_k_selection", "data.table")` with
#'   columns:
#'   \describe{
#'     \item{`k`}{Topic count \eqn{K}.}
#'     \item{`metric`}{Metric name.}
#'     \item{`scope`}{`"overall"` or `"per_topic"`.}
#'     \item{`topic_id`}{`Topic###` or `NA` (see [evaluate_topic_model()]).}
#'     \item{`value`}{Numeric metric value.}
#'     \item{`supported`}{Logical; `TRUE` when the metric was computed.}
#'   }
#'   If `return_fits = TRUE` the fitted models are stored in
#'   `attr(result, "fits")`, a named list with names `"k<value>"`.
#'
#' @details
#' **Holdout split.** When `holdout > 0` and either predictive or coherence
#' metrics are requested, `x` is split at the document level into a training
#' shard (`1 - holdout` fraction) and a held-out shard (`holdout` fraction).
#' The split is random but reproducible when `seed` is supplied. The training
#' shard is passed to [fit_topic_model()] and to [evaluate_topic_model()] for
#' coherence and training likelihood metrics; the held-out shard is passed to
#' [evaluate_topic_model()] for held-out metrics. With `holdout = 0`, the full
#' `x` is used for fitting, coherence, and training likelihood metrics, while
#' held-out metrics are reported as unsupported.
#'
#' A warning is issued when the number of documents is fewer than 50, because
#' the holdout shard may be too small for stable predictive metrics.
#'
#' **Parallelisation.** Uses `"PSOCK"` sockets. Each worker receives its own
#' \eqn{K} value and seed and runs the full fit + evaluate cycle independently.
#' The `ncores = 1` path bypasses cluster creation entirely and runs
#' sequentially.
#'
#' @seealso [fit_topic_model()], [evaluate_topic_model()],
#'   [print.nlp_k_selection()], [plot.nlp_k_selection()]
#'
#' @examplesIf requireNamespace("text2vec", quietly = TRUE)
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
#' sel <- select_k_topics(
#'   dtm, engine = "text2vec", model = "lda",
#'   k_grid  = 2:3,
#'   metrics = c("diversity", "exclusivity"),
#'   holdout = 0,
#'   seed    = 42L,
#'   control = list(fit = list(n_iter = 25, progressbar = FALSE))
#' )
#' print(sel)
#'
#' @export
select_k_topics <- function(
  x,
  engine,
  model,
  k_grid      = 5:15,
  metrics     = c("coherence_npmi", "coherence_umass",
                  "diversity", "exclusivity",
                  "held_out_nll", "held_out_perplexity",
                  "train_nll", "train_perplexity"),
  level       = c("aggregate", "topic", "all"),
  control     = list(),
  holdout     = 0.2,
  ncores      = 1L,
  seed        = NULL,
  return_fits = FALSE,
  top_n       = 10L,
  epsilon     = 1e-12,
  method      = NULL,
  ...
) {
  # Input validation
  if (!is.numeric(k_grid) || length(k_grid) == 0L || anyNA(k_grid) ||
      any(!is.finite(k_grid)) || any(k_grid < 1L) ||
      any(k_grid != as.integer(k_grid))) {
    stop("'k_grid' must be a non-empty vector of positive integers.", call. = FALSE)
  }
  k_grid <- as.integer(k_grid)
  if (anyDuplicated(k_grid)) {
    k_grid <- unique(k_grid)
    warning("Duplicate values in 'k_grid' removed.", call. = FALSE)
  }

  valid_inputs <- c("dgCMatrix", "dfm", "DocumentTermMatrix")
  if (!any(vapply(valid_inputs, function(cl) inherits(x, cl) || methods::is(x, cl),
                  logical(1L)))) {
    stop(sprintf(
      "'x' must be a %s. A corpus is not accepted; convert to a DFM first.",
      paste(valid_inputs, collapse = ", ")
    ), call. = FALSE)
  }

  if (!is.numeric(holdout) || length(holdout) != 1L || is.na(holdout) ||
      !is.finite(holdout) ||
      holdout < 0 || holdout >= 1) {
    stop("'holdout' must be a single number in [0, 1).", call. = FALSE)
  }
  metrics <- .validate_topic_eval_metrics(metrics)
  level <- match.arg(level)
  if (!is.numeric(top_n) || length(top_n) != 1L || is.na(top_n) ||
      !is.finite(top_n) || top_n < 1L ||
      top_n != as.integer(top_n)) {
    stop("'top_n' must be a single positive integer.", call. = FALSE)
  }
  if (!is.numeric(epsilon) || length(epsilon) != 1L || is.na(epsilon) ||
      !is.finite(epsilon) || epsilon <= 0) {
    stop("'epsilon' must be a single positive number.", call. = FALSE)
  }
  .validate_parallel_args(ncores, nchunks = length(k_grid))

  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) == 0L || anyNA(seed) ||
        any(!is.finite(seed)) || any(seed != as.integer(seed))) {
      stop("'seed' must be NULL, a single integer, or a vector of length(k_grid).",
           call. = FALSE)
    }
    if (length(seed) == 1L) {
      seed <- seed + seq(0L, length(k_grid) - 1L)
    }
    if (length(seed) != length(k_grid)) {
      stop("'seed' must be NULL, a single integer, or a vector of length(k_grid).",
           call. = FALSE)
    }
    seed <- as.integer(seed)
  }

  needs_coherence <- any(c("coherence_npmi", "coherence_umass") %in% metrics)
  needs_train_likelihood <- any(c("train_nll", "train_perplexity") %in% metrics)
  needs_heldout <- any(c("held_out_nll", "held_out_perplexity") %in% metrics)
  needs_training_eval <- needs_coherence || needs_train_likelihood
  needs_split <- holdout > 0 && (needs_heldout || needs_training_eval)

  n_docs <- nrow(.as_topic_dgCMatrix(x))
  if (needs_split && n_docs < 50L) {
    warning(sprintf(
      "'x' has only %d documents. The holdout shard may be too small for stable predictive metrics.",
      n_docs
    ), call. = FALSE)
  }

  # Holdout split
  if (needs_split) {
    split_seed <- if (!is.null(seed)) seed[1L] else NULL
    split      <- .k_select_split(x, holdout, split_seed)
    x_train  <- split$train
    x_holdout <- split$holdout
  } else {
    x_train   <- x
    x_holdout <- NULL
  }

  # Per-K worker function
  worker <- function(k_seed_pair) {
    k_val  <- k_seed_pair[[1L]]
    k_seed <- k_seed_pair[[2L]]

    if (!is.null(k_seed)) set.seed(k_seed)

    fit <- fit_topic_model(
      x_train, engine = engine, model = model,
      k = k_val, method = method, control = control, ...
    )

    eval_result <- evaluate_topic_model(
      fit,
      training = if (needs_training_eval) x_train else NULL,
      newdata  = x_holdout,
      metrics  = metrics,
      top_n    = top_n,
      epsilon  = epsilon,
      level    = level
    )
    eval_result[, k := k_val]

    list(eval = eval_result, fit = if (return_fits) fit else NULL)
  }

  # Pair each K value with its seed
  k_seed_pairs <- lapply(seq_along(k_grid), function(i) {
    list(k_grid[i], if (!is.null(seed)) seed[i] else NULL)
  })

  # Run (parallel or sequential)
  ncores_int  <- as.integer(ncores)
  export_vars <- c("x_train", "x_holdout", "engine", "model", "method",
                   "control", "metrics", "top_n", "epsilon", "level",
                   "needs_training_eval", "return_fits")

  if (ncores_int <= 1L) {
    raw <- lapply(k_seed_pairs, worker)
  } else {
    cl <- tryCatch(
      parallel::makeCluster(ncores_int),
      error = function(e) {
        warning(sprintf(
          "PSOCK cluster could not be created; falling back to sequential: %s",
          conditionMessage(e)
        ), call. = FALSE)
        NULL
      }
    )
    if (is.null(cl)) {
      raw <- lapply(k_seed_pairs, worker)
    } else {
      on.exit(parallel::stopCluster(cl), add = TRUE)
      # Load NLPstudio on each worker (required for fit_topic_model etc.)
      parallel::clusterEvalQ(cl, library(NLPstudio, quietly = TRUE))
      parallel::clusterExport(cl, varlist = export_vars, envir = environment())
      raw <- parallel::clusterApplyLB(cl, k_seed_pairs, worker)
    }
  }

  # Assemble output
  eval_list <- lapply(raw, `[[`, "eval")
  out <- data.table::rbindlist(eval_list)
  data.table::setcolorder(out, c("k", "metric", "scope", "topic_id",
                                  "value", "supported"))
  data.table::setorder(out, k, metric, scope, topic_id)

  data.table::setattr(out, "class", c("nlp_k_selection", "data.table", "data.frame"))

  if (return_fits) {
    fits <- lapply(raw, `[[`, "fit")
    names(fits) <- paste0("k", k_grid)
    data.table::setattr(out, "fits", fits)
  }

  out[]
}

#' Create a K-selection holdout split
#'
#' Splits documents into fitting and held-out shards for `select_k_topics()`.
#'
#' @keywords internal
#' @noRd
.k_select_split <- function(x, holdout, seed) {
  mat    <- .as_topic_dgCMatrix(x)
  n_docs <- nrow(mat)
  n_hold <- max(1L, round(n_docs * holdout))
  n_train <- n_docs - n_hold

  if (!is.null(seed)) set.seed(seed)
  hold_idx  <- sample.int(n_docs, n_hold)
  train_idx <- setdiff(seq_len(n_docs), hold_idx)

  # Preserve the original class for the shards
  if (inherits(x, "dfm")) {
    list(train = x[train_idx, ], holdout = x[hold_idx, ])
  } else if (methods::is(x, "DocumentTermMatrix")) {
    list(train = x[train_idx, ], holdout = x[hold_idx, ])
  } else {
    list(train = mat[train_idx, , drop = FALSE],
         holdout = mat[hold_idx, , drop = FALSE])
  }
}

#' Print a Compact Summary of Topic-Count Selection Results
#'
#' @param x An `nlp_k_selection` object returned by [select_k_topics()].
#' @param ... Unused.
#' @returns Invisibly returns `x`.
#' @export
print.nlp_k_selection <- function(x, ...) {
  k_vals <- sort(unique(x$k))

  cat("<nlp_k_selection>\n")
  cat("  K grid:  ", paste(k_vals, collapse = ", "), "\n", sep = "")
  cat("  metrics: ", paste(unique(x$metric), collapse = ", "), "\n\n", sep = "")

  # Best K per metric (overall scope only, supported only)
  overall <- x[x$scope == "overall" & x$supported, ]
  if (nrow(overall) == 0L) {
    cat("  No supported overall metrics to summarize.\n")
    return(invisible(x))
  }

  cat("  Best K per metric (overall scope):\n")
  for (m in unique(overall$metric)) {
    sub <- overall[overall$metric == m, ]
    # For coherence: higher is better (closer to 0)
    # For nll / perplexity: lower is better
    # For diversity / exclusivity: higher is better
    if (m %in% c("held_out_nll", "held_out_perplexity",
                 "train_nll", "train_perplexity")) {
      best_idx <- which.min(sub$value)
    } else {
      best_idx <- which.max(sub$value)
    }
    best_k   <- sub$k[best_idx]
    best_val <- sub$value[best_idx]
    # Flag ties
    if (m %in% c("held_out_nll", "held_out_perplexity",
                 "train_nll", "train_perplexity")) {
      ties <- sum(sub$value == best_val) > 1L
    } else {
      ties <- sum(sub$value == best_val) > 1L
    }
    tie_flag <- if (ties) " [tied]" else ""
    cat(sprintf("    %-20s K = %d  (%.4g)%s\n", m, best_k, best_val, tie_flag))
  }
  invisible(x)
}

#' Plot Topic-Count Selection Results
#'
#' Produces a faceted line chart with \eqn{K} on the x-axis and each metric on its
#' own facet. Per-topic rows are aggregated to their overall mean before
#' plotting; only `scope == "overall"` rows are shown.
#'
#' @param x An `nlp_k_selection` object returned by [select_k_topics()].
#' @param metrics Character vector of metrics to include. Defaults to all
#'   supported metrics present in `x`.
#' @param ... Unused.
#' @returns A [ggplot2::ggplot()] object.
#' @export
plot.nlp_k_selection <- function(x, metrics = NULL, ...) {
  plot_data <- x[x$scope == "overall" & x$supported, ]

  if (!is.null(metrics)) {
    plot_data <- plot_data[plot_data$metric %in% metrics, ]
  }
  if (nrow(plot_data) == 0L) {
    stop("No supported overall metric rows to plot.", call. = FALSE)
  }

  plot_data <- data.table::copy(plot_data)
  plot_data[, k := as.integer(k)]

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = k, y = value)
  ) +
    ggplot2::geom_line(colour = "#2166ac", linewidth = 0.8) +
    ggplot2::geom_point(colour = "#2166ac", size = 2) +
    ggplot2::facet_wrap(
      ~ metric,
      scales = "free_y",
      labeller = ggplot2::label_value
    ) +
    ggplot2::scale_x_continuous(breaks = function(lims) pretty(lims, n = 8L)) +
    ggplot2::labs(
      x     = "Number of topics (K)",
      y     = NULL,
      title = "Topic-count selection"
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "grey92"),
      panel.grid.minor = ggplot2::element_blank()
    )
}
