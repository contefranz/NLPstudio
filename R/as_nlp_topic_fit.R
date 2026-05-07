if (getRversion() >= "2.15.1") {
  utils::globalVariables(character())
}

#' Convert Legacy Topic-Model Objects to `nlp_topic_fit`
#'
#' `as_nlp_topic_fit()` converts supported legacy topic-model outputs into the
#' current `nlp_topic_fit` class used by [fit_topic_model()]. It is primarily a
#' migration helper for saved outputs from the removed `warp_lda()` wrapper.
#'
#' @param x Object to convert.
#' @param ... Additional arguments forwarded to methods.
#'
#' @returns An object of class `c("nlp_topic_fit", "list")`.
#'
#' @export
as_nlp_topic_fit <- function(x, ...) {
  UseMethod("as_nlp_topic_fit")
}

#' @rdname as_nlp_topic_fit
#' @export
as_nlp_topic_fit.nlp_topic_fit <- function(x, ...) {
  x
}

#' @rdname as_nlp_topic_fit
#' @param k Optional topic count. Usually inferred from `theta`, `phi`, or the
#'   stored backend object.
#' @param doc_ids Optional document IDs used when legacy `theta` does not
#'   already contain document identifiers.
#' @param vocab Optional vocabulary used when legacy `phi` does not already
#'   contain term names.
#' @param docvars Optional document metadata to store on the converted object.
#' @param doc_data Optional document metadata or text sidecar to store on the
#'   converted object.
#' @param control Optional backend controls to store as migration metadata. Use
#'   `control$model$doc_topic_prior` and `control$model$topic_word_prior` when
#'   the old model used non-default WarpLDA priors.
#' @param warn_partial Logical. Warn when `theta` or `phi` cannot be recovered.
#'   Defaults to `TRUE`.
#'
#' @examplesIf interactive()
#' old <- readRDS("legacy-warp-lda-output.rds")
#' fit <- as_nlp_topic_fit(old)
#' get_top_terms(fit)
#'
#' @export
as_nlp_topic_fit.list <- function(x, k = NULL, doc_ids = NULL, vocab = NULL,
                                  docvars = NULL, doc_data = NULL,
                                  control = list(), warn_partial = TRUE,
                                  ...) {
  if (!.is_legacy_warp_lda_output(x)) {
    stop(
      "List input must be a legacy warp_lda() output with a WarpLDA 'lda_object'.",
      call. = FALSE
    )
  }
  if (!is.logical(warn_partial) || length(warn_partial) != 1L || is.na(warn_partial)) {
    stop("'warn_partial' must be a single TRUE/FALSE value.", call. = FALSE)
  }

  control <- .normalize_topic_control(control)
  k <- .validate_legacy_warp_k_arg(k)

  model_object <- x$lda_object
  model_tww <- .legacy_warp_model_tww(model_object)
  dtw <- .legacy_warp_dtw_matrix(x$theta, doc_ids = doc_ids)
  tww_source <- if (!is.null(x$phi)) x$phi else model_tww
  tww <- .legacy_warp_tww_matrix(tww_source, vocab = vocab)

  inferred_k <- .legacy_warp_infer_k(
    k = k,
    dtw = dtw,
    tww = tww,
    model_object = model_object
  )

  if (is.null(dtw) && warn_partial) {
    warning(
      "Legacy WarpLDA object does not contain theta; converted fit will not contain cached DTW.",
      call. = FALSE
    )
  }
  if (is.null(tww) && warn_partial) {
    warning(
      "Legacy WarpLDA object does not contain recoverable phi; converted fit will not contain cached TWW.",
      call. = FALSE
    )
  }

  docvars <- .legacy_warp_docvars_table(docvars, doc_ids = rownames(dtw))
  doc_data <- .normalize_doc_data_table(
    doc_data,
    include_text = TRUE,
    arg_name = "doc_data"
  )

  alpha <- .legacy_warp_control_value(control$model$doc_topic_prior, 0.1)
  beta <- .legacy_warp_control_value(control$model$topic_word_prior, 0.001)

  .new_nlp_topic_fit(
    engine = "text2vec",
    model = "lda",
    method = NULL,
    model_object = model_object,
    dtw = dtw,
    tww = tww,
    doc_ids = .legacy_warp_fit_doc_ids(dtw, docvars, doc_data),
    vocab = .legacy_warp_fit_vocab(tww, vocab),
    docvars = docvars,
    doc_data = doc_data,
    hyperparameters = .topic_hyperparameters_table(
      k = inferred_k,
      alpha = alpha,
      beta = beta,
      sources = list(
        k = list(section = "legacy", name = "as_nlp_topic_fit"),
        alpha = list(section = "control", name = "model$doc_topic_prior"),
        beta = list(section = "control", name = "model$topic_word_prior")
      )
    ),
    backend_control = .topic_backend_control(
      model = control$model,
      fit = control$fit,
      optimizer = control$optimizer
    ),
    call = match.call()
  )
}

#' @rdname as_nlp_topic_fit
#' @export
as_nlp_topic_fit.default <- function(x, ...) {
  stop(
    sprintf(
      "Objects of class %s cannot be converted to nlp_topic_fit.",
      paste(class(x), collapse = "/")
    ),
    call. = FALSE
  )
}

.is_legacy_warp_lda_output <- function(x) {
  is.list(x) &&
    !is.null(x$lda_object) &&
    inherits(x$lda_object, "WarpLDA")
}

.validate_legacy_warp_k_arg <- function(k) {
  if (is.null(k)) {
    return(NULL)
  }
  if (!is.numeric(k) || length(k) != 1L || is.na(k) ||
      !is.finite(k) || k < 1L || k != as.integer(k)) {
    stop("'k' must be NULL or a single positive integer.", call. = FALSE)
  }
  as.integer(k)
}

.legacy_warp_dtw_matrix <- function(theta, doc_ids = NULL) {
  if (is.null(theta)) {
    if (!is.null(doc_ids)) {
      stop("'doc_ids' can be supplied only when legacy theta is available.", call. = FALSE)
    }
    return(NULL)
  }

  dt <- data.table::as.data.table(theta)
  if (!is.null(doc_ids)) {
    if (length(doc_ids) != nrow(dt)) {
      stop("'doc_ids' must have one value per theta row.", call. = FALSE)
    }
    ids <- as.character(doc_ids)
  } else if ("doc_id" %in% names(dt)) {
    ids <- as.character(dt$doc_id)
  } else if ("rn" %in% names(dt)) {
    ids <- as.character(dt$rn)
  } else {
    ids <- rownames(theta)
    if (is.null(ids)) {
      ids <- as.character(seq_len(nrow(dt)))
    }
  }

  drop_cols <- intersect(
    c("doc_id", "rn", "topic_max_id", "topic_max_int", "topic_max_value"),
    names(dt)
  )
  topic_dt <- dt[, setdiff(names(dt), drop_cols), with = FALSE]
  if (!ncol(topic_dt)) {
    stop("Legacy theta must contain topic columns.", call. = FALSE)
  }
  if (!all(vapply(topic_dt, is.numeric, logical(1L)))) {
    stop("Legacy theta topic columns must be numeric.", call. = FALSE)
  }

  mat <- as.matrix(topic_dt)
  .dtw_matrix_from_matrix(mat, doc_ids = ids)
}

.legacy_warp_tww_matrix <- function(phi, vocab = NULL) {
  if (is.null(phi)) {
    if (!is.null(vocab)) {
      stop("'vocab' can be supplied only when legacy phi is available or recoverable.", call. = FALSE)
    }
    return(NULL)
  }

  if (data.table::is.data.table(phi) || is.data.frame(phi)) {
    dt <- data.table::as.data.table(phi)
    topic_id_cols <- intersect("topic_id", names(dt))
    term_dt <- dt[, setdiff(names(dt), topic_id_cols), with = FALSE]
    term_names <- names(term_dt)
    if (!all(vapply(term_dt, is.numeric, logical(1L)))) {
      stop("Legacy phi term columns must be numeric.", call. = FALSE)
    }
    mat <- as.matrix(term_dt)
  } else {
    mat <- as.matrix(phi)
    term_names <- colnames(mat)
  }

  if (!ncol(mat)) {
    stop("Legacy phi must contain term columns.", call. = FALSE)
  }
  if (!is.numeric(mat)) {
    stop("Legacy phi must be numeric.", call. = FALSE)
  }

  if (!is.null(vocab)) {
    if (length(vocab) != ncol(mat)) {
      stop("'vocab' must have one value per phi column.", call. = FALSE)
    }
    term_names <- as.character(vocab)
  }

  .tww_matrix_from_matrix(mat, term_names = term_names)
}

.legacy_warp_model_tww <- function(model_object) {
  out <- tryCatch(
    model_object$topic_word_distribution,
    error = function(e) NULL
  )
  if (is.null(out)) {
    return(NULL)
  }
  out
}

.legacy_warp_model_k <- function(model_object) {
  out <- tryCatch(
    model_object$n_topics,
    error = function(e) NULL
  )
  if (is.null(out) || length(out) != 1L || is.na(out) || !is.finite(out)) {
    return(NULL)
  }
  as.integer(out)
}

.legacy_warp_infer_k <- function(k, dtw, tww, model_object) {
  candidates <- c(
    argument = if (is.null(k)) NA_integer_ else as.integer(k),
    dtw = if (is.null(dtw)) NA_integer_ else ncol(dtw),
    tww = if (is.null(tww)) NA_integer_ else nrow(tww),
    model_object = {
      model_k <- .legacy_warp_model_k(model_object)
      if (is.null(model_k)) NA_integer_ else model_k
    }
  )
  candidates <- candidates[!is.na(candidates)]
  candidates <- as.integer(candidates)

  if (!length(candidates)) {
    stop(
      "Could not infer the topic count from the legacy WarpLDA object; supply 'k'.",
      call. = FALSE
    )
  }
  if (length(unique(candidates)) != 1L) {
    stop(
      "Legacy WarpLDA components disagree on the number of topics.",
      call. = FALSE
    )
  }
  candidates[1L]
}

.legacy_warp_control_value <- function(x, default) {
  if (is.null(x) || length(x) == 0L) {
    return(default)
  }
  x
}

.legacy_warp_docvars_table <- function(docvars, doc_ids) {
  if (is.null(docvars)) {
    return(NULL)
  }
  if (!data.table::is.data.table(docvars) && !is.data.frame(docvars)) {
    stop("'docvars' must be a data.frame or data.table.", call. = FALSE)
  }
  out <- data.table::as.data.table(docvars)
  if (!"doc_id" %in% names(out)) {
    if (is.null(doc_ids)) {
      stop("'docvars' must contain a 'doc_id' column when DTW is unavailable.", call. = FALSE)
    }
    if (nrow(out) != length(doc_ids)) {
      stop("'docvars' must have one row per document.", call. = FALSE)
    }
    out[, doc_id := as.character(doc_ids)]
    data.table::setcolorder(out, "doc_id")
  } else {
    out[, doc_id := as.character(doc_id)]
  }
  out[]
}

.legacy_warp_fit_doc_ids <- function(dtw, docvars, doc_data) {
  if (!is.null(dtw)) {
    return(rownames(dtw))
  }
  if (!is.null(docvars) && "doc_id" %in% names(docvars)) {
    return(as.character(docvars$doc_id))
  }
  if (!is.null(doc_data) && "doc_id" %in% names(doc_data)) {
    return(as.character(doc_data$doc_id))
  }
  character()
}

.legacy_warp_fit_vocab <- function(tww, vocab) {
  if (!is.null(tww)) {
    return(colnames(tww))
  }
  if (!is.null(vocab)) {
    return(as.character(vocab))
  }
  NULL
}
