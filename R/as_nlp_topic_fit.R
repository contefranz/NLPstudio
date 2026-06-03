if (getRversion() >= "2.15.1") {
  utils::globalVariables(character())
}

#' Convert Existing Topic-Model Objects to `nlp_topic_fit`
#'
#' `as_nlp_topic_fit()` converts supported fitted topic-model objects into the
#' current `nlp_topic_fit` class used by [fit_topic_model()]. It can adopt raw
#' fits from supported backends and saved outputs from the removed `warp_lda()`
#' wrapper without refitting models.
#'
#' Supported input families are:
#'
#' - **topicmodels** S4 fits from [topicmodels::LDA()] and [topicmodels::CTM()]
#'   (`LDA_Gibbs`, `LDA_VEM`, and `CTM_VEM`);
#' - **seededlda** `textmodel` fits from `textmodel_lda()` and
#'   `textmodel_seededlda()`;
#' - raw **text2vec** WarpLDA/LDA R6 objects, optionally paired with the
#'   `theta` matrix returned by `fit_transform()`;
#' - raw **stm** `STM` objects without content covariates;
#' - saved list outputs from the removed NLPstudio `warp_lda()` wrapper.
#'
#' The conversion is non-refitting. It standardizes cached DTW/TWW matrices,
#' topic IDs, document IDs, vocabulary, and metadata where those components are
#' already present on the input object. Raw **text2vec** objects do not retain
#' document-topic weights internally, so pass `theta` when downstream DTW access
#' is needed.
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
#' @param theta Optional document-topic matrix for raw **text2vec** WarpLDA
#'   objects. Raw WarpLDA objects do not retain the return value of
#'   `fit_transform()`, so pass that matrix here when available.
#' @param doc_ids Optional document IDs used when legacy `theta` does not
#'   already contain document identifiers.
#' @param vocab Optional vocabulary used when legacy `phi` does not already
#'   contain term names.
#' @param model Optional model family override for raw **seededlda** objects.
#'   Use `"seqlda"` for sequential LDA fits, which are not reliably
#'   distinguishable from ordinary seededlda LDA after fitting.
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
#' @details
#' Raw **stm** content-covariate models are not converted because
#' they imply covariate-specific topic-word distributions, while NLPstudio
#' currently standardizes one TWW matrix per fit.
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
  .as_text2vec_warp_nlp_topic_fit(
    model_object = x$lda_object,
    theta = x$theta,
    phi = x$phi,
    k = k,
    doc_ids = doc_ids,
    vocab = vocab,
    docvars = docvars,
    doc_data = doc_data,
    control = control,
    warn_partial = warn_partial,
    allow_doc_ids_without_theta = FALSE,
    missing_theta_message = "Legacy WarpLDA object does not contain theta; converted fit will not contain cached DTW.",
    missing_phi_message = "Legacy WarpLDA object does not contain recoverable phi; converted fit will not contain cached TWW.",
    call = match.call()
  )
}

#' @rdname as_nlp_topic_fit
#' @export
as_nlp_topic_fit.TopicModel <- function(x, docvars = NULL, doc_data = NULL, ...) {
  if (!isS4(x)) {
    stop(
      "TopicModel conversion currently supports S4 objects from the topicmodels package.",
      call. = FALSE
    )
  }
  model <- .topicmodels_adopt_model_name(x)
  method <- .topicmodels_adopt_method_name(x)
  dtw <- .dtw_matrix_from_matrix(x@gamma, doc_ids = .topicmodels_doc_ids(x))
  tww <- .tww_matrix_from_matrix(x@beta, term_names = x@terms, log_scale = TRUE)
  docvars <- .legacy_warp_docvars_table(docvars, doc_ids = rownames(dtw))
  doc_data <- .normalize_doc_data_table(
    doc_data,
    include_text = TRUE,
    arg_name = "doc_data"
  )
  .new_nlp_topic_fit(
    engine = "topicmodels",
    model = model,
    method = method,
    model_object = x,
    dtw = dtw,
    tww = tww,
    doc_ids = rownames(dtw),
    vocab = colnames(tww),
    docvars = docvars,
    doc_data = doc_data,
    hyperparameters = .topicmodels_hyperparameters(x, model, x@k, method),
    backend_control = .topic_backend_control(
      model = list(),
      fit = .s4_slots_to_list(x@control),
      optimizer = list()
    ),
    call = match.call()
  )
}

#' @rdname as_nlp_topic_fit
#' @export
as_nlp_topic_fit.LDA_Gibbs <- function(x, docvars = NULL, doc_data = NULL, ...) {
  as_nlp_topic_fit.TopicModel(x, docvars = docvars, doc_data = doc_data, ...)
}

#' @rdname as_nlp_topic_fit
#' @export
as_nlp_topic_fit.LDA_VEM <- function(x, docvars = NULL, doc_data = NULL, ...) {
  as_nlp_topic_fit.TopicModel(x, docvars = docvars, doc_data = doc_data, ...)
}

#' @rdname as_nlp_topic_fit
#' @export
as_nlp_topic_fit.CTM_VEM <- function(x, docvars = NULL, doc_data = NULL, ...) {
  as_nlp_topic_fit.TopicModel(x, docvars = docvars, doc_data = doc_data, ...)
}

#' @rdname as_nlp_topic_fit
#' @export
as_nlp_topic_fit.textmodel <- function(x, model = NULL, docvars = NULL,
                                       doc_data = NULL, ...) {
  model <- .seededlda_adopt_model_name(x, model)
  dtw <- .dtw_matrix_from_matrix(x$theta, doc_ids = .seededlda_doc_ids(x))
  tww <- .tww_matrix_from_matrix(x$phi, term_names = colnames(x$phi))
  docvars <- .legacy_warp_docvars_table(docvars, doc_ids = rownames(dtw))
  doc_data <- .normalize_doc_data_table(
    doc_data,
    include_text = TRUE,
    arg_name = "doc_data"
  )
  .new_nlp_topic_fit(
    engine = "seededlda",
    model = model,
    method = NULL,
    model_object = x,
    dtw = dtw,
    tww = tww,
    doc_ids = rownames(dtw),
    vocab = colnames(tww),
    docvars = docvars,
    doc_data = doc_data,
    hyperparameters = .topic_hyperparameters_table(
      k = x$k,
      alpha = x$alpha,
      beta = x$beta,
      sources = list(
        k = list(section = "model_object", name = "k"),
        alpha = list(section = "model_object", name = "alpha"),
        beta = list(section = "model_object", name = "beta")
      )
    ),
    backend_control = .topic_backend_control(
      model = list(),
      fit = .seededlda_adopt_control(x),
      optimizer = list()
    ),
    call = match.call()
  )
}

#' @rdname as_nlp_topic_fit
#' @export
as_nlp_topic_fit.textmodel_lda <- function(x, model = NULL, docvars = NULL,
                                           doc_data = NULL, ...) {
  as_nlp_topic_fit.textmodel(
    x,
    model = model,
    docvars = docvars,
    doc_data = doc_data,
    ...
  )
}

#' @rdname as_nlp_topic_fit
#' @export
as_nlp_topic_fit.WarpLDA <- function(x, theta = NULL, doc_ids = NULL,
                                     vocab = NULL, docvars = NULL,
                                     doc_data = NULL, control = list(),
                                     warn_partial = TRUE, ...) {
  .as_text2vec_warp_nlp_topic_fit(
    model_object = x,
    theta = theta,
    phi = NULL,
    k = NULL,
    doc_ids = doc_ids,
    vocab = vocab,
    docvars = docvars,
    doc_data = doc_data,
    control = control,
    warn_partial = warn_partial,
    allow_doc_ids_without_theta = TRUE,
    missing_theta_message = "Raw text2vec WarpLDA objects do not retain DTW; pass the fit_transform() output via 'theta' to cache DTW.",
    missing_phi_message = "Raw text2vec WarpLDA object does not contain recoverable TWW.",
    call = match.call()
  )
}

#' @rdname as_nlp_topic_fit
#' @export
as_nlp_topic_fit.STM <- function(x, doc_ids = NULL, docvars = NULL,
                                 doc_data = NULL, ...) {
  tww <- .stm_tww_matrix(x)
  doc_ids <- .stm_doc_ids(x, doc_ids = doc_ids)
  dtw <- .dtw_matrix_from_matrix(x$theta, doc_ids = doc_ids)
  tww <- .tww_matrix_from_matrix(tww, term_names = x$vocab)
  docvars <- .legacy_warp_docvars_table(docvars, doc_ids = rownames(dtw))
  doc_data <- .normalize_doc_data_table(
    doc_data,
    include_text = TRUE,
    arg_name = "doc_data"
  )
  .new_nlp_topic_fit(
    engine = "stm",
    model = "stm",
    method = NULL,
    model_object = x,
    dtw = dtw,
    tww = tww,
    doc_ids = rownames(dtw),
    vocab = colnames(tww),
    docvars = docvars,
    doc_data = doc_data,
    hyperparameters = .topic_hyperparameters_table(
      k = .stm_topic_count(x),
      alpha = NA_real_,
      beta = NA_real_,
      sources = list(k = list(section = "model_object", name = "settings$dim$K"))
    ),
    backend_control = .topic_backend_control(
      model = list(),
      fit = if (!is.null(x$settings$call)) as.list(x$settings$call)[-1L] else list(),
      optimizer = list()
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

.as_text2vec_warp_nlp_topic_fit <- function(model_object, theta = NULL,
                                            phi = NULL, k = NULL,
                                            doc_ids = NULL, vocab = NULL,
                                            docvars = NULL, doc_data = NULL,
                                            control = list(),
                                            warn_partial = TRUE,
                                            allow_doc_ids_without_theta = FALSE,
                                            missing_theta_message,
                                            missing_phi_message,
                                            call) {
  if (!is.logical(warn_partial) || length(warn_partial) != 1L || is.na(warn_partial)) {
    stop("'warn_partial' must be a single TRUE/FALSE value.", call. = FALSE)
  }
  control <- .normalize_topic_control(control)
  k <- .validate_legacy_warp_k_arg(k)

  explicit_doc_ids <- NULL
  dtw_doc_ids <- doc_ids
  if (is.null(theta) && allow_doc_ids_without_theta && !is.null(doc_ids)) {
    explicit_doc_ids <- as.character(doc_ids)
    dtw_doc_ids <- NULL
  }

  model_tww <- .legacy_warp_model_tww(model_object)
  dtw <- .legacy_warp_dtw_matrix(theta, doc_ids = dtw_doc_ids)
  tww_source <- if (!is.null(phi)) phi else model_tww
  tww <- .legacy_warp_tww_matrix(tww_source, vocab = vocab)

  inferred_k <- .legacy_warp_infer_k(
    k = k,
    dtw = dtw,
    tww = tww,
    model_object = model_object
  )

  if (is.null(dtw) && warn_partial) {
    warning(missing_theta_message, call. = FALSE)
  }
  if (is.null(tww) && warn_partial) {
    warning(missing_phi_message, call. = FALSE)
  }

  docvars <- .legacy_warp_docvars_table(
    docvars,
    doc_ids = if (!is.null(dtw)) rownames(dtw) else explicit_doc_ids
  )
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
    doc_ids = .legacy_warp_fit_doc_ids(dtw, docvars, doc_data, explicit_doc_ids),
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
    call = call
  )
}

.topicmodels_adopt_model_name <- function(x) {
  cls <- class(x)[1L]
  valid <- c("LDA_Gibbs", "LDA_VEM", "CTM_VEM")
  if (!cls %in% valid) {
    stop(
      sprintf(
        "Unsupported topicmodels object class '%s'. Supported classes are: %s.",
        cls,
        paste(valid, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (grepl("^CTM", cls)) {
    return("ctm")
  }
  "lda"
}

.topicmodels_adopt_method_name <- function(x) {
  cls <- class(x)[1L]
  if (grepl("Gibbs", cls, fixed = TRUE)) {
    return("Gibbs")
  }
  "VEM"
}

.seededlda_adopt_model_name <- function(x, model = NULL) {
  if (is.null(x$theta) || is.null(x$phi)) {
    stop(
      "seededlda textmodel objects must contain 'theta' and 'phi' to be converted.",
      call. = FALSE
    )
  }
  valid <- c("lda", "seqlda", "seededlda")
  if (!is.null(model)) {
    if (!is.character(model) || length(model) != 1L || is.na(model) ||
        !model %in% valid) {
      stop("'model' must be one of 'lda', 'seqlda', or 'seededlda'.", call. = FALSE)
    }
    return(model)
  }

  call_text <- paste(deparse(x$call), collapse = " ")
  if (grepl("textmodel_seededlda", call_text, fixed = TRUE)) {
    return("seededlda")
  }
  "lda"
}

.seededlda_adopt_control <- function(x) {
  keep <- intersect(
    c(
      "max_iter", "last_iter", "auto_iter", "adjust_alpha", "epsilon",
      "gamma", "batch_size", "concatenator", "version"
    ),
    names(x)
  )
  out <- x[keep]
  if ("version" %in% names(out)) {
    out$version <- as.character(out$version)
  }
  out
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
      stop("'doc_ids' can be supplied only when theta is available.", call. = FALSE)
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
    stop("theta must contain topic columns.", call. = FALSE)
  }
  if (!all(vapply(topic_dt, is.numeric, logical(1L)))) {
    stop("theta topic columns must be numeric.", call. = FALSE)
  }

  mat <- as.matrix(topic_dt)
  .dtw_matrix_from_matrix(mat, doc_ids = ids)
}

.legacy_warp_tww_matrix <- function(phi, vocab = NULL) {
  if (is.null(phi)) {
    if (!is.null(vocab)) {
      stop("'vocab' can be supplied only when phi is available or recoverable.", call. = FALSE)
    }
    return(NULL)
  }

  if (data.table::is.data.table(phi) || is.data.frame(phi)) {
    dt <- data.table::as.data.table(phi)
    topic_id_cols <- intersect("topic_id", names(dt))
    term_dt <- dt[, setdiff(names(dt), topic_id_cols), with = FALSE]
    term_names <- names(term_dt)
    if (!all(vapply(term_dt, is.numeric, logical(1L)))) {
      stop("phi term columns must be numeric.", call. = FALSE)
    }
    mat <- as.matrix(term_dt)
  } else {
    mat <- as.matrix(phi)
    term_names <- colnames(mat)
  }

  if (!ncol(mat)) {
    stop("phi must contain term columns.", call. = FALSE)
  }
  if (!is.numeric(mat)) {
    stop("phi must be numeric.", call. = FALSE)
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
      "Could not infer the topic count from the text2vec topic model; supply 'k' for legacy list inputs.",
      call. = FALSE
    )
  }
  if (length(unique(candidates)) != 1L) {
    stop(
      "text2vec topic-model components disagree on the number of topics.",
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

.legacy_warp_fit_doc_ids <- function(dtw, docvars, doc_data, explicit_doc_ids = NULL) {
  if (!is.null(dtw)) {
    return(rownames(dtw))
  }
  if (!is.null(explicit_doc_ids)) {
    return(as.character(explicit_doc_ids))
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
