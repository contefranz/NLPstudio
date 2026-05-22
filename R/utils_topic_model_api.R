# Internal helpers for the topic-model API
#
# These helpers support the exported functions in R/topic_model_api.R. They
# are intentionally documented in source with roxygen comments and @noRd, but
# they are not part of the public package interface.

# -----------------------------------------------------------------------------
# Fit helpers
# -----------------------------------------------------------------------------
#
# Validation, backend fitting, hyperparameter extraction, and backend-control sanitization used by `fit_topic_model()`.

#' Validate and normalize a topic-model family
#'
#' Checks that a requested model is supported for the selected backend and returns its canonical lowercase name.
#'
#' @keywords internal
#' @noRd
.match_topic_model <- function(engine, model) {
  if (!is.character(model) || length(model) != 1L || is.na(model)) {
    stop("model must be a single character string.")
  }

  model <- tolower(model)
  allowed <- switch(
    engine,
    text2vec = "lda",
    topicmodels = c("lda", "ctm"),
    seededlda = c("lda", "seqlda", "seededlda"),
    `topicmodels.etm` = "etm",
    stm = "stm"
  )

  if (!model %in% allowed) {
    stop(
      sprintf(
        "Unsupported model '%s' for engine '%s'. Allowed values are: %s.",
        model,
        engine,
        paste(allowed, collapse = ", ")
      )
    )
  }

  model
}

#' Normalize a backend topic-model method
#'
#' Resolves default methods and validates method restrictions for backend/model combinations.
#'
#' @keywords internal
#' @noRd
.normalize_topic_method <- function(engine, model, method) {
  if (engine == "topicmodels" && model == "lda") {
    if (is.null(method)) {
      return("VEM")
    }
    if (!is.character(method) || length(method) != 1L || is.na(method)) {
      stop("method must be NULL or a single character string.")
    }
    method <- toupper(method)
    if (!method %in% c("VEM", "GIBBS")) {
      stop("For topicmodels LDA, method must be 'VEM' or 'Gibbs'.")
    }
    return(if (method == "GIBBS") "Gibbs" else "VEM")
  }

  if (engine == "topicmodels" && model == "ctm") {
    if (is.null(method)) {
      return("VEM")
    }
    if (!identical(toupper(method), "VEM")) {
      stop("CTM only supports method = 'VEM'.")
    }
    return("VEM")
  }

  if (!is.null(method)) {
    stop(sprintf(
      "method must be NULL for engine = '%s', model = '%s'.",
      engine,
      model
    ))
  }

  NULL
}

#' Normalize topic-model control lists
#'
#' Validates the top-level `control` structure and returns model, fit, and optimizer sublists.
#'
#' @keywords internal
#' @noRd
.normalize_topic_control <- function(control) {
  if (is.null(control) || !length(control)) {
    return(list(model = list(), fit = list(), optimizer = list()))
  }
  if (!is.list(control)) {
    stop("control must be a list.")
  }

  nms <- names(control)
  if (is.null(nms) || any(nms == "")) {
    stop("control must be a named list with optional 'model', 'fit', and 'optimizer' entries.")
  }

  extra <- setdiff(nms, c("model", "fit", "optimizer"))
  if (length(extra)) {
    stop(
      sprintf(
        "Unknown top-level control entries: %s.",
        paste(extra, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  out <- list(
    model = if ("model" %in% nms) control$model else list(),
    fit = if ("fit" %in% nms) control$fit else list(),
    optimizer = if ("optimizer" %in% nms) control$optimizer else list()
  )

  if (is.null(out$model)) {
    out$model <- list()
  }
  if (is.null(out$fit)) {
    out$fit <- list()
  }
  if (is.null(out$optimizer)) {
    out$optimizer <- list()
  }
  if (!is.list(out$model) || !is.list(out$fit) || !is.list(out$optimizer)) {
    stop("control$model, control$fit, and control$optimizer must all be lists.")
  }

  out
}

#' Validate topic-model fit arguments
#'
#' Applies cross-backend argument checks before dispatching to a backend fitter.
#'
#' @keywords internal
#' @noRd
.validate_topic_fit_args <- function(engine, model, method, k, control,
                                     dictionary, seedwords, initial_model) {
  if (is.null(k) && !(engine == "seededlda" && model == "seededlda")) {
    stop("k must be supplied for the selected engine/model combination.")
  }
  if (!is.null(k) && engine == "seededlda" && model == "seededlda") {
    stop("k is not used when engine = 'seededlda' and model = 'seededlda'.")
  }
  if (engine != "seededlda" && !is.null(dictionary)) {
    stop("dictionary is only valid when engine = 'seededlda' and model = 'seededlda'.")
  }
  if (engine == "seededlda" && model != "seededlda" && !is.null(dictionary)) {
    stop("dictionary is only valid for engine = 'seededlda' and model = 'seededlda'.")
  }
  if (engine == "seededlda" && model == "seededlda" && is.null(dictionary)) {
    stop("dictionary must be supplied when engine = 'seededlda' and model = 'seededlda'.")
  }
  if (!is.null(seedwords) &&
      !(engine == "topicmodels" && model == "lda" && identical(method, "Gibbs"))) {
    stop("seedwords is only valid for topicmodels LDA with method = 'Gibbs'.")
  }
  if (!(engine %in% c("text2vec", "topicmodels.etm")) && length(control$model)) {
    stop(
      "control$model must be empty unless engine = 'text2vec' or 'topicmodels.etm'.",
      call. = FALSE
    )
  }
  if (engine != "topicmodels.etm" && length(control$optimizer)) {
    stop(
      "control$optimizer must be empty unless engine = 'topicmodels.etm'.",
      call. = FALSE
    )
  }
  if (engine == "text2vec" && !is.null(initial_model)) {
    stop("initial_model is not supported for engine = 'text2vec'.")
  }
  if (engine == "seededlda" && model == "seededlda" && !is.null(initial_model)) {
    stop("initial_model is not supported for engine = 'seededlda', model = 'seededlda'.")
  }
  if (engine == "topicmodels.etm" && !is.null(initial_model)) {
    stop("initial_model is not supported for engine = 'topicmodels.etm'.")
  }
  if (engine == "stm" && !is.null(initial_model)) {
    stop("initial_model is not supported for engine = 'stm'.")
  }
}

#' Build standardized hyperparameter rows
#'
#' Creates the stable `k`/`alpha`/`beta` table stored on topic-model fits.
#'
#' @keywords internal
#' @noRd
.topic_hyperparameters_table <- function(k = NA_real_, alpha = NA_real_,
                                         beta = NA_real_, sources = list()) {
  params <- c("k", "alpha", "beta")
  values <- list(
    .normalize_hyperparameter_value(k),
    .normalize_hyperparameter_value(alpha),
    .normalize_hyperparameter_value(beta)
  )

  source_section <- vapply(params, function(p) {
    src <- sources[[p]]
    if (is.null(src) || is.null(src$section)) {
      return(NA_character_)
    }
    as.character(src$section)[1L]
  }, character(1L))

  source_name <- vapply(params, function(p) {
    src <- sources[[p]]
    if (is.null(src) || is.null(src$name)) {
      return(NA_character_)
    }
    as.character(src$name)[1L]
  }, character(1L))

  data.table::data.table(
    parameter = params,
    value = I(values),
    source_section = source_section,
    source_name = source_name
  )
}

#' Normalize a stored hyperparameter value
#'
#' Converts empty values to `NA` and collapses symmetric numeric vectors to scalars.
#'
#' @keywords internal
#' @noRd
.normalize_hyperparameter_value <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_real_)
  }
  if (!is.numeric(x) && !is.integer(x)) {
    return(x)
  }

  x <- unname(as.numeric(x))
  if (length(x) == 1L) {
    return(x)
  }
  finite <- is.finite(x)
  if (all(finite) && length(unique(x)) == 1L) {
    return(x[1L])
  }
  x
}

#' Extract topicmodels hyperparameters
#'
#' Maps topicmodels-native slots and controls onto package-standard `k`, `alpha`, and `beta` rows.
#'
#' @keywords internal
#' @noRd
.topicmodels_hyperparameters <- function(model_object, model, k, method) {
  if (!identical(model, "lda")) {
    return(.topic_hyperparameters_table(
      k = k,
      alpha = NA_real_,
      beta = NA_real_,
      sources = list(k = list(section = "argument", name = "k"))
    ))
  }

  beta <- NA_real_
  beta_source <- NULL
  if (identical(method, "Gibbs") && "delta" %in% methods::slotNames(model_object@control)) {
    beta <- model_object@control@delta
    beta_source <- list(section = "fit", name = "delta")
  }

  sources <- list(
    k = list(section = "argument", name = "k"),
    alpha = list(section = "model_object", name = "alpha")
  )
  if (!is.null(beta_source)) {
    sources$beta <- beta_source
  }

  .topic_hyperparameters_table(
    k = k,
    alpha = model_object@alpha,
    beta = beta,
    sources = sources
  )
}

#' Return seededlda fitting defaults
#'
#' Mirrors seededlda defaults so stored backend controls reflect implicit arguments as well as user overrides.
#'
#' @keywords internal
#' @noRd
.seededlda_default_fit_args <- function(model, k) {
  switch(
    model,
    lda = list(
      k = k,
      max_iter = 2000L,
      auto_iter = FALSE,
      alpha = 0.5,
      beta = 0.1,
      gamma = 0,
      adjust_alpha = 0,
      update_model = FALSE,
      batch_size = 1L
    ),
    seqlda = list(
      k = k,
      max_iter = 2000L,
      auto_iter = FALSE,
      alpha = 0.5,
      beta = 0.1,
      batch_size = 1L
    ),
    seededlda = list(
      levels = 1L,
      valuetype = "glob",
      case_insensitive = TRUE,
      residual = 0,
      weight = 0.01,
      max_iter = 2000L,
      auto_iter = FALSE,
      alpha = 0.5,
      beta = 0.1,
      gamma = 0,
      adjust_alpha = 0,
      batch_size = 1L
    )
  )
}

#' Build sanitized backend-control metadata
#'
#' Stores model, fit, and optimizer controls after removing heavy inputs and normalizing opaque objects.
#'
#' @keywords internal
#' @noRd
.topic_backend_control <- function(model = list(), fit = list(),
                                   optimizer = list()) {
  list(
    model = .sanitize_backend_control_list(model, drop_names = character()),
    fit = .sanitize_backend_control_list(
      fit,
      drop_names = c("x", "data", "model", "optimizer", "dictionary")
    ),
    optimizer = .sanitize_backend_control_list(
      optimizer,
      drop_names = c("params")
    )
  )
}

#' Sanitize a backend-control list
#'
#' Drops requested heavy entries and sanitizes remaining values for storage on a fit object.
#'
#' @keywords internal
#' @noRd
.sanitize_backend_control_list <- function(x, drop_names = character()) {
  if (is.null(x) || !length(x)) {
    return(list())
  }
  x <- as.list(x)
  if (length(drop_names)) {
    x <- x[setdiff(names(x), drop_names)]
  }
  lapply(x, .sanitize_backend_control_value)
}

#' Sanitize a backend-control value
#'
#' Replaces large matrices, model objects, functions, and environments with compact summaries.
#'
#' @keywords internal
#' @noRd
.sanitize_backend_control_value <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.function(x)) {
    return("<function>")
  }
  if (is.environment(x)) {
    return(sprintf("<%s>", class(x)[1L]))
  }
  if (methods::is(x, "dgCMatrix") || inherits(x, "dfm") ||
      methods::is(x, "DocumentTermMatrix")) {
    return(list(class = class(x)[1L], dim = dim(x)))
  }
  if (is.matrix(x)) {
    return(list(class = "matrix", dim = dim(x)))
  }
  if (methods::is(x, "simple_triplet_matrix")) {
    return(list(class = "simple_triplet_matrix", dim = dim(x)))
  }
  if (methods::is(x, "BasicTextmodel") || methods::is(x, "TopicModel")) {
    return(sprintf("<%s>", class(x)[1L]))
  }
  if (isS4(x)) {
    return(.s4_slots_to_list(x))
  }
  if (is.list(x)) {
    return(lapply(x, .sanitize_backend_control_value))
  }

  x
}

#' Convert S4 slots to a list
#'
#' Recursively converts S4 slot contents so backend controls can be stored as ordinary lists.
#'
#' @keywords internal
#' @noRd
.s4_slots_to_list <- function(x) {
  out <- lapply(methods::slotNames(x), function(nm) {
    .sanitize_backend_control_value(methods::slot(x, nm))
  })
  names(out) <- methods::slotNames(x)
  out
}

#' Fit a text2vec topic model
#'
#' Converts supported inputs and fits the text2vec LDA backend with standardized outputs.
#'
#' @keywords internal
#' @noRd
.fit_text2vec_topic_model <- function(x, k, control) {
  if (!requireNamespace("text2vec", quietly = TRUE)) {
    stop("Package 'text2vec' must be installed to use engine = 'text2vec'.", call. = FALSE)
  }

  x_sparse <- .as_topic_dgCMatrix(x)

  lda_args <- utils::modifyList(
    list(
      n_topics = k,
      doc_topic_prior = 0.1,
      topic_word_prior = 0.001
    ),
    control$model
  )
  lda_args$n_topics <- k
  lda_args$method <- NULL

  fit_args <- utils::modifyList(
    list(
      x = x_sparse,
      n_iter = 1000,
      convergence_tol = 0.001,
      n_check_convergence = 25,
      progressbar = TRUE
    ),
    control$fit
  )
  fit_args$x <- x_sparse

  model_object <- do.call(text2vec::LDA$new, lda_args)
  dtw <- do.call(model_object$fit_transform, fit_args)
  tww <- model_object$topic_word_distribution

  list(
    model_object = model_object,
    dtw = dtw,
    tww = tww,
    doc_ids = .matrix_doc_ids(dtw, fallback = rownames(x_sparse)),
    term_names = colnames(tww),
    method = NULL,
    hyperparameters = .topic_hyperparameters_table(
      k = k,
      alpha = lda_args$doc_topic_prior,
      beta = lda_args$topic_word_prior,
      sources = list(
        k = list(section = "argument", name = "k"),
        alpha = list(section = "model", name = "doc_topic_prior"),
        beta = list(section = "model", name = "topic_word_prior")
      )
    ),
    backend_control = .topic_backend_control(
      model = lda_args,
      fit = fit_args,
      optimizer = list()
    )
  )
}

#' Fit a topicmodels topic model
#'
#' Converts supported inputs and fits topicmodels LDA or CTM with standardized outputs.
#'
#' @keywords internal
#' @noRd
.fit_topicmodels_topic_model <- function(x, model, k, method, control,
                                         seedwords, initial_model) {
  if (!requireNamespace("topicmodels", quietly = TRUE)) {
    stop("Package 'topicmodels' must be installed to use engine = 'topicmodels'.", call. = FALSE)
  }

  x_topicmodels <- .as_topicmodels_input(x)
  control_arg <- if (length(control$fit)) control$fit else NULL

  fit_args <- list(
    x = x_topicmodels,
    k = k,
    method = method,
    control = control_arg,
    model = initial_model
  )

  if (!is.null(seedwords)) {
    fit_args$seedwords <- seedwords
  }

  model_object <- if (model == "lda") {
    do.call(topicmodels::LDA, fit_args)
  } else {
    do.call(topicmodels::CTM, fit_args)
  }

  list(
    model_object = model_object,
    dtw = model_object@gamma,
    tww = exp(model_object@beta),
    doc_ids = .topicmodels_doc_ids(model_object),
    term_names = model_object@terms,
    method = method,
    hyperparameters = .topicmodels_hyperparameters(model_object, model, k, method),
    backend_control = .topic_backend_control(
      model = list(),
      fit = .s4_slots_to_list(model_object@control),
      optimizer = list()
    )
  )
}

#' Fit a seededlda topic model
#'
#' Converts supported inputs and fits seededlda LDA, sequential LDA, or seeded LDA with standardized outputs.
#'
#' @keywords internal
#' @noRd
.fit_seededlda_topic_model <- function(x, model, k, control, dictionary,
                                       initial_model) {
  if (!requireNamespace("seededlda", quietly = TRUE)) {
    stop("Package 'seededlda' must be installed to use engine = 'seededlda'.", call. = FALSE)
  }

  x_dfm <- .as_topic_dfm(x)

  default_fit_args <- .seededlda_default_fit_args(model, k)
  fit_args <- utils::modifyList(c(list(x = x_dfm), default_fit_args), control$fit)
  fit_fun <- switch(
    model,
    lda = seededlda::textmodel_lda,
    seqlda = seededlda::textmodel_seqlda,
    seededlda = seededlda::textmodel_seededlda
  )

  if (model == "seededlda") {
    fit_args$dictionary <- dictionary
  } else {
    fit_args$k <- k
    if (!is.null(initial_model)) {
      fit_args$model <- initial_model
    }
  }

  fit_args$x <- x_dfm
  model_object <- do.call(fit_fun, fit_args)

  list(
    model_object = model_object,
    dtw = model_object$theta,
    tww = model_object$phi,
    doc_ids = .seededlda_doc_ids(model_object),
    term_names = colnames(model_object$phi),
    method = NULL,
    hyperparameters = .topic_hyperparameters_table(
      k = model_object$k,
      alpha = model_object$alpha,
      beta = model_object$beta,
      sources = list(
        k = list(section = "model_object", name = "k"),
        alpha = list(section = "model_object", name = "alpha"),
        beta = list(section = "model_object", name = "beta")
      )
    ),
    backend_control = .topic_backend_control(
      model = list(),
      fit = fit_args,
      optimizer = list()
    )
  )
}

#' Fit an embedded topic model
#'
#' Prepares ETM input, fits the topicmodels.etm backend, and extracts standardized outputs.
#'
#' @keywords internal
#' @noRd
.fit_etm_topic_model <- function(x, k, control) {
  if (!requireNamespace("topicmodels.etm", quietly = TRUE)) {
    stop("Package 'topicmodels.etm' must be installed to use engine = 'topicmodels.etm'.", call. = FALSE)
  }
  if (!requireNamespace("torch", quietly = TRUE)) {
    stop("Package 'torch' must be installed to use engine = 'topicmodels.etm'.", call. = FALSE)
  }
  if (!torch::torch_is_installed()) {
    stop(
      "The torch backend is not installed. Run torch::install_torch() to use engine = 'topicmodels.etm'.",
      call. = FALSE
    )
  }

  prep <- .prepare_etm_input(x, control$model)

  model_args <- utils::modifyList(
    list(k = k),
    prep$model_control
  )
  model_args$k <- k

  fit_args <- utils::modifyList(
    list(
      data = prep$x,
      epoch = 40L,
      batch_size = as.integer(min(1000L, nrow(prep$x))),
      normalize = TRUE,
      clip = 0,
      lr_anneal_factor = 4,
      lr_anneal_nonmono = 10
    ),
    control$fit
  )
  fit_args$data <- prep$x

  if (is.null(fit_args$epoch) || !is.numeric(fit_args$epoch) || length(fit_args$epoch) != 1L ||
      fit_args$epoch < 1 || fit_args$epoch != as.integer(fit_args$epoch)) {
    stop("For ETM, control$fit$epoch must be a single positive integer.", call. = FALSE)
  }
  if (is.null(fit_args$batch_size) || !is.numeric(fit_args$batch_size) || length(fit_args$batch_size) != 1L ||
      fit_args$batch_size < 1 || fit_args$batch_size != as.integer(fit_args$batch_size)) {
    stop("For ETM, control$fit$batch_size must be a single positive integer.", call. = FALSE)
  }
  fit_args$epoch <- as.integer(fit_args$epoch)
  fit_args$batch_size <- as.integer(min(fit_args$batch_size, nrow(prep$x)))

  model_object <- do.call(topicmodels.etm::ETM, model_args)
  optimizer_args <- utils::modifyList(
    list(params = model_object$parameters, lr = 0.005, weight_decay = 1.2e-06),
    control$optimizer
  )
  optimizer_args$params <- model_object$parameters
  optimizer <- do.call(torch::optim_adam, optimizer_args)
  fit_args$optimizer <- optimizer

  .fit_etm_model_original(model_object, fit_args)

  dtw <- stats::predict(
    model_object,
    newdata = prep$x,
    type = "topics",
    batch_size = as.integer(min(fit_args$batch_size, nrow(prep$x))),
    normalize = if ("normalize" %in% names(fit_args)) fit_args$normalize else TRUE
  )
  beta <- .etm_beta_tww(model_object, term_names = prep$term_names)
  tww <- beta$tww

  list(
    model_object = model_object,
    dtw = dtw,
    tww = tww,
    doc_ids = prep$doc_ids,
    term_names = beta$term_names,
    method = NULL,
    hyperparameters = .topic_hyperparameters_table(
      k = model_args$k,
      alpha = NA_real_,
      beta = NA_real_,
      sources = list(k = list(section = "argument", name = "k"))
    ),
    backend_control = .topic_backend_control(
      model = model_args,
      fit = fit_args,
      optimizer = optimizer_args
    )
  )
}

#' Fit an STM topic model
#'
#' Converts supported inputs to STM documents and fits stm with prevalence-covariate support.
#'
#' @keywords internal
#' @noRd
.fit_stm_topic_model <- function(x, k, control) {
  if (!requireNamespace("stm", quietly = TRUE)) {
    stop("Package 'stm' must be installed to use engine = 'stm'.", call. = FALSE)
  }

  prep <- .prepare_stm_input(x, control$fit)
  fit_args <- utils::modifyList(
    list(
      documents = prep$documents,
      vocab = prep$vocab,
      K = k,
      init.type = "Spectral",
      max.em.its = 75L,
      verbose = TRUE
    ),
    prep$fit_control
  )
  fit_args$documents <- prep$documents
  fit_args$vocab <- prep$vocab
  fit_args$K <- k

  model_object <- do.call(stm::stm, fit_args)
  tww <- .stm_tww_matrix(model_object)

  list(
    model_object = model_object,
    dtw = model_object$theta,
    tww = tww,
    doc_ids = prep$doc_ids,
    term_names = colnames(tww),
    method = NULL,
    hyperparameters = .topic_hyperparameters_table(
      k = .stm_topic_count(model_object),
      alpha = NA_real_,
      beta = NA_real_,
      sources = list(k = list(section = "model_object", name = "settings$dim$K"))
    ),
    backend_control = .topic_backend_control(
      model = list(),
      fit = fit_args,
      optimizer = list()
    )
  )
}

.prepare_stm_input <- function(x, fit_control) {
  x_dfm <- .as_topic_dfm(x)
  if (!quanteda::nfeat(x_dfm)) {
    stop("STM input must contain at least one feature.", call. = FALSE)
  }

  x_sparse <- methods::as(x_dfm, "dgCMatrix")
  if (!length(x_sparse@x) || any(x_sparse@x < 0) ||
      any(abs(x_sparse@x - round(x_sparse@x)) > .Machine$double.eps^0.5)) {
    stop("STM input must contain non-negative integer token counts.", call. = FALSE)
  }
  keep_terms <- Matrix::colSums(x_sparse) > 0
  if (!all(keep_terms)) {
    x_dfm <- x_dfm[, keep_terms]
    x_sparse <- methods::as(x_dfm, "dgCMatrix")
  }
  keep_docs <- Matrix::rowSums(x_sparse) > 0
  if (!all(keep_docs)) {
    stop("STM input must not contain empty documents.", call. = FALSE)
  }

  converted <- quanteda::convert(x_dfm, to = "stm")
  doc_ids <- quanteda::docnames(x_dfm)
  meta <- data.table::as.data.table(converted$meta)
  meta[, doc_id := doc_ids]
  data.table::setcolorder(meta, c("doc_id", setdiff(names(meta), "doc_id")))

  list(
    documents = .stm_documents_from_sparse(x_sparse),
    vocab = quanteda::featnames(x_dfm),
    doc_ids = doc_ids,
    fit_control = .normalize_stm_fit_control(fit_control, meta, doc_ids)
  )
}

.normalize_stm_fit_control <- function(fit_control, meta, doc_ids) {
  fit_control <- if (is.null(fit_control)) list() else as.list(fit_control)
  locked <- intersect(names(fit_control), c("documents", "vocab", "K"))
  if (length(locked)) {
    stop(
      sprintf(
        "For STM, control$fit entries are managed by NLPstudio and cannot be supplied: %s.",
        paste(locked, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if ("content" %in% names(fit_control) && !is.null(fit_control$content)) {
    stop(.stm_content_covariate_error(), call. = FALSE)
  }
  fit_control$content <- NULL

  if (!is.null(fit_control$prevalence)) {
    fit_control$data <- .stm_align_metadata(fit_control$data, meta, doc_ids)
  } else if (!is.null(fit_control$data)) {
    fit_control$data <- .stm_align_metadata(fit_control$data, meta, doc_ids)
  }
  fit_control
}

.stm_align_metadata <- function(data, meta, doc_ids) {
  if (is.null(data)) {
    if (ncol(meta) <= 1L) {
      return(data.frame(doc_id = doc_ids))
    }
    return(as.data.frame(meta))
  }

  dt <- data.table::as.data.table(data)
  if ("doc_id" %in% names(dt)) {
    if (anyDuplicated(dt$doc_id)) {
      stop("STM control$fit$data must not contain duplicate 'doc_id' values.", call. = FALSE)
    }
    idx <- match(doc_ids, as.character(dt$doc_id))
    if (anyNA(idx)) {
      stop("STM control$fit$data must contain one row for each input document ID.", call. = FALSE)
    }
    dt <- dt[idx]
    if (!identical(as.character(dt$doc_id), as.character(doc_ids))) {
      stop("STM control$fit$data could not be aligned to the input document IDs.", call. = FALSE)
    }
  } else if (nrow(dt) != length(doc_ids)) {
    stop(
      "STM control$fit$data must either have one row per input document in order or contain a 'doc_id' column.",
      call. = FALSE
    )
  }
  as.data.frame(dt)
}

.stm_documents_from_sparse <- function(x) {
  x <- methods::as(x, "dgCMatrix")
  coo <- Matrix::summary(methods::as(x, "TsparseMatrix"))
  lapply(seq_len(nrow(x)), function(i) {
    idx <- which(coo$i == i)
    if (!length(idx)) {
      stop("STM input must not contain empty documents.", call. = FALSE)
    }
    rbind(as.integer(coo$j[idx]), as.integer(round(coo$x[idx])))
  })
}

.stm_content_covariate_error <- function() {
  paste(
    "STM content covariates are not supported in v0.9.4 because they imply",
    "covariate-specific topic-word distributions, while NLPstudio currently",
    "standardizes one TWW matrix per fit."
  )
}

#' Fit an ETM model with drop-safe train/test splits
#'
#' Calls `fit_original()` with explicit `drop = FALSE` subsetting so one-row
#' held-out splits remain `dgCMatrix` objects for `topicmodels.etm`.
#'
#' @keywords internal
#' @noRd
.fit_etm_model_original <- function(model_object, fit_args) {
  data <- fit_args$data
  if (inherits(data, "sparseMatrix")) {
    data <- data[Matrix::rowSums(data) > 0, , drop = FALSE]
  }
  data <- methods::as(data, "dgCMatrix")
  if (nrow(data) < 3L) {
    stop("ETM fitting requires at least 3 non-empty documents.", call. = FALSE)
  }

  idx <- .etm_train_test_indices(nrow(data), train_pct = 0.7)
  as_tokencounts <- utils::getFromNamespace("as_tokencounts", "topicmodels.etm")

  original_args <- list(
    data = as_tokencounts(data[idx$train, , drop = FALSE]),
    test1 = as_tokencounts(data[idx$test1, , drop = FALSE]),
    test2 = as_tokencounts(data[idx$test2, , drop = FALSE]),
    optimizer = fit_args$optimizer,
    epoch = fit_args$epoch,
    batch_size = fit_args$batch_size,
    normalize = fit_args$normalize,
    clip = fit_args$clip,
    lr_anneal_factor = fit_args$lr_anneal_factor,
    lr_anneal_nonmono = fit_args$lr_anneal_nonmono
  )

  loss_evolution <- do.call(model_object$fit_original, original_args)
  model_object$loss_fit <- loss_evolution
  invisible(loss_evolution)
}

#' Create ETM train/test indices with non-empty held-out halves
#'
#' Mirrors the backend 70/30 split while ensuring both held-out halves contain
#' at least one document and preserving integer row indices.
#'
#' @keywords internal
#' @noRd
.etm_train_test_indices <- function(n, train_pct = 0.7) {
  if (!is.numeric(n) || length(n) != 1L || n < 3L || n != as.integer(n)) {
    stop("n must be a single integer greater than or equal to 3.", call. = FALSE)
  }
  n <- as.integer(n)
  test_n <- max(2L, as.integer(round(n * (1 - train_pct))))
  test_n <- min(test_n, n - 1L)

  idx <- seq_len(n)
  test <- sort(sample(idx, size = test_n, replace = FALSE))
  test1_n <- max(1L, as.integer(round(length(test) / 2)))
  test1_n <- min(test1_n, length(test) - 1L)

  test1 <- sort(sample(test, size = test1_n, replace = FALSE))
  test2 <- sort(setdiff(test, test1))
  train <- sort(setdiff(idx, test))

  list(train = train, test1 = test1, test2 = test2)
}

# -----------------------------------------------------------------------------
# Prediction helpers
# -----------------------------------------------------------------------------
#
# Vocabulary alignment and backend-specific prediction helpers used by `predict_topic_model()`.

#' Normalize prediction control lists
#'
#' Validates optional backend controls forwarded during topic prediction.
#'
#' @keywords internal
#' @noRd
.normalize_prediction_control <- function(control) {
  if (is.null(control) || !length(control)) {
    return(list())
  }
  if (!is.list(control)) {
    stop("control must be a named list of backend-specific prediction arguments.", call. = FALSE)
  }
  nms <- names(control)
  if (is.null(nms) || any(nms == "")) {
    stop("control must be a named list of backend-specific prediction arguments.", call. = FALSE)
  }
  control
}

#' Predict backend document-topic weights
#'
#' Dispatches to the fitted backend to produce a document-topic matrix for aligned new data.
#'
#' @keywords internal
#' @noRd
.predict_topic_matrix <- function(fit, newdata_aligned, control) {
  out <- switch(
    fit$engine,
    text2vec = {
      args <- utils::modifyList(list(x = newdata_aligned$sparse), control)
      args$x <- newdata_aligned$sparse
      do.call(fit$model_object$transform, args)
    },
    topicmodels = {
      args <- utils::modifyList(
        list(object = fit$model_object, newdata = .as_topicmodels_input(newdata_aligned$dfm)),
        control
      )
      args$object <- fit$model_object
      args$newdata <- .as_topicmodels_input(newdata_aligned$dfm)
      do.call(topicmodels::posterior, args)$topics
    },
    seededlda = .predict_seededlda_topic_matrix(
      fit = fit,
      newdata_dfm = newdata_aligned$dfm,
      control = control
    ),
    `topicmodels.etm` = {
      args <- utils::modifyList(
        list(object = fit$model_object, newdata = newdata_aligned$sparse, type = "topics"),
        control
      )
      args$object <- fit$model_object
      args$newdata <- newdata_aligned$sparse
      args$type <- "topics"
      do.call(stats::predict, args)
    },
    stm = .predict_stm_topic_matrix(
      fit = fit,
      newdata_aligned = newdata_aligned,
      control = control
    )
  )

  .dtw_matrix_from_matrix(out, doc_ids = newdata_aligned$doc_ids)
}

.predict_stm_topic_matrix <- function(fit, newdata_aligned, control) {
  if (.stm_has_prevalence(fit)) {
    stop(
      "STM prediction for prevalence-covariate fits is not supported in v0.9.4; refit without prevalence or use stm::fitNewDocuments() directly with explicit covariate handling.",
      call. = FALSE
    )
  }
  if (!requireNamespace("stm", quietly = TRUE)) {
    stop("Package 'stm' must be installed to predict with engine = 'stm'.", call. = FALSE)
  }
  reserved <- intersect(
    names(control),
    c("model", "documents", "newData", "origData", "prevalence", "designMatrix", "betaIndex")
  )
  if (length(reserved)) {
    stop(
      sprintf(
        "For STM prediction, control entries are managed by NLPstudio and cannot be supplied: %s.",
        paste(reserved, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  args <- utils::modifyList(
    list(
      model = fit$model_object,
      documents = .stm_documents_from_sparse(newdata_aligned$sparse),
      verbose = FALSE
    ),
    control
  )
  args$model <- fit$model_object
  args$documents <- .stm_documents_from_sparse(newdata_aligned$sparse)
  do.call(stm::fitNewDocuments, args)$theta
}

#' Predict seededlda document-topic weights
#'
#' Uses seededlda update semantics to infer document-topic weights for new documents.
#'
#' @keywords internal
#' @noRd
.predict_seededlda_topic_matrix <- function(fit, newdata_dfm, control) {
  predict_args <- utils::modifyList(
    list(x = newdata_dfm, model = fit$model_object),
    control
  )
  predict_args$x <- newdata_dfm
  predict_args$model <- fit$model_object

  if (fit$model %in% c("lda", "seededlda") &&
      !"update_model" %in% names(predict_args)) {
    predict_args$update_model <- FALSE
  }

  fit_fun <- switch(
    fit$model,
    lda = seededlda::textmodel_lda,
    seqlda = seededlda::textmodel_seqlda,
    seededlda = seededlda::textmodel_lda
  )

  predicted <- withCallingHandlers(
    do.call(fit_fun, predict_args),
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("overwritten by the fitted model", msg, fixed = TRUE) ||
          grepl("gamma has no effect when docid are all unique", msg, fixed = TRUE)) {
        tryInvokeRestart("muffleWarning")
      }
    }
  )

  predicted$theta
}

#' Align topic-model input to a vocabulary
#'
#' Converts new or training data to sparse/dfm forms with columns ordered to the fitted vocabulary.
#'
#' @keywords internal
#' @noRd
.align_topic_input_to_vocab <- function(x, vocab, vocab_label = "target vocabulary",
                                        context = "vocabulary alignment") {
  x_dfm <- .as_topic_dfm(x)
  vocab <- as.character(vocab)
  if (!length(vocab)) {
    stop("vocab must contain at least one term.", call. = FALSE)
  }

  input_terms <- quanteda::featnames(x_dfm)
  dropped_terms <- setdiff(input_terms, vocab)
  if (length(dropped_terms)) {
    warning(
      sprintf(
        "Dropping %d terms that were not found in the %s.",
        length(dropped_terms),
        vocab_label
      ),
      call. = FALSE
    )
  }

  x_dfm <- quanteda::dfm_match(x_dfm, features = vocab)
  keep <- Matrix::rowSums(methods::as(x_dfm, "dgCMatrix")) > 0

  if (!all(keep)) {
    warning(
      sprintf(
        "Dropping %d documents with zero counts after %s.",
        sum(!keep),
        context
      ),
      call. = FALSE
    )
    x_dfm <- x_dfm[keep, ]
  }

  if (!quanteda::ndoc(x_dfm)) {
    stop(sprintf("No documents remain after %s.", context), call. = FALSE)
  }

  x_sparse <- methods::as(x_dfm, "dgCMatrix")
  doc_ids <- quanteda::docnames(x_dfm)
  rownames(x_sparse) <- doc_ids
  colnames(x_sparse) <- vocab

  list(
    dfm = x_dfm,
    sparse = x_sparse,
    doc_ids = doc_ids,
    term_names = vocab
  )
}

#' Prepare ETM input data
#'
#' Builds the ETM data matrix and model-control metadata while preserving surviving document and term IDs.
#'
#' @keywords internal
#' @noRd
.prepare_etm_input <- function(x, model_control) {
  x_sparse <- .as_topic_dgCMatrix(x)
  doc_ids <- .matrix_doc_ids(x_sparse, fallback = rownames(x_sparse))
  rownames(x_sparse) <- doc_ids

  if (is.null(colnames(x_sparse))) {
    stop("ETM requires term names on the input matrix.", call. = FALSE)
  }

  embeddings <- model_control$embeddings
  if (is.null(embeddings)) {
    stop("For ETM, control$model$embeddings must be supplied as an integer dimension or embedding matrix.", call. = FALSE)
  }

  if (is.matrix(embeddings)) {
    if (is.null(rownames(embeddings))) {
      stop("Pretrained ETM embeddings must have rownames.", call. = FALSE)
    }
    storage.mode(embeddings) <- "double"
    if (!is.null(model_control$vocab) &&
        !identical(as.character(model_control$vocab), rownames(embeddings))) {
      stop(
        "When ETM embeddings are supplied as a matrix, control$model$vocab must match the embedding rownames or be omitted.",
        call. = FALSE
      )
    }

    common_terms <- rownames(embeddings)[rownames(embeddings) %in% colnames(x_sparse)]
    if (!length(common_terms)) {
      stop("No overlap was found between the ETM embedding vocabulary and the input terms.", call. = FALSE)
    }

    aligned <- .align_topic_input_to_vocab(
      x,
      vocab = common_terms,
      vocab_label = "ETM embedding vocabulary",
      context = "ETM vocabulary alignment"
    )
    x_sparse <- aligned$sparse
    model_control$embeddings <- embeddings[common_terms, , drop = FALSE]
    model_control$vocab <- common_terms
    doc_ids <- aligned$doc_ids
  } else {
    if (!is.numeric(embeddings) || length(embeddings) != 1L ||
        embeddings < 1 || embeddings != as.integer(embeddings)) {
      stop(
        "For ETM, control$model$embeddings must be either a single positive integer or a numeric matrix.",
        call. = FALSE
      )
    }

    vocab <- model_control$vocab
    if (is.null(vocab)) {
      vocab <- colnames(x_sparse)
      aligned <- .align_topic_input_to_vocab(
        x,
        vocab = vocab,
        vocab_label = "ETM learned-embedding vocabulary",
        context = "ETM vocabulary alignment"
      )
      x_sparse <- aligned$sparse
      doc_ids <- aligned$doc_ids
    } else {
      vocab <- as.character(vocab)
      if (length(vocab) != ncol(x_sparse)) {
        stop(
          "For learned ETM embeddings, control$model$vocab must have length equal to the number of input terms.",
          call. = FALSE
        )
      }

      idx <- match(vocab, colnames(x_sparse))
      if (anyNA(idx) || anyDuplicated(idx)) {
        stop(
          "For learned ETM embeddings, control$model$vocab must match the input terms exactly, up to ordering.",
          call. = FALSE
        )
      }
      aligned <- .align_topic_input_to_vocab(
        x,
        vocab = vocab,
        vocab_label = "ETM learned-embedding vocabulary",
        context = "ETM vocabulary alignment"
      )
      x_sparse <- aligned$sparse
      doc_ids <- aligned$doc_ids
    }

    model_control$embeddings <- as.integer(embeddings)
    model_control$vocab <- vocab
  }

  list(
    x = x_sparse,
    doc_ids = doc_ids,
    term_names = colnames(x_sparse),
    model_control = model_control
  )
}

# -----------------------------------------------------------------------------
# DTW/TWW helpers
# -----------------------------------------------------------------------------
#
# Input coercion, document/topic identifiers, matrix normalization, and DTW/TWW table conversion.

#' Extract a standardized DTW table
#'
#' Converts supported topic-model objects or existing tables into the package DTW table format.
#'
#' @keywords internal
#' @noRd
.extract_dtw_table <- function(x) {
  if (inherits(x, "nlp_topic_fit")) {
    if (!is.null(x$dtw)) {
      return(.dtw_dt_from_matrix(x$dtw))
    }
    if (identical(x$engine, "text2vec")) {
      stop(
        "This text2vec fit does not contain cached DTW. Refit with return_dtw = TRUE.",
        call. = FALSE
      )
    }
    if (identical(x$engine, "topicmodels.etm")) {
      stop(
        "This ETM fit does not contain cached DTW. Refit with return_dtw = TRUE.",
        call. = FALSE
      )
    }
    return(.extract_dtw_table(x$model_object))
  }

  if (.looks_like_dtw_table(x)) {
    return(.coerce_existing_dtw_table(x))
  }

  if (methods::is(x, "TopicModel")) {
    return(.dtw_dt_from_matrix(x@gamma, doc_ids = .topicmodels_doc_ids(x)))
  }

  if (inherits(x, "textmodel")) {
    return(.dtw_dt_from_matrix(x$theta, doc_ids = .seededlda_doc_ids(x)))
  }

  if (.is_stm_object(x)) {
    return(.dtw_dt_from_matrix(x$theta, doc_ids = .stm_doc_ids(x)))
  }

  if (.is_etm_object(x)) {
    stop(
      "Raw ETM objects do not retain fitted DTW in a stable package interface. Use fit_topic_model(..., return_dtw = TRUE).",
      call. = FALSE
    )
  }

  if (inherits(x, "WarpLDA")) {
    stop(
      "Raw text2vec WarpLDA objects do not retain DTW. Use fit_topic_model(..., return_dtw = TRUE).",
      call. = FALSE
    )
  }

  stop("x is an unrecognized object for DTW extraction.")
}

#' Extract a standardized TWW table
#'
#' Converts supported topic-model objects or existing tables into the package TWW table format.
#'
#' @keywords internal
#' @noRd
.extract_tww_table <- function(x) {
  if (inherits(x, "nlp_topic_fit")) {
    if (!is.null(x$tww)) {
      return(.tww_dt_from_matrix(x$tww))
    }
    return(.extract_tww_table(x$model_object))
  }

  if (.looks_like_tww_table(x)) {
    return(.coerce_existing_tww_table(x))
  }

  if (methods::is(x, "TopicModel")) {
    return(.tww_dt_from_matrix(x@beta, term_names = x@terms, log_scale = TRUE))
  }

  if (inherits(x, "textmodel")) {
    return(.tww_dt_from_matrix(x$phi, term_names = colnames(x$phi)))
  }

  if (.is_stm_object(x)) {
    return(.tww_dt_from_matrix(.stm_tww_matrix(x), term_names = x$vocab))
  }

  if (.is_etm_object(x)) {
    beta <- .etm_beta_tww(x)
    return(.tww_dt_from_matrix(
      beta$tww,
      term_names = if (!is.null(beta$term_names)) beta$term_names else .stored_topic_vocab(x)
    ))
  }

  if (inherits(x, "WarpLDA")) {
    if (is.null(x$topic_word_distribution)) {
      stop(
        "Raw text2vec WarpLDA objects do not contain recoverable TWW. Use fit_topic_model(..., return_tww = TRUE) or convert a legacy object with saved phi.",
        call. = FALSE
      )
    }
    return(.tww_dt_from_matrix(
      x$topic_word_distribution,
      term_names = colnames(x$topic_word_distribution)
    ))
  }

  stop("x is an unrecognized object for TWW extraction.")
}

#' Coerce input to a topic sparse matrix
#'
#' Converts supported topic-model inputs to `dgCMatrix` without changing term order.
#'
#' @keywords internal
#' @noRd
.as_topic_dgCMatrix <- function(x) {
  if (methods::is(x, "dgCMatrix")) {
    return(x)
  }
  if (inherits(x, "dfm")) {
    return(methods::as(x, "dgCMatrix"))
  }
  if (methods::is(x, "DocumentTermMatrix")) {
    return(methods::as(quanteda::as.dfm(x), "dgCMatrix"))
  }

  stop(
    "x must be a dgCMatrix, quanteda dfm, or DocumentTermMatrix for topic modeling.",
    call. = FALSE
  )
}

#' Coerce input to a topic dfm
#'
#' Converts supported topic-model inputs to quanteda dfm for backends that require it.
#'
#' @keywords internal
#' @noRd
.as_topic_dfm <- function(x) {
  if (inherits(x, "dfm")) {
    return(x)
  }
  if (methods::is(x, "dgCMatrix")) {
    return(quanteda::as.dfm(x))
  }
  if (methods::is(x, "DocumentTermMatrix")) {
    return(quanteda::as.dfm(x))
  }

  stop(
    "x must be a dgCMatrix, quanteda dfm, or DocumentTermMatrix for topic modeling.",
    call. = FALSE
  )
}

#' Coerce input for topicmodels
#'
#' Converts supported topic-model inputs to the DocumentTermMatrix format expected by topicmodels.
#'
#' @keywords internal
#' @noRd
.as_topicmodels_input <- function(x) {
  if (methods::is(x, "DocumentTermMatrix")) {
    return(x)
  }

  quanteda::convert(.as_topic_dfm(x), to = "topicmodels")
}

#' Format package topic identifiers
#'
#' Creates canonical `Topic###` identifiers for standardized DTW/TWW outputs.
#'
#' @keywords internal
#' @noRd
.topic_ids <- function(n) {
  sprintf("Topic%03d", seq_len(n))
}

#' Recover matrix document identifiers
#'
#' Returns row names or fallback sequential IDs as character document IDs.
#'
#' @keywords internal
#' @noRd
.matrix_doc_ids <- function(x, fallback = NULL) {
  doc_ids <- rownames(x)
  if (is.null(doc_ids)) {
    doc_ids <- fallback
  }
  if (is.null(doc_ids)) {
    doc_ids <- as.character(seq_len(nrow(x)))
  }
  as.character(doc_ids)
}

#' Normalize a DTW matrix
#'
#' Converts backend document-topic weights to a numeric matrix with document IDs and `Topic###` columns.
#'
#' @keywords internal
#' @noRd
.dtw_matrix_from_matrix <- function(x, doc_ids = NULL) {
  mat <- as.matrix(x)
  rownames(mat) <- .matrix_doc_ids(mat, fallback = doc_ids)
  colnames(mat) <- .topic_ids(ncol(mat))
  storage.mode(mat) <- "double"
  mat
}

#' Normalize a TWW matrix
#'
#' Converts backend topic-word weights to a numeric matrix with `Topic###` rows and term columns.
#'
#' @keywords internal
#' @noRd
.tww_matrix_from_matrix <- function(x, term_names = NULL, log_scale = FALSE) {
  mat <- as.matrix(x)
  if (log_scale) {
    mat <- exp(mat)
  }
  if (is.null(term_names)) {
    term_names <- colnames(mat)
  }
  if (is.null(term_names)) {
    term_names <- paste0("term", seq_len(ncol(mat)))
  }

  rownames(mat) <- .topic_ids(nrow(mat))
  colnames(mat) <- term_names
  storage.mode(mat) <- "double"
  mat
}

#' Normalize ETM beta output to topic-by-term orientation
#'
#' `topicmodels.etm` exposes beta as term-by-topic; NLPstudio stores TWW matrices
#' as topic-by-term. This helper also carries the backend vocabulary forward.
#'
#' @keywords internal
#' @noRd
.etm_beta_tww <- function(model_object, term_names = NULL) {
  beta <- as.matrix(model_object, type = "beta")
  beta <- as.matrix(beta)

  if (is.null(term_names)) {
    term_names <- rownames(beta)
  }
  if (is.null(term_names)) {
    term_names <- colnames(beta)
  }
  if (!is.null(term_names)) {
    term_names <- as.character(term_names)
    if (nrow(beta) == length(term_names) && ncol(beta) != length(term_names)) {
      beta <- t(beta)
    }
  }

  list(tww = beta, term_names = term_names)
}

#' Convert DTW matrix to data.table
#'
#' Builds a standardized DTW table and adds dominant-topic summary columns.
#'
#' @keywords internal
#' @noRd
.dtw_dt_from_matrix <- function(x, doc_ids = NULL) {
  mat <- .dtw_matrix_from_matrix(x, doc_ids = doc_ids)
  out <- data.table::data.table(doc_id = rownames(mat))
  out <- cbind(out, data.table::as.data.table(mat))
  .add_topic_max_columns(out)
}

#' Convert TWW matrix to data.table
#'
#' Builds a standardized TWW table from a normalized topic-word matrix.
#'
#' @keywords internal
#' @noRd
.tww_dt_from_matrix <- function(x, term_names = NULL, log_scale = FALSE) {
  mat <- .tww_matrix_from_matrix(x, term_names = term_names, log_scale = log_scale)
  out <- data.table::data.table(topic_id = rownames(mat))
  cbind(out, data.table::as.data.table(mat))
}

#' Coerce an existing DTW table
#'
#' Normalizes user-supplied DTW-like tables to the package column contract.
#'
#' @keywords internal
#' @noRd
.coerce_existing_dtw_table <- function(x) {
  dt <- data.table::copy(data.table::as.data.table(x))

  if ("rn" %in% names(dt) && !"doc_id" %in% names(dt)) {
    data.table::setnames(dt, "rn", "doc_id")
  }
  if (!"doc_id" %in% names(dt)) {
    stop("DTW tables must contain a 'doc_id' column.", call. = FALSE)
  }

  topic_cols <- .find_topic_columns(dt, id_col = "doc_id")
  if (!length(topic_cols)) {
    stop("No topic columns were found in the supplied DTW table.", call. = FALSE)
  }

  non_topic_cols <- setdiff(names(dt), c("doc_id", topic_cols))
  data.table::setcolorder(dt, c("doc_id", topic_cols, non_topic_cols))
  data.table::setnames(dt, topic_cols, .topic_ids(length(topic_cols)))

  old_summary_cols <- intersect(c("topic_max_id", "topic_max_int", "topic_max_value"), names(dt))
  if (length(old_summary_cols)) {
    dt[, (old_summary_cols) := NULL]
  }
  .add_topic_max_columns(dt)
}

#' Coerce an existing TWW table
#'
#' Normalizes user-supplied TWW-like tables to the package column contract.
#'
#' @keywords internal
#' @noRd
.coerce_existing_tww_table <- function(x) {
  dt <- data.table::copy(data.table::as.data.table(x))

  if (!"topic_id" %in% names(dt)) {
    stop("TWW tables must contain a 'topic_id' column.", call. = FALSE)
  }

  term_cols <- setdiff(names(dt), "topic_id")
  data.table::setnames(dt, "topic_id", "topic_id")
  dt[, topic_id := .topic_ids(.N)]
  data.table::setcolorder(dt, c("topic_id", term_cols))
  dt[]
}

#' Find standardized topic columns
#'
#' Identifies and orders numeric topic columns while excluding document-summary columns.
#'
#' @keywords internal
#' @noRd
.find_topic_columns <- function(x, id_col) {
  nms <- names(x)
  topic_cols <- grep("^Topic\\d+$", nms, value = TRUE)
  if (length(topic_cols)) {
    ord <- order(as.integer(sub("^Topic", "", topic_cols)))
    return(topic_cols[ord])
  }

  candidate_cols <- setdiff(nms, c(id_col, "topic_max_id", "topic_max_int", "topic_max_value"))
  if (length(candidate_cols) &&
      all(vapply(x[, candidate_cols, with = FALSE], is.numeric, logical(1)))) {
    return(candidate_cols)
  }

  character()
}

#' List DTW output columns
#'
#' Returns topic and dominant-topic summary columns that belong to the DTW output surface.
#'
#' @keywords internal
#' @noRd
.dtw_output_columns <- function(x) {
  topic_cols <- .find_topic_columns(x, id_col = "doc_id")
  intersect(c(topic_cols, "topic_max_id", "topic_max_int", "topic_max_value"), names(x))
}

#' List representative-candidate output columns
#'
#' Returns DTW output columns plus candidate-band and within-topic rank columns.
#'
#' @keywords internal
#' @noRd
.representative_candidates_output_columns <- function(x) {
  intersect(
    c(.dtw_output_columns(x), "candidate_band", "topic_rank"),
    names(x)
  )
}

#' Recover topicmodels document IDs
#'
#' Extracts document IDs from a topicmodels fit with sequential fallback.
#'
#' @keywords internal
#' @noRd
.topicmodels_doc_ids <- function(x) {
  doc_ids <- rownames(x@gamma)
  if (is.null(doc_ids)) {
    doc_ids <- x@documents
  }
  if (is.null(doc_ids)) {
    doc_ids <- as.character(seq_len(nrow(x@gamma)))
  }
  as.character(doc_ids)
}

#' Recover seededlda document IDs
#'
#' Extracts document IDs from a seededlda fit with model-data and sequential fallbacks.
#'
#' @keywords internal
#' @noRd
.seededlda_doc_ids <- function(x) {
  doc_ids <- rownames(x$theta)
  if (is.null(doc_ids) && !is.null(x$data)) {
    doc_ids <- quanteda::docnames(x$data)
  }
  if (is.null(doc_ids)) {
    doc_ids <- as.character(seq_len(nrow(x$theta)))
  }
  doc_ids
}

# -----------------------------------------------------------------------------
# Metadata helpers
# -----------------------------------------------------------------------------
#
# Docvar/doc_data normalization, metadata joins, output ordering, and representative-candidate utilities.

#' Filter metadata from existing DTW tables
#'
#' Keeps only requested metadata columns when an existing DTW table is used as input.
#'
#' @keywords internal
#' @noRd
.filter_existing_dtw_metadata <- function(x, docvars, include_text) {
  output_cols <- .dtw_output_columns(x)
  keep <- c("doc_id", output_cols)
  if (docvars) {
    keep <- c(keep, setdiff(names(x), c(keep, "text")))
  }
  if (include_text && "text" %in% names(x)) {
    keep <- c(keep, "text")
  }

  x[, intersect(keep, names(x)), with = FALSE]
}

#' Drop stored docvars from output
#'
#' Removes columns that came from stored docvars when the caller requested no docvars.
#'
#' @keywords internal
#' @noRd
.drop_stored_docvars <- function(x, source) {
  stored <- .stored_docvars_table(source, doc_ids = x$doc_id)
  if (is.null(stored)) {
    return(x[])
  }

  drop_cols <- intersect(setdiff(names(stored), "doc_id"), names(x))
  if (length(drop_cols)) {
    x[, (drop_cols) := NULL]
  }
  x[]
}

#' Order document-topic output columns
#'
#' Places document IDs, metadata, function outputs, and optional text in a stable order.
#'
#' @keywords internal
#' @noRd
.order_document_topic_output <- function(x, output_cols) {
  text_cols <- intersect("text", names(x))
  metadata_cols <- setdiff(names(x), c("doc_id", output_cols, text_cols))
  data.table::setcolorder(
    x,
    c("doc_id", metadata_cols, output_cols, text_cols)
  )
  x[]
}

#' Order DTW output columns
#'
#' Applies the standard document-topic column order to DTW outputs.
#'
#' @keywords internal
#' @noRd
.order_dtw_output <- function(x) {
  .order_document_topic_output(x, .dtw_output_columns(x))
}

#' Order representative-candidate columns
#'
#' Applies the standard document-topic column order to representative-candidate outputs.
#'
#' @keywords internal
#' @noRd
.order_representative_candidates_output <- function(x) {
  .order_document_topic_output(x, .representative_candidates_output_columns(x))
}

#' Add dominant-topic summary columns
#'
#' Adds topic ID, integer topic number, and weight for the highest-weight topic in each document.
#'
#' @keywords internal
#' @noRd
.add_topic_max_columns <- function(x) {
  topic_cols <- .find_topic_columns(x, id_col = "doc_id")
  topic_mat <- as.matrix(x[, topic_cols, with = FALSE])
  max_idx <- max.col(topic_mat, ties.method = "first")
  topic_ints <- as.integer(sub("^Topic", "", topic_cols))
  x[, topic_max_id := topic_cols[max_idx]]
  x[, topic_max_int := topic_ints[max_idx]]
  x[, topic_max_value := topic_mat[cbind(seq_len(nrow(topic_mat)), max_idx)]]
  x[]
}

#' Extract docvars from model input
#'
#' Builds a compact document-variable table aligned to fitted document IDs.
#'
#' @keywords internal
#' @noRd
.docvars_table_from_input <- function(x, doc_ids) {
  doc_ids <- as.character(doc_ids)
  base <- data.table::data.table(doc_id = doc_ids)

  if (quanteda::is.corpus(x)) {
    meta <- data.table::data.table(doc_id = quanteda::docnames(x))
    x_docvars <- quanteda::docvars(x)
    if (ncol(x_docvars)) {
      meta <- cbind(meta, data.table::as.data.table(x_docvars))
    }
    return(.bind_topic_metadata(base, meta, overwrite = TRUE))
  }

  if (inherits(x, "dfm")) {
    meta <- data.table::data.table(doc_id = quanteda::docnames(x))
    x_docvars <- quanteda::docvars(x)
    if (ncol(x_docvars)) {
      meta <- cbind(meta, data.table::as.data.table(x_docvars))
    }
    return(.bind_topic_metadata(base, meta, overwrite = TRUE))
  }

  base[]
}

#' Normalize sidecar document data
#'
#' Converts corpus or tabular sidecar data to a `doc_id`-keyed metadata table.
#'
#' @keywords internal
#' @noRd
.normalize_doc_data_table <- function(data, include_text, doc_id_col = "doc_id",
                                      text_col = "text", arg_name = "doc_data") {
  if (is.null(data)) {
    return(NULL)
  }

  if (quanteda::is.corpus(data)) {
    meta <- data.table::data.table(doc_id = quanteda::docnames(data))
    x_docvars <- quanteda::docvars(data)
    if (ncol(x_docvars)) {
      meta <- cbind(meta, data.table::as.data.table(x_docvars))
    }
    if (include_text) {
      meta[, text := as.character(data)]
    }
    return(meta[])
  }

  if (data.table::is.data.table(data) || is.data.frame(data)) {
    meta <- data.table::as.data.table(data)
    if (!doc_id_col %in% names(meta)) {
      stop(sprintf("%s must contain a '%s' column.", arg_name, doc_id_col), call. = FALSE)
    }
    if (doc_id_col != "doc_id") {
      data.table::setnames(meta, doc_id_col, "doc_id")
    }
    if (include_text) {
      if (text_col %in% names(meta) && text_col != "text") {
        data.table::setnames(meta, text_col, "text")
      }
    } else if (text_col %in% names(meta)) {
      meta[, (text_col) := NULL]
    }
    return(meta[])
  }

  stop(
    sprintf("%s must be a corpus, data.frame, or data.table.", arg_name),
    call. = FALSE
  )
}

#' Resolve stored docvars for output
#'
#' Returns stored document variables aligned to requested document IDs when available.
#'
#' @keywords internal
#' @noRd
.stored_docvars_table <- function(x, doc_ids) {
  if (inherits(x, "nlp_topic_fit")) {
    if (is.null(x$docvars)) {
      return(NULL)
    }
    return(.bind_topic_metadata(
      data.table::data.table(doc_id = as.character(doc_ids)),
      x$docvars,
      overwrite = TRUE
    ))
  }

  if (inherits(x, "textmodel") && !is.null(x$data) &&
      (inherits(x$data, "dfm") || quanteda::is.corpus(x$data))) {
    return(.docvars_table_from_input(x$data, doc_ids = doc_ids))
  }

  NULL
}

#' Resolve sidecar document data for output
#'
#' Chooses explicit or stored sidecar data and normalizes it for metadata joins.
#'
#' @keywords internal
#' @noRd
.resolved_doc_data_table <- function(x, doc_data, include_text,
                                     doc_id_col, text_col) {
  source <- doc_data
  explicit_override <- !is.null(doc_data)
  if (is.null(source) && inherits(x, "nlp_topic_fit")) {
    source <- x$doc_data
  }
  if (is.null(source) && inherits(x, "textmodel") &&
      !is.null(x$data) &&
      (quanteda::is.corpus(x$data) ||
       data.table::is.data.table(x$data) ||
       is.data.frame(x$data))) {
    source <- x$data
  }
  if (is.null(source)) {
    return(NULL)
  }

  .normalize_doc_data_table(
    source,
    include_text = include_text,
    doc_id_col = if (explicit_override) doc_id_col else "doc_id",
    text_col = if (explicit_override) text_col else "text",
    arg_name = "doc_data"
  )
}

#' Join metadata to topic output
#'
#' Binds doc_id-keyed metadata to DTW-like output while protecting topic columns from overwrite.
#'
#' @keywords internal
#' @noRd
.bind_topic_metadata <- function(dtw, meta, overwrite = FALSE) {
  if (is.null(meta)) {
    return(dtw[])
  }

  meta <- data.table::as.data.table(meta)
  meta <- meta[!duplicated(doc_id)]
  extra_cols <- setdiff(names(meta), "doc_id")
  protected <- c("doc_id", .find_topic_columns(dtw, id_col = "doc_id"), "topic_max_id", "topic_max_int", "topic_max_value")
  conflicts <- intersect(extra_cols, names(dtw))
  protected_conflicts <- intersect(conflicts, protected)
  if (length(protected_conflicts)) {
    warning(
      sprintf(
        "Dropping metadata columns that conflict with DTW columns: %s",
        paste(protected_conflicts, collapse = ", ")
      ),
      call. = FALSE
    )
    extra_cols <- setdiff(extra_cols, protected_conflicts)
    conflicts <- setdiff(conflicts, protected_conflicts)
  }
  if (length(conflicts)) {
    if (overwrite) {
      dtw[, (conflicts) := NULL]
    } else {
      warning(
        sprintf(
          "Dropping metadata columns that conflict with DTW columns: %s",
          paste(conflicts, collapse = ", ")
        ),
        call. = FALSE
      )
      extra_cols <- setdiff(extra_cols, conflicts)
    }
  }

  if (!length(extra_cols)) {
    return(dtw[])
  }

  matched <- meta[match(dtw$doc_id, meta$doc_id), extra_cols, with = FALSE]
  cbind(dtw, matched)
}

#' Resolve requested topic selectors
#'
#' Maps numeric topic indices or `Topic###` identifiers to available topic IDs.
#'
#' @keywords internal
#' @noRd
.resolve_topic_selector <- function(topic_ids, topics) {
  if (is.null(topics)) {
    return(topic_ids)
  }

  if (is.numeric(topics)) {
    if (anyNA(topics) || any(topics < 1) || any(topics != floor(topics))) {
      stop("topics must contain positive integer indices or Topic### identifiers.")
    }
    if (any(topics > length(topic_ids))) {
      stop("Some requested topics are not available.")
    }
    return(topic_ids[as.integer(topics)])
  }

  if (is.character(topics)) {
    if (!all(topics %in% topic_ids)) {
      stop("Some requested topics are not available.")
    }
    return(topics)
  }

  stop("topics must be NULL, numeric topic indices, or Topic### identifiers.")
}

#' Assign representative-candidate bands
#'
#' Uses quantile cuts with deterministic rank fallback to label candidates within each topic.
#'
#' @keywords internal
#' @noRd
.assign_candidate_bands <- function(values, quantile_probs, labels) {
  cuts <- stats::quantile(values, probs = quantile_probs, names = FALSE, na.rm = TRUE)
  target_bands <- min(length(labels), length(values))

  if (length(unique(cuts)) == length(cuts)) {
    value_bands <- as.character(cut(
      values,
      breaks = c(-Inf, cuts, Inf),
      labels = labels,
      include.lowest = TRUE,
      right = TRUE
    ))

    if (
      length(unique(stats::na.omit(value_bands))) >= target_bands) {
      return(value_bands)
    }
  }

  ord <- order(values, seq_along(values))
  rank_position <- integer(length(values))
  rank_position[ord] <- seq_along(values)
  pct <- (rank_position - 0.5) / length(values)
  as.character(cut(
    pct,
    breaks = c(0, quantile_probs, 1),
    labels = labels,
    include.lowest = TRUE,
    right = TRUE
  ))
}

# -----------------------------------------------------------------------------
# Object helpers
# -----------------------------------------------------------------------------
#
# S3 object construction, stored-vocabulary recovery, fitted-topic-count fallbacks, and object-shape predicates.

#' Construct an nlp_topic_fit object
#'
#' Builds the lightweight S3 wrapper returned by `fit_topic_model()` from standardized backend outputs.
#'
#' @keywords internal
#' @noRd
.new_nlp_topic_fit <- function(engine, model, method, model_object, dtw, tww,
                               doc_ids, vocab, docvars, doc_data,
                               hyperparameters, backend_control, call) {
  structure(
    list(
      engine = engine,
      model = model,
      method = method,
      model_object = model_object,
      dtw = dtw,
      tww = tww,
      doc_ids = doc_ids,
      vocab = vocab,
      docvars = docvars,
      doc_data = doc_data,
      hyperparameters = hyperparameters,
      backend_control = backend_control,
      call = call
    ),
    class = c("nlp_topic_fit", "list")
  )
}

#' Recover the fitted topic vocabulary
#'
#' Returns the stored vocabulary or derives it from cached topic-word weights when needed.
#'
#' @keywords internal
#' @noRd
.stored_topic_vocab <- function(x) {
  if (inherits(x, "nlp_topic_fit")) {
    if (!is.null(x$vocab)) {
      return(as.character(x$vocab))
    }
    if (!is.null(x$tww)) {
      return(colnames(x$tww))
    }
    if (!is.null(x$model_object) && .is_etm_object(x$model_object) && !is.null(x$model_object$vocab)) {
      return(as.character(x$model_object$vocab))
    }
    stop("This fit does not contain a stored vocabulary.", call. = FALSE)
  }
  if (.is_etm_object(x) && !is.null(x$vocab)) {
    return(as.character(x$vocab))
  }
  if (.is_stm_object(x) && !is.null(x$vocab)) {
    return(as.character(x$vocab))
  }
  NULL
}

#' Infer the fitted number of topics
#'
#' Returns a best-effort topic count from cached DTW/TWW matrices or backend model objects.
#'
#' @keywords internal
#' @noRd
.fit_topic_count <- function(x) {
  .fit_topic_count_source(x)$value
}

#' Infer topic count with source metadata
#'
#' Returns the fitted topic count plus the object location used to infer it.
#'
#' @keywords internal
#' @noRd
.fit_topic_count_source <- function(x) {
  if (!is.null(x$dtw)) {
    return(list(value = ncol(x$dtw), section = "fit_object", name = "dtw"))
  }
  if (!is.null(x$tww)) {
    return(list(value = nrow(x$tww), section = "fit_object", name = "tww"))
  }
  if (!is.null(x$model_object)) {
    if (!is.null(x$model_object$k)) {
      return(list(value = x$model_object$k, section = "model_object", name = "k"))
    }
    if (isS4(x$model_object) && "k" %in% methods::slotNames(x$model_object)) {
      return(list(
        value = methods::slot(x$model_object, "k"),
        section = "model_object",
        name = "k"
      ))
    }
    if (.is_stm_object(x$model_object)) {
      return(list(
        value = .stm_topic_count(x$model_object),
        section = "model_object",
        name = "settings$dim$K"
      ))
    }
  }
  list(value = NA_real_, section = NA_character_, name = NA_character_)
}

.is_stm_object <- function(x) {
  inherits(x, "STM")
}

.stm_topic_count <- function(x) {
  k <- x$settings$dim$K
  if (is.null(k)) {
    k <- ncol(x$theta)
  }
  as.integer(k)
}

.stm_doc_ids <- function(x, doc_ids = NULL) {
  if (!is.null(doc_ids)) {
    return(as.character(doc_ids))
  }
  ids <- rownames(x$theta)
  if (is.null(ids)) {
    ids <- as.character(seq_len(nrow(x$theta)))
  }
  as.character(ids)
}

.stm_tww_matrix <- function(x) {
  logbeta <- x$beta$logbeta
  if (is.null(logbeta) || length(logbeta) != 1L) {
    stop(.stm_content_covariate_error(), call. = FALSE)
  }
  mat <- exp(logbeta[[1L]])
  if (!is.null(x$vocab)) {
    colnames(mat) <- as.character(x$vocab)
  }
  mat
}

.stm_has_prevalence <- function(x) {
  model_object <- if (inherits(x, "nlp_topic_fit")) x$model_object else x
  !is.null(model_object$settings$covariates$formula)
}

#' Detect raw ETM objects
#'
#' Identifies topicmodels.etm objects without exposing backend class details elsewhere.
#'
#' @keywords internal
#' @noRd
.is_etm_object <- function(x) {
  inherits(x, "ETM")
}

#' Resolve an ETM model object
#'
#' Returns the raw ETM backend object from either an NLPstudio fit or an ETM object.
#'
#' @keywords internal
#' @noRd
.as_etm_model_object <- function(x) {
  if (inherits(x, "nlp_topic_fit")) {
    if (!identical(x$engine, "topicmodels.etm")) {
      stop("x must be an ETM fit or a raw ETM object.", call. = FALSE)
    }
    return(x$model_object)
  }
  if (.is_etm_object(x)) {
    return(x)
  }
  stop("x must be an ETM fit or a raw ETM object.", call. = FALSE)
}

#' Detect DTW-like tables
#'
#' Checks whether a table has document IDs and standardized topic columns.
#'
#' @keywords internal
#' @noRd
.looks_like_dtw_table <- function(x) {
  if (!(data.table::is.data.table(x) || is.data.frame(x))) {
    return(FALSE)
  }
  nms <- names(x)
  "doc_id" %in% nms && any(grepl("^Topic\\d+$", nms))
}

#' Detect TWW-like tables
#'
#' Checks whether a table has topic IDs and at least one term-weight column.
#'
#' @keywords internal
#' @noRd
.looks_like_tww_table <- function(x) {
  if (!(data.table::is.data.table(x) || is.data.frame(x))) {
    return(FALSE)
  }
  nms <- names(x)
  "topic_id" %in% nms && length(setdiff(nms, "topic_id")) > 0L
}
