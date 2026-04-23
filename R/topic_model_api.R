if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      "candidate_band", "doc_id", "probability", "rank", "term", "text",
      "topic", "topic_id", "topic_max_id", "topic_max_value", "topic_rank"
    )
  )
}

#' Fit a Topic Model Across Supported Backends
#'
#' Fit a topic model with a unified API across **text2vec**, **topicmodels**,
#' and **seededlda**. The fitted object stores both the raw backend fit and, by
#' default, cached DTW/TWW outputs following Lewis and Grossetti (2022):
#'
#' - **DTW**: document-topic weights
#' - **TWW**: topic-word weights
#'
#' @param x A document-feature input. Supported classes are
#'   [dgCMatrix-class][Matrix::dgCMatrix-class],
#'   [dfm][quanteda::dfm], and
#'   `DocumentTermMatrix`.
#' @param engine Backend package. One of `"text2vec"`, `"topicmodels"`, or
#'   `"seededlda"`.
#' @param model Model family within the selected backend.
#'   Supported combinations are:
#'
#'   - `engine = "text2vec"` with `model = "lda"`
#'   - `engine = "topicmodels"` with `model = "lda"` or `"ctm"`
#'   - `engine = "seededlda"` with `model = "lda"`, `"seqlda"`, or `"seededlda"`
#' @param k Number of topics. Required for all supported models except
#'   `engine = "seededlda", model = "seededlda"`.
#' @param method Fitting method within the selected model family.
#'
#'   - `topicmodels + lda`: `"VEM"` (default) or `"Gibbs"`
#'   - `topicmodels + ctm`: `"VEM"` only
#'   - `text2vec + lda`: `NULL` only
#'   - `seededlda`: `NULL` only
#' @param docvars Should a compact document-variable table be stored alongside
#'   the fitted model? Defaults to `TRUE`. Stored docvars always include the
#'   fitted `doc_id` values and, when `x` is a [dfm][quanteda::dfm], any
#'   available document variables. This does not retain original text.
#' @param doc_data Optional sidecar document data to store for downstream
#'   enrichment. Accepted inputs are a corpus, data.frame, or data.table keyed
#'   by `doc_id`. Text can only be attached downstream when this sidecar
#'   contains text or when it is supplied as a corpus.
#' @param return_dtw Should DTW be cached in the returned object? Defaults to
#'   `TRUE`.
#' @param return_tww Should TWW be cached in the returned object? Defaults to
#'   `TRUE`.
#' @param control A named list of backend controls with optional `model` and
#'   `fit` entries. Use `control$model` for model-construction arguments and
#'   `control$fit` for fitting arguments.
#'
#'   - `text2vec`: `control$model` is forwarded to `LDA$initialize()` and
#'     `control$fit` is forwarded to `LDA$fit_transform()`
#'   - `topicmodels`: `control$model` must be empty and `control$fit` is passed
#'     as backend `control =`
#'   - `seededlda`: `control$model` must be empty and `control$fit` is spliced
#'     into the selected `textmodel_*()` call
#' @param dictionary Dictionary required for
#'   `engine = "seededlda", model = "seededlda"`.
#' @param seedwords Optional `seedwords` argument forwarded only to
#'   `engine = "topicmodels", model = "lda", method = "Gibbs"`.
#' @param initial_model Optional previously fitted model passed to backend
#'   `model =` arguments where supported.
#'
#' @returns An S3 object of class `c("nlp_topic_fit", "list")`. It is a named
#'   list with these fields:
#'
#'   - `engine`: backend package used for estimation.
#'   - `model`: model family requested.
#'   - `method`: fitting method used, if applicable.
#'   - `model_object`: raw backend fit.
#'   - `dtw`: cached DTW matrix with `doc_id` rownames and `Topic###` columns,
#'     or `NULL`.
#'   - `tww`: cached TWW matrix with `Topic###` rownames and term columns, or
#'     `NULL`.
#'   - `doc_ids`: fitted document IDs in model order.
#'   - `docvars`: compact stored docvars keyed by `doc_id`, or `NULL`.
#'   - `doc_data`: stored sidecar document data, or `NULL`.
#'   - `call`: matched function call.
#'
#'   Users access these components with `$`, for example `fit$dtw` or
#'   `fit$model_object`.
#'
#' @details
#' `fit_topic_model()` standardizes model fitting while preserving the original
#' backend object in `model_object`. That design avoids brittle inheritance
#' across R6, S4, and list-based classes while still providing a stable package
#' interface for downstream helpers such as `get_dtw()`, `get_tww()`,
#' [get_top_terms()], and [plot_dtw()].
#'
#' The standardized DTW/TWW outputs always use topic identifiers of the form
#' `Topic001`, `Topic002`, and so on, regardless of backend-specific naming.
#'
#' Stored `docvars` and `doc_data` are used only for downstream alignment and
#' enrichment. They are never passed to the backend estimator itself.
#'
#' The API currently covers these model families and fitting algorithms:
#'
#' - LDA via **text2vec**, **topicmodels**, and **seededlda**
#' - CTM via **topicmodels**
#' - WarpLDA as the **text2vec** estimation algorithm for LDA
#' - Sequential LDA via **seededlda**
#' - Seeded LDA via **seededlda**
#'
#' @references
#' Lewis, C. M., & Grossetti, F. (2022).
#' [A statistical approach for optimal topic model identification](https://www.jmlr.org/papers/volume23/19-297/19-297.pdf).
#' _Journal of Machine Learning Research_, 23(58), 1-20.
#'
#' Blei, D. M., Ng, A. Y., & Jordan, M. I. (2003).
#' [Latent Dirichlet Allocation](https://www.jmlr.org/papers/volume3/blei03a/blei03a.pdf).
#' _Journal of Machine Learning Research_, 3, 993-1022.
#'
#' Blei, D. M., & Lafferty, J. D. (2006).
#' [Correlated topic models](https://www.cs.cmu.edu/afs/cs/usr/lafferty/www/pub/ctm.pdf).
#' _Advances in Neural Information Processing Systems_, 18, 147.
#' 
#' Blei, D. M., & Lafferty, J. D. (2007).
#' [A correlated topic model of Science](https://doi.org/10.1214/07-AOAS114).
#' _The Annals of Applied Statistics_, 1(1), 17-35.
#' 
#' Chen, J., Li, K., Zhu, J., & Chen, W. (2016).
#' [WarpLDA: A Cache Efficient O(1) Algorithm for Latent Dirichlet Allocation](https://pacman.cs.tsinghua.edu.cn/~cwg/publication/vldb16/vldb16.pdf).
#' _Proceedings of the VLDB Endowment_, 9(10), 744-755.
#'
#' Du, L., Buntine, W. L., Jin, H., & Chen, C. (2012).
#' [Sequential latent Dirichlet allocation](https://doi.org/10.1007/s10115-011-0425-1).
#' _Knowledge and Information Systems_, 31(3), 475-503.
#'
#' Lu, B., Ott, M., Cardie, C., & Tsou, B. K. (2011).
#' [Multi-aspect sentiment analysis with topic models](https://doi.org/10.1109/ICDMW.2011.125).
#' In _2011 IEEE 11th International Conference on Data Mining Workshops_, 81-88.
#'
#' Jagarlamudi, J., Daume III, H., & Udupa, R. (2012).
#' [Incorporating lexical priors into topic models](https://aclanthology.org/E12-1021.pdf).
#' In _Proceedings of the 13th Conference of the European Chapter of the Association for Computational Linguistics_, 204-213.
#'
#' Watanabe, K., & Zhou, Y. (2022).
#' [Theory-Driven Analysis of Large Corpora: Semisupervised Topic Classification of the UN Speeches](https://journals.sagepub.com/doi/full/10.1177/0894439320907027).
#' _Social Science Computer Review_, 40(2), 346-366.
#'
#' Watanabe, K., & Baturo, A. (2024).
#' [Seeded Sequential LDA: A Semi-Supervised Algorithm for Topic-Specific Analysis of Sentences](https://journals.sagepub.com/doi/10.1177/08944393231178605).
#' _Social Science Computer Review_, 42(1), 224-248.
#'
#' @examples
#' dtm <- methods::as(
#'   Matrix::Matrix(
#'     matrix(
#'       c(1, 0, 1,
#'         1, 1, 0,
#'         0, 1, 1,
#'         1, 1, 1),
#'       nrow = 4,
#'       byrow = TRUE
#'     ),
#'     sparse = TRUE
#'   ),
#'   "dgCMatrix"
#' )
#' rownames(dtm) <- paste0("doc", 1:4)
#' colnames(dtm) <- paste0("term", 1:3)
#'
#' fit <- fit_topic_model(
#'   dtm,
#'   engine = "text2vec",
#'   model = "lda",
#'   k = 2,
#'   control = list(
#'     model = list(doc_topic_prior = 0.1, topic_word_prior = 0.01),
#'     fit = list(n_iter = 25, progressbar = FALSE)
#'   )
#' )
#'
#' class(fit)
#' names(fit)
#'
#' if (requireNamespace("topicmodels", quietly = TRUE)) {
#'   fit_topic_model(
#'     dtm,
#'     engine = "topicmodels",
#'     model = "lda",
#'     k = 2,
#'     control = list(
#'       fit = list(seed = 1, em = list(iter.max = 5), var = list(iter.max = 5))
#'     )
#'   )
#'
#'   fit_topic_model(
#'     dtm,
#'     engine = "topicmodels",
#'     model = "ctm",
#'     k = 2,
#'     control = list(
#'       fit = list(seed = 1, em = list(iter.max = 5), var = list(iter.max = 5))
#'     )
#'   )
#' }
#'
#' if (requireNamespace("seededlda", quietly = TRUE)) {
#'   fit_topic_model(
#'     dtm,
#'     engine = "seededlda",
#'     model = "lda",
#'     k = 2,
#'     control = list(fit = list(max_iter = 100, verbose = FALSE))
#'   )
#'
#'   suppressWarnings(
#'     fit_topic_model(
#'       dtm,
#'       engine = "seededlda",
#'       model = "seqlda",
#'       k = 2,
#'       control = list(fit = list(max_iter = 100, verbose = FALSE))
#'     )
#'   )
#'
#'   dict <- quanteda::dictionary(list(
#'     topic_a = c("term1", "term2"),
#'     topic_b = c("term3")
#'   ))
#'
#'   fit_topic_model(
#'     dtm,
#'     engine = "seededlda",
#'     model = "seededlda",
#'     dictionary = dict,
#'     control = list(fit = list(max_iter = 100, verbose = FALSE))
#'   )
#' }
#'
#' @export
fit_topic_model <- function(x, engine, model, k = NULL, method = NULL,
                            docvars = TRUE, doc_data = NULL,
                            return_dtw = TRUE, return_tww = TRUE,
                            control = list(model = list(), fit = list()),
                            dictionary = NULL,
                            seedwords = NULL, initial_model = NULL) {

  call <- match.call()
  engine <- match.arg(engine, c("text2vec", "topicmodels", "seededlda"))
  model <- .match_topic_model(engine, model)
  method <- .normalize_topic_method(engine, model, method)
  control <- .normalize_topic_control(control)

  if (!is.null(k) && (!is.numeric(k) || length(k) != 1L || k < 1L || k != as.integer(k))) {
    stop("k must be NULL or a single positive integer.")
  }
  if (!is.logical(docvars) || length(docvars) != 1L) {
    stop("docvars must be a single TRUE/FALSE value.")
  }
  if (!is.logical(return_dtw) || length(return_dtw) != 1L) {
    stop("return_dtw must be a single TRUE/FALSE value.")
  }
  if (!is.logical(return_tww) || length(return_tww) != 1L) {
    stop("return_tww must be a single TRUE/FALSE value.")
  }

  .validate_topic_fit_args(
    engine = engine,
    model = model,
    method = method,
    k = k,
    control = control,
    dictionary = dictionary,
    seedwords = seedwords,
    initial_model = initial_model
  )

  fit_result <- switch(
    engine,
    text2vec = .fit_text2vec_topic_model(
      x = x,
      k = as.integer(k),
      control = control
    ),
    topicmodels = .fit_topicmodels_topic_model(
      x = x,
      model = model,
      k = as.integer(k),
      method = method,
      control = control,
      seedwords = seedwords,
      initial_model = initial_model
    ),
    seededlda = .fit_seededlda_topic_model(
      x = x,
      model = model,
      k = if (is.null(k)) NULL else as.integer(k),
      control = control,
      dictionary = dictionary,
      initial_model = initial_model
    )
  )

  .new_nlp_topic_fit(
    engine = engine,
    model = model,
    method = fit_result$method,
    model_object = fit_result$model_object,
    dtw = if (return_dtw) .dtw_matrix_from_matrix(
      fit_result$dtw,
      doc_ids = fit_result$doc_ids
    ) else NULL,
    tww = if (return_tww) .tww_matrix_from_matrix(
      fit_result$tww,
      term_names = fit_result$term_names
    ) else NULL,
    doc_ids = as.character(fit_result$doc_ids),
    docvars = if (docvars) .docvars_table_from_input(x, doc_ids = fit_result$doc_ids) else NULL,
    doc_data = if (is.null(doc_data)) NULL else .normalize_doc_data_table(
      doc_data,
      include_text = TRUE,
      arg_name = "doc_data"
    ),
    call = call
  )
}

#' Extract Standardized Document Topic Weights
#'
#' Extract DTW (document-topic weights) from a supported topic-model object and
#' return a standardized [data.table][data.table::data.table].
#'
#' @param x A supported topic-model object. This includes `nlp_topic_fit`,
#'   `warp_lda()` output, raw `topicmodels` fits, raw `seededlda` fits, and
#'   already standardized DTW tables.
#' @param doc_data Optional document-data override. When supplied, this is used
#'   instead of any `doc_data` stored in `x`. Accepted inputs are a corpus,
#'   data.frame, or data.table keyed by `doc_id`.
#' @param include_text Should a `text` column be attached when a text-bearing
#'   `doc_data` source is available? Defaults to `FALSE`. When `TRUE` but no
#'   text-bearing `doc_data` is available, the function emits a warning.
#' @param doc_id_col Document-ID column name when `doc_data` is a data.frame or
#'   data.table. Defaults to `"doc_id"`.
#' @param text_col Text column name when `doc_data` is a data.frame or
#'   data.table. Defaults to `"text"`.
#'
#' @returns A `data.table` with:
#'
#' - `doc_id`
#' - topic columns named `Topic001`, `Topic002`, ...
#' - `topic_max_id`
#' - `topic_max_value`
#' - metadata columns when available
#' - `text` when `include_text = TRUE` and text is available
#'
#' @examples
#' dtm <- methods::as(
#'   Matrix::Matrix(
#'     matrix(
#'       c(1, 0, 1,
#'         1, 1, 0,
#'         0, 1, 1,
#'         1, 1, 1),
#'       nrow = 4,
#'       byrow = TRUE
#'     ),
#'     sparse = TRUE
#'   ),
#'   "dgCMatrix"
#' )
#' rownames(dtm) <- paste0("doc", 1:4)
#' colnames(dtm) <- paste0("term", 1:3)
#'
#' fit <- fit_topic_model(
#'   dtm,
#'   engine = "text2vec",
#'   model = "lda",
#'   k = 2,
#'   control = list(fit = list(n_iter = 25, progressbar = FALSE))
#' )
#'
#' get_dtw(fit)
#'
#' @export
get_dtw <- function(x, doc_data = NULL, include_text = FALSE,
                    doc_id_col = "doc_id", text_col = "text") {
  if (!is.logical(include_text) || length(include_text) != 1L) {
    stop("include_text must be a single TRUE/FALSE value.")
  }

  dtw <- .extract_dtw_table(x)
  dtw <- .bind_topic_metadata(
    dtw,
    .stored_docvars_table(x, doc_ids = dtw$doc_id),
    overwrite = FALSE
  )
  dtw <- .bind_topic_metadata(
    dtw,
    .resolved_doc_data_table(
      x = x,
      doc_data = doc_data,
      include_text = include_text,
      doc_id_col = doc_id_col,
      text_col = text_col
    ),
    overwrite = TRUE
  )

  if (include_text && !"text" %in% names(dtw)) {
    warning(
      "include_text = TRUE, but no text-bearing doc_data was available.",
      call. = FALSE
    )
  }

  dtw[]
}

#' Extract Standardized Topic Word Weights
#'
#' Extract TWW (topic-word weights) from a supported topic-model object and
#' return a standardized wide [data.table][data.table::data.table].
#'
#' @param x A supported topic-model object. This includes `nlp_topic_fit`,
#'   `warp_lda()` output, raw `topicmodels` fits, raw `seededlda` fits, raw
#'   `text2vec::LDA` objects, and already standardized TWW tables.
#'
#' @returns A `data.table` with one row per topic, a `topic_id` column using
#'   the `Topic###` convention, and one column per term.
#'
#' @examples
#' dtm <- methods::as(
#'   Matrix::Matrix(
#'     matrix(
#'       c(1, 0, 1,
#'         1, 1, 0,
#'         0, 1, 1,
#'         1, 1, 1),
#'       nrow = 4,
#'       byrow = TRUE
#'     ),
#'     sparse = TRUE
#'   ),
#'   "dgCMatrix"
#' )
#' rownames(dtm) <- paste0("doc", 1:4)
#' colnames(dtm) <- paste0("term", 1:3)
#'
#' fit <- fit_topic_model(
#'   dtm,
#'   engine = "text2vec",
#'   model = "lda",
#'   k = 2,
#'   control = list(fit = list(n_iter = 25, progressbar = FALSE))
#' )
#'
#' get_tww(fit)
#'
#' @export
get_tww <- function(x) {
  .extract_tww_table(x)
}

#' Extract Representative Topic Candidates
#'
#' Identify representative document candidates from a topic model by assigning
#' each document to its dominant topic and banding documents within topic by
#' their dominant DTW values.
#'
#' @inheritParams get_dtw
#' @param topics Optional topic filter. May be supplied as numeric indices or
#'   `Topic###` identifiers. Filtering occurs after dominant-topic assignment.
#' @param quantile_probs Numeric vector of cumulative probabilities used to form
#'   candidate bands within each dominant topic. Defaults to quartiles:
#'   `c(0.25, 0.50, 0.75)`.
#' @param labels Labels used for the candidate bands. Must have length
#'   `length(quantile_probs) + 1L`. Defaults to `c("VLOW", "LOW", "HIGH", "VHIGH")`.
#'
#' @returns A `data.table` with one row per document and these core columns:
#'
#' - `doc_id`
#' - `topic_max_id`
#' - `topic_max_value`
#' - `candidate_band`
#' - `topic_rank`
#'
#' Metadata columns and optional `text` are included when available.
#'
#' @details
#' Candidate bands are computed within each dominant topic, not globally across
#' the corpus. When within-topic quantile cut points collapse because of small
#' groups or tied values, the function falls back to deterministic rank-based
#' banding.
#'
#' @examples
#' dtm <- methods::as(
#'   Matrix::Matrix(
#'     matrix(
#'       c(1, 0, 1,
#'         1, 1, 0,
#'         0, 1, 1,
#'         1, 1, 1),
#'       nrow = 4,
#'       byrow = TRUE
#'     ),
#'     sparse = TRUE
#'   ),
#'   "dgCMatrix"
#' )
#' rownames(dtm) <- paste0("doc", 1:4)
#' colnames(dtm) <- paste0("term", 1:3)
#'
#' metadata <- data.table::data.table(
#'   doc_id = rownames(dtm),
#'   year = 2020:2023,
#'   text = c("alpha beta", "beta gamma", "gamma delta", "alpha delta")
#' )
#'
#' fit <- fit_topic_model(
#'   dtm,
#'   engine = "text2vec",
#'   model = "lda",
#'   k = 2,
#'   doc_data = metadata,
#'   control = list(fit = list(n_iter = 25, progressbar = FALSE))
#' )
#'
#' get_representative_candidates(fit, include_text = TRUE)
#'
#' @export
get_representative_candidates <- function(x, doc_data = NULL, topics = NULL,
                                          include_text = FALSE,
                                          quantile_probs = c(0.25, 0.50, 0.75),
                                          labels = c("VLOW", "LOW", "HIGH", "VHIGH"),
                                          doc_id_col = "doc_id",
                                          text_col = "text") {
  if (!is.numeric(quantile_probs) || anyNA(quantile_probs)) {
    stop("quantile_probs must be a numeric vector without NA values.")
  }
  if (length(labels) != length(quantile_probs) + 1L) {
    stop("labels must have length length(quantile_probs) + 1.")
  }
  if (!all(diff(quantile_probs) > 0) || any(quantile_probs <= 0) || any(quantile_probs >= 1)) {
    stop("quantile_probs must be strictly increasing values between 0 and 1.")
  }

  out <- get_dtw(
    x = x,
    doc_data = doc_data,
    include_text = include_text,
    doc_id_col = doc_id_col,
    text_col = text_col
  )
  out <- data.table::copy(out)

  if (!is.null(topics)) {
    keep_topics <- .resolve_topic_selector(unique(out$topic_max_id), topics)
    out <- out[topic_max_id %in% keep_topics]
  }

  if (!nrow(out)) {
    out[, candidate_band := character()]
    out[, topic_rank := integer()]
    return(out[])
  }

  out[, topic_rank := data.table::frank(-topic_max_value, ties.method = "first"),
      by = topic_max_id]
  out[, candidate_band := .assign_candidate_bands(topic_max_value, quantile_probs, labels),
      by = topic_max_id]

  data.table::setorder(out, topic_max_id, topic_rank, doc_id)
  out[]
}

#' @keywords internal
.new_nlp_topic_fit <- function(engine, model, method, model_object, dtw, tww,
                               doc_ids, docvars, doc_data, call) {
  structure(
    list(
      engine = engine,
      model = model,
      method = method,
      model_object = model_object,
      dtw = dtw,
      tww = tww,
      doc_ids = doc_ids,
      docvars = docvars,
      doc_data = doc_data,
      call = call
    ),
    class = c("nlp_topic_fit", "list")
  )
}

#' Print a Compact Summary of a Topic-Model Fit
#'
#' Print a compact summary of an object returned by [fit_topic_model()] without
#' expanding the cached DTW, TWW, or backend fit internals.
#'
#' @param x An object returned by [fit_topic_model()].
#' @param ... Unused.
#'
#' @returns Invisibly returns `x`.
#' @seealso [fit_topic_model()]
#' @export
print.nlp_topic_fit <- function(x, ...) {
  n_docs <- length(x$doc_ids)
  n_topics <- if (!is.null(x$dtw)) {
    ncol(x$dtw)
  } else if (!is.null(x$tww)) {
    nrow(x$tww)
  } else {
    NA_integer_
  }
  n_terms <- if (!is.null(x$tww)) ncol(x$tww) else NA_integer_

  cat("<nlp_topic_fit>\n")
  cat("  engine: ", x$engine, "\n", sep = "")
  cat("  model: ", x$model, sep = "")
  if (!is.null(x$method)) {
    cat(" (", x$method, ")", sep = "")
  }
  cat("\n")
  cat("  documents: ", n_docs, "\n", sep = "")
  if (!is.na(n_topics)) {
    cat("  topics: ", n_topics, "\n", sep = "")
  }
  if (!is.na(n_terms)) {
    cat("  terms: ", n_terms, "\n", sep = "")
  }
  cat("  cached DTW: ", !is.null(x$dtw), "\n", sep = "")
  cat("  cached TWW: ", !is.null(x$tww), "\n", sep = "")
  cat("  stored docvars: ", !is.null(x$docvars), "\n", sep = "")
  cat("  stored doc_data: ", !is.null(x$doc_data), "\n", sep = "")

  invisible(x)
}

#' @keywords internal
.match_topic_model <- function(engine, model) {
  if (!is.character(model) || length(model) != 1L || is.na(model)) {
    stop("model must be a single character string.")
  }

  model <- tolower(model)
  allowed <- switch(
    engine,
    text2vec = "lda",
    topicmodels = c("lda", "ctm"),
    seededlda = c("lda", "seqlda", "seededlda")
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

#' @keywords internal
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

#' @keywords internal
.normalize_topic_control <- function(control) {
  if (is.null(control) || !length(control)) {
    return(list(model = list(), fit = list()))
  }
  if (!is.list(control)) {
    stop("control must be a list.")
  }

  nms <- names(control)
  if (is.null(nms) || any(nms == "")) {
    stop("control must be a named list with optional 'model' and 'fit' entries.")
  }

  extra <- setdiff(nms, c("model", "fit"))
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
    fit = if ("fit" %in% nms) control$fit else list()
  )

  if (is.null(out$model)) {
    out$model <- list()
  }
  if (is.null(out$fit)) {
    out$fit <- list()
  }
  if (!is.list(out$model) || !is.list(out$fit)) {
    stop("control$model and control$fit must both be lists.")
  }

  out
}

#' @keywords internal
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
  if (engine != "text2vec" && length(control$model)) {
    stop(
      "control$model must be empty unless engine = 'text2vec'.",
      call. = FALSE
    )
  }
  if (engine == "text2vec" && !is.null(initial_model)) {
    stop("initial_model is not supported for engine = 'text2vec'.")
  }
  if (engine == "seededlda" && model == "seededlda" && !is.null(initial_model)) {
    stop("initial_model is not supported for engine = 'seededlda', model = 'seededlda'.")
  }
}

#' @keywords internal
.fit_text2vec_topic_model <- function(x, k, control) {
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
    method = NULL
  )
}

#' @keywords internal
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
    method = method
  )
}

#' @keywords internal
.fit_seededlda_topic_model <- function(x, model, k, control, dictionary,
                                       initial_model) {
  if (!requireNamespace("seededlda", quietly = TRUE)) {
    stop("Package 'seededlda' must be installed to use engine = 'seededlda'.", call. = FALSE)
  }

  x_dfm <- .as_topic_dfm(x)

  fit_args <- utils::modifyList(list(x = x_dfm), control$fit)
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
    method = NULL
  )
}

#' @keywords internal
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
    return(.extract_dtw_table(x$model_object))
  }

  if (.is_warp_lda_result(x)) {
    if (is.null(x$theta)) {
      stop(
        "This warp_lda() result does not contain cached theta/DTW. Refit with return_theta = TRUE.",
        call. = FALSE
      )
    }
    return(.coerce_existing_dtw_table(x$theta))
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

  if (inherits(x, "WarpLDA")) {
    stop(
      "Raw text2vec WarpLDA objects do not retain DTW. Use fit_topic_model(..., return_dtw = TRUE) or warp_lda(..., return_theta = TRUE).",
      call. = FALSE
    )
  }

  stop("x is an unrecognized object for DTW extraction.")
}

#' @keywords internal
.extract_tww_table <- function(x) {
  if (inherits(x, "nlp_topic_fit")) {
    if (!is.null(x$tww)) {
      return(.tww_dt_from_matrix(x$tww))
    }
    return(.extract_tww_table(x$model_object))
  }

  if (.is_warp_lda_result(x)) {
    if (!is.null(x$phi)) {
      if (.looks_like_tww_table(x$phi)) {
        return(.coerce_existing_tww_table(x$phi))
      }
      return(.tww_dt_from_matrix(
        x$phi,
        term_names = colnames(as.matrix(x$phi))
      ))
    }
    return(.tww_dt_from_matrix(
      x$lda_object$topic_word_distribution,
      term_names = colnames(x$lda_object$topic_word_distribution)
    ))
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

  if (inherits(x, "WarpLDA")) {
    return(.tww_dt_from_matrix(
      x$topic_word_distribution,
      term_names = colnames(x$topic_word_distribution)
    ))
  }

  stop("x is an unrecognized object for TWW extraction.")
}

#' @keywords internal
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

#' @keywords internal
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

#' @keywords internal
.as_topicmodels_input <- function(x) {
  if (methods::is(x, "DocumentTermMatrix")) {
    return(x)
  }

  quanteda::convert(.as_topic_dfm(x), to = "topicmodels")
}

#' @keywords internal
.topic_ids <- function(n) {
  sprintf("Topic%03d", seq_len(n))
}

#' @keywords internal
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

#' @keywords internal
.dtw_matrix_from_matrix <- function(x, doc_ids = NULL) {
  mat <- as.matrix(x)
  rownames(mat) <- .matrix_doc_ids(mat, fallback = doc_ids)
  colnames(mat) <- .topic_ids(ncol(mat))
  storage.mode(mat) <- "double"
  mat
}

#' @keywords internal
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

#' @keywords internal
.dtw_dt_from_matrix <- function(x, doc_ids = NULL) {
  mat <- .dtw_matrix_from_matrix(x, doc_ids = doc_ids)
  out <- data.table::data.table(doc_id = rownames(mat))
  out <- cbind(out, data.table::as.data.table(mat))
  .add_topic_max_columns(out)
}

#' @keywords internal
.tww_dt_from_matrix <- function(x, term_names = NULL, log_scale = FALSE) {
  mat <- .tww_matrix_from_matrix(x, term_names = term_names, log_scale = log_scale)
  out <- data.table::data.table(topic_id = rownames(mat))
  cbind(out, data.table::as.data.table(mat))
}

#' @keywords internal
.coerce_existing_dtw_table <- function(x) {
  dt <- data.table::as.data.table(x)

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

  old_summary_cols <- intersect(c("topic_max_id", "topic_max_value"), names(dt))
  if (length(old_summary_cols)) {
    dt[, (old_summary_cols) := NULL]
  }
  .add_topic_max_columns(dt)
}

#' @keywords internal
.coerce_existing_tww_table <- function(x) {
  dt <- data.table::as.data.table(x)

  if (!"topic_id" %in% names(dt)) {
    stop("TWW tables must contain a 'topic_id' column.", call. = FALSE)
  }

  term_cols <- setdiff(names(dt), "topic_id")
  data.table::setnames(dt, "topic_id", "topic_id")
  dt[, topic_id := .topic_ids(.N)]
  data.table::setcolorder(dt, c("topic_id", term_cols))
  dt[]
}

#' @keywords internal
.find_topic_columns <- function(x, id_col) {
  nms <- names(x)
  topic_cols <- grep("^Topic\\d+$", nms, value = TRUE)
  if (length(topic_cols)) {
    ord <- order(as.integer(stringr::str_extract(topic_cols, "\\d+")))
    return(topic_cols[ord])
  }

  candidate_cols <- setdiff(nms, c(id_col, "topic_max_id", "topic_max_value"))
  if (length(candidate_cols) &&
      all(vapply(x[, candidate_cols, with = FALSE], is.numeric, logical(1)))) {
    return(candidate_cols)
  }

  character()
}

#' @keywords internal
.add_topic_max_columns <- function(x) {
  topic_cols <- .find_topic_columns(x, id_col = "doc_id")
  topic_mat <- as.matrix(x[, topic_cols, with = FALSE])
  max_idx <- max.col(topic_mat, ties.method = "first")
  x[, topic_max_id := topic_cols[max_idx]]
  x[, topic_max_value := topic_mat[cbind(seq_len(nrow(topic_mat)), max_idx)]]
  x[]
}

#' @keywords internal
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

#' @keywords internal
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

#' @keywords internal
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

#' @keywords internal
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

#' @keywords internal
.bind_topic_metadata <- function(dtw, meta, overwrite = FALSE) {
  if (is.null(meta)) {
    return(dtw[])
  }

  meta <- data.table::as.data.table(meta)
  meta <- meta[!duplicated(doc_id)]
  extra_cols <- setdiff(names(meta), "doc_id")
  protected <- c("doc_id", .find_topic_columns(dtw, id_col = "doc_id"), "topic_max_id", "topic_max_value")
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

#' @keywords internal
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

#' @keywords internal
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

#' @keywords internal
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

#' @keywords internal
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

#' @keywords internal
.is_warp_lda_result <- function(x) {
  is.list(x) && !is.null(x$lda_object) && inherits(x$lda_object, "WarpLDA")
}

#' @keywords internal
.looks_like_dtw_table <- function(x) {
  if (!(data.table::is.data.table(x) || is.data.frame(x))) {
    return(FALSE)
  }
  nms <- names(x)
  "doc_id" %in% nms && any(grepl("^Topic\\d+$", nms))
}

#' @keywords internal
.looks_like_tww_table <- function(x) {
  if (!(data.table::is.data.table(x) || is.data.frame(x))) {
    return(FALSE)
  }
  nms <- names(x)
  "topic_id" %in% nms && length(setdiff(nms, "topic_id")) > 0L
}
