if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      "candidate_band", "cluster", "dim_001", "dim_002", "doc_id", "label", "probability",
      "rank", "term", "text", "topic", "topic_id", "topic_max_id",
      "topic_max_int", "topic_max_value", "topic_rank", "type", "weight", "x", "y"
    )
  )
}

#' Fit a Topic Model Via a Unified API
#'
#' Fit a topic model with a unified API across **text2vec**, **topicmodels**,
#' **seededlda**, and **topicmodels.etm**. The fitted object stores both the
#' raw backend fit and, by default, cached DTW/TWW outputs following the convention of Lewis and
#' Grossetti (2022):
#'
#' @param x A document-feature input. Supported classes are
#'   [dgCMatrix-class][Matrix::dgCMatrix-class],
#'   [dfm][quanteda::dfm], and
#'   `DocumentTermMatrix`.
#' @param engine Backend package. One of `"text2vec"`, `"topicmodels"`,
#'   `"seededlda"`, or `"topicmodels.etm"`.
#' @param model Model family within the selected backend.
#'   Supported combinations are:
#'
#'   - `engine = "text2vec"` with `model = "lda"`
#'   - `engine = "topicmodels"` with `model = "lda"` or `"ctm"`
#'   - `engine = "seededlda"` with `model = "lda"`, `"seqlda"`, or `"seededlda"`
#'   - `engine = "topicmodels.etm"` with `model = "etm"`
#' @param k Number of topics \eqn{K}. Required for all supported models except
#'   `engine = "seededlda", model = "seededlda"`.
#' @param method Fitting method within the selected model family.
#'
#'   - `topicmodels + lda`: `"VEM"` (default) or `"Gibbs"`
#'   - `topicmodels + ctm`: `"VEM"` only
#'   - `text2vec + lda`: `NULL` only
#'   - `seededlda`: `NULL` only
#'   - `topicmodels.etm + etm`: `NULL` only
#' @param docvars Should a compact document-variable table be stored alongside
#'   the fitted model? Defaults to `TRUE`. Stored docvars always include the
#'   fitted `doc_id` values and, when `x` is a [dfm][quanteda::dfm], any
#'   available document variables. This does not retain original text.
#' @param doc_data Optional sidecar document data to store for downstream
#'   enrichment. Accepted inputs are a corpus, data.frame, or data.table keyed
#'   by `doc_id`. Text can only be attached downstream when this sidecar
#'   contains text or when it is supplied as a corpus.
#' @param return_dtw Should document-topic-weights (DTW) be cached in the returned object? Defaults to
#'   `TRUE`.
#' @param return_tww Should topic-term-weights (TWW) be cached in the returned object? Defaults to
#'   `TRUE`.
#' @param control A named list of backend controls with optional `model`,
#'   `fit`, and `optimizer` entries. Use `control$model` for model-construction
#'   arguments, `control$fit` for fitting arguments, and `control$optimizer`
#'   for ETM optimizer arguments.
#'
#'   - `text2vec`: `control$model` is forwarded to LDA$initialize()` and
#'     `control$fit` is forwarded to `LDA$fit_transform()`; `control$optimizer`
#'     must be empty
#'   - `topicmodels`: `control$model` must be empty and `control$fit` is passed
#'     as backend `control =`; `control$optimizer` must be empty
#'   - `seededlda`: `control$model` must be empty and `control$fit` is spliced
#'     into the selected `textmodel_*()` call; `control$optimizer` must be
#'     empty
#'   - `topicmodels.etm`: `control$model` is forwarded to
#'     `topicmodels.etm::ETM()`, `control$fit` is forwarded to `$fit(...)`, and
#'     `control$optimizer` is forwarded to
#'     `torch::optim_adam(params = model$parameters, ...)`
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
#'   - `vocab`: fitted vocabulary in term order.
#'   - `docvars`: compact stored docvars keyed by `doc_id`, or `NULL`.
#'   - `doc_data`: stored sidecar document data, or `NULL`.
#'   - `hyperparameters`: standardized topic-model hyperparameters. Use
#'     [get_topic_hyperparameters()] for a stable tabular accessor.
#'   - `backend_control`: sanitized backend-native model, fit, and optimizer
#'     controls after defaults and package-level normalization are applied.
#'   - `call`: matched function call.
#'
#'   Users access these components with `$`, for example `fit$dtw` or
#'   `fit$model_object`.
#'
#' @details
#' `fit_topic_model()` standardizes model fitting while preserving the original
#' backend object in `model_object`. That design avoids brittle inheritance
#' across R6, S4, and list-based classes while still providing a stable package
#' interface for downstream helpers such as [get_dtw()], [get_tww()],
#' `predict_topic_model()`, [get_top_terms()], and [plot_dtw()].
#'
#' The standardized DTW/TWW outputs always use topic identifiers of the form
#' `Topic001`, `Topic002`, and so on, regardless of backend-specific naming.
#'
#' Stored `docvars` and `doc_data` are used only for downstream alignment and
#' enrichment. They are never passed to the backend estimator itself.
#'
#' ETM requires `control$model$embeddings`, supplied either as a single integer
#' embedding dimension or as a pretrained embedding matrix. When learned
#' embeddings are requested, `vocab` defaults to the input terms unless
#' explicitly supplied. When pretrained embeddings are supplied, the input
#' vocabulary is aligned to the embedding rownames; unmatched terms and any
#' documents that become empty after alignment are dropped with a warning while
#' preserving surviving `doc_id`, `docvars`, and `doc_data` alignment.
#' Using `engine = "topicmodels.etm"` also requires both the **topicmodels.etm**
#' package and a working **torch** backend. Installing the R **torch** package is
#' not sufficient by itself on a clean machine; run `torch::install_torch()` and
#' confirm that `torch::torch_is_installed()` returns `TRUE`.
#'
#' The API currently covers these model families and fitting algorithms:
#'
#' - LDA via **text2vec**, **topicmodels**, and **seededlda**
#' - CTM via **topicmodels**
#' - WarpLDA as the **text2vec** estimation algorithm for LDA
#' - Sequential LDA via **seededlda**
#' - Seeded LDA via **seededlda**
#' - Embedded Topic Models via **topicmodels.etm**
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
#' Dieng, A. B., Ruiz, F. J. R., & Blei, D. M. (2020).
#' [Topic Modeling in Embedding Spaces](https://arxiv.org/pdf/1907.04907).
#' _Transactions of the Association for Computational Linguistics_, 8, 439-453.
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
#' Theory-Driven Analysis of Large Corpora: Semisupervised Topic Classification of the UN Speeches.
#' DOI: 10.1177/0894439320907027.
#' _Social Science Computer Review_, 40(2), 346-366.
#'
#' Watanabe, K., & Baturo, A. (2024).
#' Seeded Sequential LDA: A Semi-Supervised Algorithm for Topic-Specific Analysis of Sentences.
#' DOI: 10.1177/08944393231178605.
#' _Social Science Computer Review_, 42(1), 224-248.
#' 
#' @seealso [topicmodels::LDA()] [topicmodels::CTM()] [text2vec::LDA()] [seededlda::textmodel_seqlda()]
#' [topicmodels.etm::ETM()]
#'
#' @examplesIf requireNamespace("text2vec", quietly = TRUE)
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
#' @examplesIf requireNamespace("topicmodels.etm", quietly = TRUE) && requireNamespace("torch", quietly = TRUE) && torch::torch_is_installed()
#' fit_topic_model(
#'   dtm,
#'   engine = "topicmodels.etm",
#'   model = "etm",
#'   k = 2,
#'   control = list(
#'     model = list(embeddings = 5),
#'     fit = list(epoch = 5, batch_size = 2, normalize = TRUE),
#'     optimizer = list(lr = 0.005, weight_decay = 1.2e-06)
#'   )
#' )
#'
#' embeddings <- matrix(
#'   seq_len(ncol(dtm) * 4),
#'   nrow = ncol(dtm),
#'   ncol = 4,
#'   dimnames = list(colnames(dtm), NULL)
#' )
#'
#' fit_topic_model(
#'   dtm,
#'   engine = "topicmodels.etm",
#'   model = "etm",
#'   k = 2,
#'   control = list(
#'     model = list(embeddings = embeddings),
#'     fit = list(epoch = 5, batch_size = 2)
#'   )
#' )
#'
#' @export
fit_topic_model <- function(x, engine, model, k = NULL, method = NULL,
                            docvars = TRUE, doc_data = NULL,
                            return_dtw = TRUE, return_tww = TRUE,
                            control = list(model = list(), fit = list(), optimizer = list()),
                            dictionary = NULL,
                            seedwords = NULL, initial_model = NULL) {

  call <- match.call()
  engine <- match.arg(engine, c("text2vec", "topicmodels", "seededlda", "topicmodels.etm"))
  model <- .match_topic_model(engine, model)
  method <- .normalize_topic_method(engine, model, method)
  control <- .normalize_topic_control(control)

  if (!is.null(k) && (!is.numeric(k) || length(k) != 1L || k < 1L || k != as.integer(k))) {
    stop("k must be NULL or a single positive integer.")
  }
  if (!is.logical(docvars) || length(docvars) != 1L || is.na(docvars)) {
    stop("docvars must be a single TRUE/FALSE value.")
  }
  if (!is.logical(return_dtw) || length(return_dtw) != 1L || is.na(return_dtw)) {
    stop("return_dtw must be a single TRUE/FALSE value.")
  }
  if (!is.logical(return_tww) || length(return_tww) != 1L || is.na(return_tww)) {
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
    ),
    `topicmodels.etm` = .fit_etm_topic_model(
      x = x,
      k = as.integer(k),
      control = control
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
    vocab = as.character(fit_result$term_names),
    docvars = if (docvars) .docvars_table_from_input(x, doc_ids = fit_result$doc_ids) else NULL,
    doc_data = if (is.null(doc_data)) NULL else .normalize_doc_data_table(
      doc_data,
      include_text = TRUE,
      arg_name = "doc_data"
    ),
    hyperparameters = fit_result$hyperparameters,
    backend_control = fit_result$backend_control,
    call = call
  )
}

#' Predict Document Topic Weights for New Data
#'
#' Predict standardized document-topic weights (DTW) for new documents using a
#' fitted object returned by [fit_topic_model()].
#'
#' @param x An object of class `nlp_topic_fit`.
#' @param newdata New document-feature input. Supported classes are
#'   [dgCMatrix-class][Matrix::dgCMatrix-class], [dfm][quanteda::dfm], and
#'   `DocumentTermMatrix`.
#' @param control A named list of backend-specific prediction arguments.
#'   Defaults to `list()`.
#'
#'   - `text2vec`: forwarded to `model_object$transform()`
#'   - `topicmodels`: forwarded to `topicmodels::posterior()`
#'   - `seededlda`: forwarded to the relevant `textmodel_*()` update call
#'   - `topicmodels.etm`: forwarded to [stats::predict()] with `type = "topics"`
#' @param docvars Should available docvars from `newdata` be joined onto the
#'   returned DTW table? Defaults to `FALSE`.
#' @param doc_data Optional document-data sidecar for metadata or text
#'   enrichment. Accepted inputs are a corpus, data.frame, or data.table keyed
#'   by `doc_id`.
#' @param include_text Should a `text` column be attached when a text-bearing
#'   `doc_data` source is available? Defaults to `FALSE`.
#' @param doc_id_col Document-ID column name when `doc_data` is a data.frame or
#'   data.table. Defaults to `"doc_id"`.
#' @param text_col Text column name when `doc_data` is a data.frame or
#'   data.table. Defaults to `"text"`.
#'
#' @returns A standardized DTW [data.table][data.table::data.table] with:
#'
#' - `doc_id`
#' - topic columns named `Topic001`, `Topic002`, ...
#' - `topic_max_id`
#' - `topic_max_int`
#' - `topic_max_value`
#' - available docvars when `docvars = TRUE`
#' - optional metadata/text joined from `doc_data`
#'
#' Columns are ordered as `doc_id`, document metadata, DTW output columns, and
#' finally `text` when text is requested and available.
#'
#' @details
#' Prediction input is first aligned to the fitted vocabulary stored in `x$vocab`.
#' Terms absent from the fitted vocabulary are dropped with a warning, missing
#' fitted terms are added as zero columns, columns are reordered to fitted
#' vocabulary order, and any documents that become empty after alignment are
#' dropped with a warning.
#'
#' @examplesIf requireNamespace("text2vec", quietly = TRUE)
#' dtm <- methods::as(
#'   Matrix::Matrix(
#'     matrix(
#'       c(2, 1, 0, 0,
#'         1, 1, 1, 0,
#'         0, 1, 2, 1,
#'         0, 0, 1, 2),
#'       nrow = 4,
#'       byrow = TRUE
#'     ),
#'     sparse = TRUE
#'   ),
#'   "dgCMatrix"
#' )
#' rownames(dtm) <- paste0("doc", 1:4)
#' colnames(dtm) <- paste0("term", 1:4)
#'
#' fit <- fit_topic_model(
#'   dtm,
#'   engine = "text2vec",
#'   model = "lda",
#'   k = 2,
#'   control = list(fit = list(n_iter = 25, progressbar = FALSE))
#' )
#'
#' new_dtm <- methods::as(
#'   Matrix::Matrix(
#'     matrix(
#'       c(1, 0, 0, 1,
#'         0, 1, 1, 0),
#'       nrow = 2,
#'       byrow = TRUE
#'     ),
#'     sparse = TRUE
#'   ),
#'   "dgCMatrix"
#' )
#' rownames(new_dtm) <- c("new1", "new2")
#' colnames(new_dtm) <- paste0("term", 1:4)
#'
#' predict_topic_model(fit, new_dtm)
#'
#' @export
predict_topic_model <- function(x, newdata, control = list(), docvars = FALSE,
                                doc_data = NULL, include_text = FALSE,
                                doc_id_col = "doc_id", text_col = "text") {
  if (!inherits(x, "nlp_topic_fit")) {
    stop("x must be an object returned by fit_topic_model().", call. = FALSE)
  }
  if (!is.logical(docvars) || length(docvars) != 1L || is.na(docvars)) {
    stop("docvars must be a single TRUE/FALSE value.")
  }
  if (!is.logical(include_text) || length(include_text) != 1L || is.na(include_text)) {
    stop("include_text must be a single TRUE/FALSE value.")
  }

  control <- .normalize_prediction_control(control)
  vocab <- .stored_topic_vocab(x)
  aligned <- .align_topic_input_to_vocab(
    newdata,
    vocab = vocab,
    vocab_label = "fitted vocabulary",
    context = "prediction vocabulary alignment"
  )

  dtw <- .predict_topic_matrix(
    fit = x,
    newdata_aligned = aligned,
    control = control
  )
  out <- .dtw_dt_from_matrix(dtw, doc_ids = aligned$doc_ids)

  if (docvars) {
    out <- .bind_topic_metadata(
      out,
      .docvars_table_from_input(aligned$dfm, doc_ids = aligned$doc_ids),
      overwrite = FALSE
    )
  }
  out <- .bind_topic_metadata(
    out,
    if (is.null(doc_data)) NULL else .normalize_doc_data_table(
      doc_data,
      include_text = include_text,
      doc_id_col = doc_id_col,
      text_col = text_col,
      arg_name = "doc_data"
    ),
    overwrite = TRUE
  )

  if (include_text && !"text" %in% names(out)) {
    warning(
      "include_text = TRUE, but no text-bearing doc_data was available.",
      call. = FALSE
    )
  }

  .order_dtw_output(out)
}

#' Extract Standardized Document Topic Weights
#'
#' Extract DTW (document-topic weights) from a supported topic-model object and
#' return a standardized [data.table][data.table::data.table].
#'
#' @param x A supported topic-model object. This includes `nlp_topic_fit`,
#'   raw `topicmodels` fits, raw `seededlda` fits, and already standardized DTW
#'   tables.
#' @param doc_data Optional document-data override. When supplied, this is used
#'   instead of any `doc_data` stored in `x`. Accepted inputs are a corpus,
#'   data.frame, or data.table keyed by `doc_id`.
#' @param docvars Should stored or pre-existing document metadata be joined
#'   onto the returned DTW table? Defaults to `FALSE`.
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
#' - `topic_max_int`
#' - `topic_max_value`
#' - stored docvars when `docvars = TRUE`
#' - metadata columns from `doc_data` when available
#' - `text` when `include_text = TRUE` and text is available
#'
#' Columns are ordered as `doc_id`, document metadata, DTW output columns, and
#' finally `text` when text is requested and available.
#' For already standardized DTW-table inputs, non-topic metadata columns are
#' treated as pre-existing document metadata and retained only when
#' `docvars = TRUE`.
#'
#' @examplesIf requireNamespace("text2vec", quietly = TRUE)
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
#' get_dtw(fit, docvars = TRUE)
#'
#' @export
get_dtw <- function(x, doc_data = NULL, docvars = FALSE, include_text = FALSE,
                    doc_id_col = "doc_id", text_col = "text") {
  if (!is.logical(docvars) || length(docvars) != 1L || is.na(docvars)) {
    stop("docvars must be a single TRUE/FALSE value.")
  }
  if (!is.logical(include_text) || length(include_text) != 1L || is.na(include_text)) {
    stop("include_text must be a single TRUE/FALSE value.")
  }

  existing_dtw_input <- .looks_like_dtw_table(x)
  dtw <- .extract_dtw_table(x)
  if (existing_dtw_input) {
    dtw <- .filter_existing_dtw_metadata(
      dtw,
      docvars = docvars,
      include_text = include_text
    )
  }
  if (docvars) {
    dtw <- .bind_topic_metadata(
      dtw,
      .stored_docvars_table(x, doc_ids = dtw$doc_id),
      overwrite = FALSE
    )
  }
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

  .order_dtw_output(dtw)
}

#' Extract Standardized Topic Word Weights
#'
#' Extract TWW (topic-word weights) from a supported topic-model object and
#' return a standardized wide [data.table][data.table::data.table].
#'
#' @param x A supported topic-model object. This includes `nlp_topic_fit`,
#'   raw `topicmodels` fits, raw `seededlda` fits, raw `topicmodels.etm`
#'   objects, raw `text2vec::LDA` objects, and already standardized TWW
#'   tables.
#'
#' @returns A `data.table` with one row per topic, a `topic_id` column using
#'   the `Topic###` convention, and one column per term.
#'
#' @examplesIf requireNamespace("text2vec", quietly = TRUE)
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

#' Extract ETM Topic Embeddings
#'
#' Extract topic-center embeddings from an embedded topic model (ETM).
#'
#' @param x Either an `nlp_topic_fit` with `engine = "topicmodels.etm"` or a raw
#'   ETM object.
#'
#' @returns A [data.table][data.table::data.table] with one row per topic,
#'   a standardized `topic_id` column using the `Topic###` convention, and one
#'   column per embedding dimension named `dim_001`, `dim_002`, and so on.
#'
#' @examplesIf requireNamespace("topicmodels.etm", quietly = TRUE) && requireNamespace("torch", quietly = TRUE) && torch::torch_is_installed()
#' path <- system.file(package = "topicmodels.etm", "example", "example_etm.ckpt")
#' model <- torch::torch_load(path)
#' get_topic_embeddings(model)
#'
#' @export
get_topic_embeddings <- function(x) {
  model <- .as_etm_model_object(x)
  emb <- as.matrix(model, type = "embedding", which = "topics")
  emb <- as.matrix(emb)
  colnames(emb) <- sprintf("dim_%03d", seq_len(ncol(emb)))

  out <- data.table::data.table(topic_id = .topic_ids(nrow(emb)))
  cbind(out, data.table::as.data.table(emb))
}

#' Extract ETM Term Embeddings
#'
#' Extract term embeddings from an embedded topic model (ETM).
#'
#' @param x Either an `nlp_topic_fit` with `engine = "topicmodels.etm"` or a raw
#'   ETM object.
#'
#' @returns A [data.table][data.table::data.table] with one row per term,
#'   a `term` column, and one column per embedding dimension named `dim_001`,
#'   `dim_002`, and so on.
#'
#' @examplesIf requireNamespace("topicmodels.etm", quietly = TRUE) && requireNamespace("torch", quietly = TRUE) && torch::torch_is_installed()
#' path <- system.file(package = "topicmodels.etm", "example", "example_etm.ckpt")
#' model <- torch::torch_load(path)
#' get_term_embeddings(model)
#'
#' @export
get_term_embeddings <- function(x) {
  model <- .as_etm_model_object(x)
  emb <- as.matrix(model, type = "embedding", which = "words")
  emb <- as.matrix(emb)
  terms <- rownames(emb)
  if (is.null(terms)) {
    terms <- .stored_topic_vocab(x)
  }
  if (is.null(terms)) {
    terms <- paste0("term", seq_len(nrow(emb)))
  }
  colnames(emb) <- sprintf("dim_%03d", seq_len(ncol(emb)))

  out <- data.table::data.table(term = as.character(terms))
  cbind(out, data.table::as.data.table(emb))
}

#' Plot ETM Topic Embeddings
#'
#' Project ETM topic centers and their top associated words to a two-dimensional
#' space using the backend UMAP summary path, then visualize the result with
#' **ggplot2**.
#'
#' @param x Either an `nlp_topic_fit` with `engine = "topicmodels.etm"` or a raw
#'   ETM object.
#' @param top_n Integer. Number of top associated words to display per topic.
#'   Defaults to `15`.
#' @param ... Additional arguments forwarded to `summary(model, type = "umap", ...)`.
#'
#' @returns A [ggplot][ggplot2::ggplot] object.
#'
#' @examplesIf requireNamespace("topicmodels.etm", quietly = TRUE) && requireNamespace("torch", quietly = TRUE) && torch::torch_is_installed() && requireNamespace("uwot", quietly = TRUE)
#' path <- system.file(package = "topicmodels.etm", "example", "example_etm.ckpt")
#' model <- torch::torch_load(path)
#' plot_topic_embeddings(model, top_n = 5)
#'
#' @export
plot_topic_embeddings <- function(x, top_n = 15, ...) {
  if (!is.numeric(top_n) || length(top_n) != 1L || top_n < 1 || top_n != as.integer(top_n)) {
    stop("top_n must be a single positive integer.", call. = FALSE)
  }
  model <- .as_etm_model_object(x)
  if (!requireNamespace("uwot", quietly = TRUE)) {
    stop("Package 'uwot' is required for plot_topic_embeddings(). Please install it.", call. = FALSE)
  }
  summary_args <- list(...)
  if ("n_components" %in% names(summary_args) &&
      !identical(as.integer(summary_args$n_components), 2L)) {
    stop("plot_topic_embeddings() requires n_components = 2.", call. = FALSE)
  }
  summary_args$n_components <- 2L

  overview <- do.call(
    summary,
    c(
      list(object = model, type = "umap", top_n = as.integer(top_n)),
      summary_args
    )
  )
  embed <- data.table::as.data.table(overview$embed_2d)
  if (!all(c("type", "term", "cluster", "x", "y", "weight") %in% names(embed))) {
    stop("The ETM summary output did not contain the expected embedding columns.", call. = FALSE)
  }

  clusters <- unique(as.character(embed$cluster))
  embed[, topic_id := .topic_ids(length(clusters))[match(as.character(cluster), clusters)]]
  centers <- embed[type == "centers"]
  words <- embed[type == "words"]
  centers[, label := topic_id]

  ggplot2::ggplot() +
    ggplot2::geom_text(
      data = words,
      ggplot2::aes(x = x, y = y, label = term, colour = topic_id, alpha = weight),
      show.legend = FALSE,
      size = 3
    ) +
    ggplot2::geom_point(
      data = centers,
      ggplot2::aes(x = x, y = y, colour = topic_id),
      size = 2.5,
      show.legend = TRUE
    ) +
    ggplot2::geom_text(
      data = centers,
      ggplot2::aes(x = x, y = y, label = label, colour = topic_id),
      show.legend = FALSE,
      fontface = "bold",
      nudge_y = 0.05
    ) +
    ggplot2::scale_alpha(range = c(0.35, 1), guide = "none") +
    ggplot2::labs(
      title = "ETM Topic Embeddings",
      x = "UMAP 1",
      y = "UMAP 2",
      colour = "Topic"
    ) +
    ggplot2::theme_minimal(base_size = 12)
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
#' - `topic_max_int`
#' - `topic_max_value`
#' - `candidate_band`
#' - `topic_rank`
#'
#' Stored docvars are included when `docvars = TRUE`. Metadata columns from
#' `doc_data` and optional `text` are included when available.
#' When `docvars = FALSE`, columns that match stored docvar names are omitted
#' even if they are also present in `doc_data`.
#' Columns are ordered as `doc_id`, document metadata, representative-candidate
#' output columns, and finally `text` when text is requested and available.
#'
#' @details
#' Candidate bands are computed within each dominant topic, not globally across
#' the corpus. When within-topic quantile cut points collapse because of small
#' groups or tied values, the function falls back to deterministic rank-based
#' banding.
#'
#' @examplesIf requireNamespace("text2vec", quietly = TRUE)
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
                                          docvars = FALSE,
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
    docvars = docvars,
    include_text = include_text,
    doc_id_col = doc_id_col,
    text_col = text_col
  )
  if (!docvars) {
    out <- .drop_stored_docvars(out, x)
  }

  if (!is.null(topics)) {
    keep_topics <- .resolve_topic_selector(unique(out$topic_max_id), topics)
    out <- out[topic_max_id %in% keep_topics]
  }

  if (!nrow(out)) {
    out[, candidate_band := character()]
    out[, topic_rank := integer()]
    return(.order_representative_candidates_output(out))
  }

  out[, topic_rank := data.table::frank(-topic_max_value, ties.method = "first"),
      by = topic_max_id]
  out[, candidate_band := .assign_candidate_bands(topic_max_value, quantile_probs, labels),
      by = topic_max_id]

  data.table::setorder(out, topic_max_id, topic_rank, doc_id)
  .order_representative_candidates_output(out)
}

#' Extract Topic-Model Hyperparameters
#'
#' Return standardized hyperparameters stored on an object returned by
#' [fit_topic_model()]. The accessor uses package-level names so users do not
#' need to remember backend-specific argument names.
#'
#' @param x An object returned by [fit_topic_model()].
#'
#' @returns A [data.table][data.table::data.table] with columns:
#'   \describe{
#'     \item{`parameter`}{Standardized parameter name: `"k"`, `"alpha"`, or
#'       `"beta"`.}
#'     \item{`value`}{Stored parameter value. Scalar symmetric priors are shown
#'       as scalars; non-scalar priors are preserved as vectors. `NA` means the
#'       parameter is not available or not applicable for the fitted model.}
#'     \item{`source_section`}{Where the value came from: e.g. `"argument"`,
#'       `"model"`, `"fit"`, or `"model_object"`.}
#'     \item{`source_name`}{Backend-native argument, slot, or field name.}
#'   }
#'
#' @details
#' `alpha` is the document-topic prior and `beta` is the topic-word prior, using
#' the notation common in LDA. Engines that do not expose an equivalent prior
#' return `NA` for that row instead of dropping it, so the table has a stable
#' shape across engines. Objects created before hyperparameters were stored on
#' `nlp_topic_fit` objects return a best-effort fallback with a warning; refit
#' the model with the current package version to recover backend-native
#' hyperparameter sources.
#'
#' @seealso [fit_topic_model()]
#'
#' @examplesIf requireNamespace("text2vec", quietly = TRUE)
#' dtm <- methods::as(
#'   Matrix::Matrix(
#'     matrix(c(1, 0, 1,  1, 1, 0,  0, 1, 1,  1, 1, 1),
#'            nrow = 4, byrow = TRUE),
#'     sparse = TRUE
#'   ),
#'   "dgCMatrix"
#' )
#' colnames(dtm) <- paste0("term", 1:3)
#' fit <- fit_topic_model(
#'   dtm, engine = "text2vec", model = "lda", k = 2,
#'   control = list(
#'     model = list(doc_topic_prior = 0.1, topic_word_prior = 0.01),
#'     fit = list(n_iter = 25, progressbar = FALSE)
#'   )
#' )
#' get_topic_hyperparameters(fit)
#'
#' @export
get_topic_hyperparameters <- function(x) {
  if (!inherits(x, "nlp_topic_fit")) {
    stop("x must be an object returned by fit_topic_model().", call. = FALSE)
  }

  hp <- x$hyperparameters
  if (is.null(hp)) {
    warning(
      "x does not contain stored hyperparameters; returning a best-effort ",
      "fallback. Refit with the current package version to recover ",
      "backend-native hyperparameter sources.",
      call. = FALSE
    )
    k_info <- .fit_topic_count_source(x)
    hp <- .topic_hyperparameters_table(
      k = k_info$value,
      alpha = NA_real_,
      beta = NA_real_,
      sources = list(k = list(section = k_info$section, name = k_info$name))
    )
  }

  out <- data.table::as.data.table(hp)
  data.table::copy(out[])
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
  n_terms <- if (!is.null(x$tww)) {
    ncol(x$tww)
  } else if (!is.null(x$vocab)) {
    length(x$vocab)
  } else {
    NA_integer_
  }

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
