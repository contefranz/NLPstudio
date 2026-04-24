if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      "candidate_band", "cluster", "dim_001", "dim_002", "doc_id", "label", "probability",
      "rank", "term", "text", "topic", "topic_id", "topic_max_id",
      "topic_max_int", "topic_max_value", "topic_rank", "type", "weight", "x", "y"
    )
  )
}

#' Fit a Topic Model Across Supported Backends
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
#' @param k Number of topics. Required for all supported models except
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
#' [Theory-Driven Analysis of Large Corpora: Semisupervised Topic Classification of the UN Speeches](https://journals.sagepub.com/doi/full/10.1177/0894439320907027).
#' _Social Science Computer Review_, 40(2), 346-366.
#'
#' Watanabe, K., & Baturo, A. (2024).
#' [Seeded Sequential LDA: A Semi-Supervised Algorithm for Topic-Specific Analysis of Sentences](https://journals.sagepub.com/doi/10.1177/08944393231178605).
#' _Social Science Computer Review_, 42(1), 224-248.
#' 
#' @seealso [topicmodels::LDA()] [topicmodels::CTM()] [text2vec::LDA()] [seededlda::textmodel_seqlda()]
#' [topicmodels.etm::ETM()]
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
#' @examples
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
  if (!is.logical(docvars) || length(docvars) != 1L) {
    stop("docvars must be a single TRUE/FALSE value.")
  }
  if (!is.logical(include_text) || length(include_text) != 1L) {
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
#' get_dtw(fit, docvars = TRUE)
#'
#' @export
get_dtw <- function(x, doc_data = NULL, docvars = FALSE, include_text = FALSE,
                    doc_id_col = "doc_id", text_col = "text") {
  if (!is.logical(docvars) || length(docvars) != 1L) {
    stop("docvars must be a single TRUE/FALSE value.")
  }
  if (!is.logical(include_text) || length(include_text) != 1L) {
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

#' @keywords internal
.new_nlp_topic_fit <- function(engine, model, method, model_object, dtw, tww,
                               doc_ids, vocab, docvars, doc_data, call) {
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
    seededlda = c("lda", "seqlda", "seededlda"),
    `topicmodels.etm` = "etm"
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

#' @keywords internal
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
}

#' @keywords internal
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
  NULL
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

  do.call(model_object$fit, fit_args)

  dtw <- stats::predict(
    model_object,
    newdata = prep$x,
    type = "topics",
    batch_size = as.integer(min(fit_args$batch_size, nrow(prep$x))),
    normalize = if ("normalize" %in% names(fit_args)) fit_args$normalize else TRUE
  )
  tww <- as.matrix(model_object, type = "beta")

  list(
    model_object = model_object,
    dtw = dtw,
    tww = tww,
    doc_ids = prep$doc_ids,
    term_names = prep$term_names,
    method = NULL
  )
}

#' @keywords internal
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
    }
  )

  .dtw_matrix_from_matrix(out, doc_ids = newdata_aligned$doc_ids)
}

#' @keywords internal
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
      if (grepl("overwritten by the fitted model", conditionMessage(w), fixed = TRUE)) {
        tryInvokeRestart("muffleWarning")
      }
    }
  )

  predicted$theta
}

#' @keywords internal
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

#' @keywords internal
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

#' @keywords internal
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

  if (.is_etm_object(x)) {
    beta <- as.matrix(x, type = "beta")
    return(.tww_dt_from_matrix(
      beta,
      term_names = if (!is.null(colnames(beta))) colnames(beta) else .stored_topic_vocab(x)
    ))
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

#' @keywords internal
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

#' @keywords internal
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

#' @keywords internal
.dtw_output_columns <- function(x) {
  topic_cols <- .find_topic_columns(x, id_col = "doc_id")
  intersect(c(topic_cols, "topic_max_id", "topic_max_int", "topic_max_value"), names(x))
}

#' @keywords internal
.representative_candidates_output_columns <- function(x) {
  intersect(
    c(.dtw_output_columns(x), "candidate_band", "topic_rank"),
    names(x)
  )
}

#' @keywords internal
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

#' @keywords internal
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

#' @keywords internal
.order_document_topic_output <- function(x, output_cols) {
  text_cols <- intersect("text", names(x))
  metadata_cols <- setdiff(names(x), c("doc_id", output_cols, text_cols))
  data.table::setcolorder(
    x,
    c("doc_id", metadata_cols, output_cols, text_cols)
  )
  x[]
}

#' @keywords internal
.order_dtw_output <- function(x) {
  .order_document_topic_output(x, .dtw_output_columns(x))
}

#' @keywords internal
.order_representative_candidates_output <- function(x) {
  .order_document_topic_output(x, .representative_candidates_output_columns(x))
}

#' @keywords internal
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
.is_etm_object <- function(x) {
  inherits(x, "ETM")
}

#' @keywords internal
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
