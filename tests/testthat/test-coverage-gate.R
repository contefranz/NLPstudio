coverage_dtm <- function() {
  x <- Matrix::Matrix(
    matrix(
      c(2, 1, 0,
        1, 0, 1,
        0, 2, 1,
        1, 1, 1),
      nrow = 4,
      byrow = TRUE
    ),
    sparse = TRUE
  )
  x <- methods::as(x, "dgCMatrix")
  rownames(x) <- paste0("doc", seq_len(nrow(x)))
  colnames(x) <- c("alpha", "beta", "gamma")
  x
}

coverage_fake_etm <- function(vocab = c("alpha", "beta", "gamma"),
                              bad_summary = FALSE) {
  words <- matrix(
    c(1, 0,
      0, 1,
      1, 1),
    nrow = 3,
    byrow = TRUE
  )
  beta <- matrix(
    c(0.7, 0.2,
      0.2, 0.7,
      0.1, 0.1),
    nrow = 3,
    byrow = TRUE
  )
  if (!is.null(vocab)) {
    rownames(beta) <- vocab
  }
  structure(
    list(
      topic_embeddings = matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE),
      word_embeddings = words,
      beta = beta,
      vocab = vocab,
      bad_summary = bad_summary
    ),
    class = c("fake_etm", "ETM")
  )
}

as.matrix.fake_etm <- function(x, type, which = NULL, ...) {
  if (identical(type, "embedding") && identical(which, "topics")) {
    return(x$topic_embeddings)
  }
  if (identical(type, "embedding") && identical(which, "words")) {
    return(x$word_embeddings)
  }
  if (identical(type, "beta")) {
    return(x$beta)
  }
  stop("unsupported fake ETM matrix request")
}

summary.fake_etm <- function(object, type, top_n, n_components, ...) {
  if (isTRUE(object$bad_summary)) {
    return(list(embed_2d = data.frame(x = 1, y = 1)))
  }
  list(embed_2d = data.frame(
    type = c("centers", "centers", "words", "words"),
    term = c("Topic 1", "Topic 2", "alpha", "beta"),
    cluster = c(1, 2, 1, 2),
    x = c(0, 1, 0.1, 0.9),
    y = c(0, 1, 0.2, 0.8),
    weight = c(1, 1, 0.8, 0.7)
  ))
}

predict.fake_etm <- function(object, newdata, type, ...) {
  stopifnot(identical(type, "topics"))
  out <- matrix(c(0.6, 0.4), nrow = nrow(newdata), ncol = 2, byrow = TRUE)
  rownames(out) <- rownames(newdata)
  out
}

registerS3method("as.matrix", "fake_etm", as.matrix.fake_etm,
                 envir = asNamespace("base"))
registerS3method("summary", "fake_etm", summary.fake_etm,
                 envir = asNamespace("base"))
registerS3method("predict", "fake_etm", predict.fake_etm,
                 envir = asNamespace("stats"))

test_that("ETM public helpers work with lightweight synthetic objects", {
  model <- coverage_fake_etm()
  fit <- structure(
    list(engine = "topicmodels.etm", model_object = model, vocab = model$vocab),
    class = c("nlp_topic_fit", "list")
  )

  topics <- get_topic_embeddings(fit)
  expect_named(topics, c("topic_id", "dim_001", "dim_002"))
  expect_equal(topics$topic_id, c("Topic001", "Topic002"))

  terms <- get_term_embeddings(fit)
  expect_named(terms, c("term", "dim_001", "dim_002"))
  expect_equal(terms$term, model$vocab)

  fallback_terms <- get_term_embeddings(coverage_fake_etm(vocab = NULL))
  expect_equal(fallback_terms$term, paste0("term", 1:3))

  expect_s3_class(plot_topic_embeddings(model, top_n = 1L), "ggplot")
  expect_error(plot_topic_embeddings(model, top_n = 0L), "top_n")
  expect_error(plot_topic_embeddings(model, n_components = 3L), "n_components = 2")
  expect_error(
    plot_topic_embeddings(coverage_fake_etm(bad_summary = TRUE)),
    "expected embedding columns"
  )
  expect_error(get_topic_embeddings(list()), "ETM fit")
  expect_identical(NLPstudio:::.as_etm_model_object(model), model)
  expect_identical(NLPstudio:::.as_etm_model_object(fit), model)
})

test_that("ETM-adjacent internals cover validation and prediction paths", {
  dtm <- coverage_dtm()

  learned <- NLPstudio:::.prepare_etm_input(dtm, list(embeddings = 2L))
  expect_equal(learned$model_control$vocab, colnames(dtm))
  expect_equal(learned$model_control$embeddings, 2L)

  reordered <- NLPstudio:::.prepare_etm_input(
    dtm,
    list(embeddings = 2L, vocab = rev(colnames(dtm)))
  )
  expect_equal(reordered$term_names, rev(colnames(dtm)))

  pretrained <- matrix(seq_len(4), nrow = 2,
                       dimnames = list(c("gamma", "alpha"), NULL))
  aligned <- suppressWarnings(
    NLPstudio:::.prepare_etm_input(dtm, list(embeddings = pretrained))
  )
  expect_equal(aligned$term_names, c("gamma", "alpha"))
  expect_type(aligned$model_control$embeddings, "double")

  nameless <- dtm
  colnames(nameless) <- NULL
  expect_error(NLPstudio:::.prepare_etm_input(nameless, list(embeddings = 2L)),
               "term names")
  expect_error(NLPstudio:::.prepare_etm_input(dtm, list()), "embeddings")
  expect_error(
    NLPstudio:::.prepare_etm_input(dtm, list(embeddings = matrix(1:4, nrow = 2))),
    "rownames"
  )
  expect_error(
    NLPstudio:::.prepare_etm_input(
      dtm,
      list(embeddings = pretrained, vocab = c("alpha", "gamma"))
    ),
    "must match the embedding rownames"
  )
  expect_error(
    NLPstudio:::.prepare_etm_input(
      dtm,
      list(embeddings = matrix(1:4, nrow = 2,
                               dimnames = list(c("delta", "epsilon"), NULL)))
    ),
    "No overlap"
  )
  expect_error(NLPstudio:::.prepare_etm_input(dtm, list(embeddings = 0L)),
               "single positive integer")
  expect_error(
    NLPstudio:::.prepare_etm_input(dtm, list(embeddings = 2L, vocab = "alpha")),
    "length equal"
  )
  expect_error(
    NLPstudio:::.prepare_etm_input(
      dtm,
      list(embeddings = 2L, vocab = c("alpha", "alpha", "gamma"))
    ),
    "match the input terms"
  )

  fit <- list(engine = "topicmodels.etm", model_object = coverage_fake_etm())
  pred <- NLPstudio:::.predict_topic_matrix(
    fit,
    list(sparse = dtm[1:2, , drop = FALSE], doc_ids = rownames(dtm)[1:2]),
    control = list(normalize = TRUE)
  )
  expect_equal(colnames(pred), c("Topic001", "Topic002"))

  control <- list(model = list(embeddings = 2L), fit = list(), optimizer = list())
  if (!requireNamespace("topicmodels.etm", quietly = TRUE)) {
    expect_error(NLPstudio:::.fit_etm_topic_model(dtm, k = 2L, control = control),
                 "topicmodels.etm")
  } else if (!requireNamespace("torch", quietly = TRUE)) {
    expect_error(NLPstudio:::.fit_etm_topic_model(dtm, k = 2L, control = control),
                 "torch")
  }
})

test_that("ETM original-fit wrapper preserves sparse split arguments", {
  dtm <- coverage_dtm()
  model <- list(
    fit_original = function(data, test1, test2, optimizer, epoch, batch_size,
                            normalize, clip, lr_anneal_factor,
                            lr_anneal_nonmono) {
      expect_s4_class(data, "dgCMatrix")
      expect_s4_class(test1, "dgCMatrix")
      expect_s4_class(test2, "dgCMatrix")
      data.table::data.table(epoch = epoch, batch_size = batch_size)
    }
  )
  fit_args <- list(
    data = dtm,
    optimizer = "optimizer",
    epoch = 2L,
    batch_size = 2L,
    normalize = TRUE,
    clip = 0,
    lr_anneal_factor = 4,
    lr_anneal_nonmono = 10
  )

  testthat::with_mocked_bindings(
    getFromNamespace = function(x, ns, ...) {
      expect_equal(x, "as_tokencounts")
      expect_equal(ns, "topicmodels.etm")
      identity
    },
    .package = "utils",
    {
      loss <- NLPstudio:::.fit_etm_model_original(model, fit_args)
    }
  )
  expect_equal(loss$epoch, 2L)

  too_small <- dtm[1:2, , drop = FALSE]
  expect_error(
    NLPstudio:::.fit_etm_model_original(model, utils::modifyList(fit_args, list(data = too_small))),
    "at least 3"
  )
})

test_that("topic extraction and coercion helpers cover cached and raw inputs", {
  dtm <- coverage_dtm()
  dfm <- quanteda::as.dfm(dtm)
  quanteda::docvars(dfm, "year") <- 2020:2023

  dtw <- matrix(c(0.8, 0.2, 0.3, 0.7), nrow = 2, byrow = TRUE)
  rownames(dtw) <- c("doc1", "doc2")
  tww <- matrix(c(0.5, 0.5, 0.2, 0.8), nrow = 2, byrow = TRUE)
  colnames(tww) <- c("alpha", "beta")
  fit <- structure(
    list(dtw = dtw, tww = tww, doc_ids = rownames(dtw), vocab = colnames(tww)),
    class = c("nlp_topic_fit", "list")
  )

  expect_equal(NLPstudio:::.extract_dtw_table(fit)$doc_id, c("doc1", "doc2"))
  expect_equal(NLPstudio:::.extract_tww_table(fit)$topic_id, c("Topic001", "Topic002"))

  existing_dtw <- data.table::data.table(
    rn = c("doc1", "doc2"),
    beta = c(0.2, 0.9),
    alpha = c(0.8, 0.1),
    topic_max_id = "old"
  )
  coerced_dtw <- NLPstudio:::.coerce_existing_dtw_table(existing_dtw)
  expect_named(
    coerced_dtw,
    c("doc_id", "Topic001", "Topic002", "topic_max_id",
      "topic_max_int", "topic_max_value")
  )

  existing_tww <- data.table::data.table(topic_id = c("a", "b"), alpha = 1:2)
  expect_equal(
    NLPstudio:::.coerce_existing_tww_table(existing_tww)$topic_id,
    c("Topic001", "Topic002")
  )
  expect_error(NLPstudio:::.coerce_existing_tww_table(data.frame(alpha = 1)),
               "topic_id")
  expect_error(NLPstudio:::.coerce_existing_dtw_table(data.frame(alpha = 1)),
               "doc_id")
  expect_error(
    NLPstudio:::.coerce_existing_dtw_table(data.frame(doc_id = "doc1", label = "x")),
    "No topic columns"
  )

  fake_etm <- coverage_fake_etm()
  expect_error(NLPstudio:::.extract_dtw_table(fake_etm), "Raw ETM")
  expect_equal(NLPstudio:::.extract_tww_table(fake_etm)$topic_id,
               c("Topic001", "Topic002"))

  warp <- structure(
    list(topic_word_distribution = matrix(c(0.6, 0.4, 0.3, 0.7), nrow = 2)),
    class = "WarpLDA"
  )
  colnames(warp$topic_word_distribution) <- c("alpha", "beta")
  expect_error(NLPstudio:::.extract_dtw_table(warp), "WarpLDA")
  expect_equal(names(NLPstudio:::.extract_tww_table(warp)),
               c("topic_id", "alpha", "beta"))
  expect_error(NLPstudio:::.extract_tww_table(list()), "unrecognized")

  textmodel <- structure(
    list(theta = dtw, phi = tww, data = dfm[1:2, ]),
    class = "textmodel"
  )
  rownames(textmodel$theta) <- NULL
  expect_equal(NLPstudio:::.extract_dtw_table(textmodel)$doc_id,
               quanteda::docnames(dfm[1:2, ]))
  expect_equal(NLPstudio:::.extract_tww_table(textmodel)$topic_id,
               c("Topic001", "Topic002"))
  sequential_textmodel <- structure(
    list(theta = `rownames<-`(dtw, NULL), phi = tww),
    class = "textmodel"
  )
  expect_equal(NLPstudio:::.extract_dtw_table(sequential_textmodel)$doc_id,
               c("1", "2"))

  expect_s4_class(NLPstudio:::.as_topic_dgCMatrix(dfm), "dgCMatrix")
  expect_true(inherits(NLPstudio:::.as_topic_dfm(dtm), "dfm"))
  expect_error(NLPstudio:::.as_topic_dgCMatrix(data.frame(x = 1)), "topic modeling")
  expect_error(NLPstudio:::.as_topic_dfm(data.frame(x = 1)), "topic modeling")
  expect_equal(NLPstudio:::.matrix_doc_ids(`rownames<-`(matrix(1, nrow = 1), NULL)),
               "1")
  expect_equal(
    colnames(NLPstudio:::.tww_matrix_from_matrix(matrix(log(1:4), nrow = 2),
                                                 log_scale = TRUE)),
    c("term1", "term2")
  )

  skip_if_not_installed("topicmodels")
  topicmodels_input <- quanteda::convert(dfm, to = "topicmodels")
  expect_identical(NLPstudio:::.as_topicmodels_input(topicmodels_input),
                   topicmodels_input)
  expect_s4_class(NLPstudio:::.as_topic_dgCMatrix(topicmodels_input), "dgCMatrix")
  expect_true(inherits(NLPstudio:::.as_topic_dfm(topicmodels_input), "dfm"))

  tm_fit <- topicmodels::LDA(
    topicmodels_input,
    k = 2L,
    method = "Gibbs",
    control = list(seed = 1L, iter = 10L, burnin = 0L, thin = 1L)
  )
  rownames(tm_fit@gamma) <- NULL
  expect_equal(NLPstudio:::.topicmodels_doc_ids(tm_fit), tm_fit@documents)
  expect_equal(NLPstudio:::.extract_dtw_table(tm_fit)$doc_id, tm_fit@documents)
  expect_equal(NLPstudio:::.extract_tww_table(tm_fit)$topic_id,
               c("Topic001", "Topic002"))
})

test_that("metadata and stored topic fallbacks are deterministic", {
  corp <- quanteda::corpus(c(doc1 = "alpha beta", doc2 = "gamma delta"))
  quanteda::docvars(corp, "year") <- c(2020L, 2021L)
  dfm <- quanteda::dfm(quanteda::tokens(corp))

  docvars <- NLPstudio:::.docvars_table_from_input(corp, c("doc2", "doc1"))
  expect_equal(docvars$doc_id, c("doc2", "doc1"))
  expect_equal(docvars$year, c(2021L, 2020L))
  expect_equal(
    NLPstudio:::.docvars_table_from_input(dfm, c("doc1", "doc2"))$year,
    c(2020L, 2021L)
  )

  corpus_meta <- NLPstudio:::.normalize_doc_data_table(corp, include_text = TRUE)
  expect_equal(corpus_meta$text, unname(as.character(corp)))

  tabular <- data.frame(key = "doc1", body = "text body", group = "a")
  renamed <- NLPstudio:::.normalize_doc_data_table(
    tabular,
    include_text = TRUE,
    doc_id_col = "key",
    text_col = "body"
  )
  expect_named(renamed, c("doc_id", "text", "group"))
  dropped_text <- NLPstudio:::.normalize_doc_data_table(tabular, include_text = FALSE,
                                                       doc_id_col = "key",
                                                       text_col = "body")
  expect_false("body" %in% names(dropped_text))
  expect_error(NLPstudio:::.normalize_doc_data_table(data.frame(id = "doc1"), FALSE),
               "doc_id")
  expect_error(NLPstudio:::.normalize_doc_data_table("bad", FALSE), "doc_data")
  expect_null(NLPstudio:::.normalize_doc_data_table(NULL, FALSE))

  fit_without_docvars <- structure(list(docvars = NULL), class = c("nlp_topic_fit", "list"))
  expect_null(NLPstudio:::.stored_docvars_table(fit_without_docvars, "doc1"))

  textmodel <- structure(list(data = dfm), class = "textmodel")
  expect_equal(NLPstudio:::.stored_docvars_table(textmodel, c("doc1", "doc2"))$year,
               c(2020L, 2021L))

  stored_fit <- structure(
    list(doc_data = data.table::data.table(doc_id = "doc1", text = "stored")),
    class = c("nlp_topic_fit", "list")
  )
  expect_equal(
    NLPstudio:::.resolved_doc_data_table(stored_fit, NULL, TRUE, "doc_id", "text")$text,
    "stored"
  )
  expect_equal(
    NLPstudio:::.resolved_doc_data_table(
      list(),
      data.frame(key = "doc1", body = "explicit"),
      TRUE,
      "key",
      "body"
    )$text,
    "explicit"
  )
  data_textmodel <- structure(
    list(data = data.frame(doc_id = "doc1", text = "from model")),
    class = "textmodel"
  )
  expect_equal(
    NLPstudio:::.resolved_doc_data_table(data_textmodel, NULL, TRUE, "doc_id", "text")$text,
    "from model"
  )

  tww <- matrix(1:4, nrow = 2)
  colnames(tww) <- c("alpha", "beta")
  expect_equal(
    NLPstudio:::.stored_topic_vocab(structure(list(tww = tww),
                                             class = c("nlp_topic_fit", "list"))),
    c("alpha", "beta")
  )
  fake_etm <- coverage_fake_etm()
  expect_equal(
    NLPstudio:::.stored_topic_vocab(structure(
      list(model_object = fake_etm),
      class = c("nlp_topic_fit", "list")
    )),
    fake_etm$vocab
  )
  expect_equal(NLPstudio:::.stored_topic_vocab(fake_etm), fake_etm$vocab)
  expect_error(
    NLPstudio:::.stored_topic_vocab(structure(list(), class = c("nlp_topic_fit", "list"))),
    "stored vocabulary"
  )

  count_from_tww <- NLPstudio:::.fit_topic_count_source(list(tww = tww))
  expect_equal(count_from_tww$value, 2L)
  expect_equal(NLPstudio:::.fit_topic_count(list(tww = tww)), 2L)
  count_from_model <- NLPstudio:::.fit_topic_count_source(list(model_object = list(k = 3L)))
  expect_equal(count_from_model$name, "k")
  expect_true(is.na(NLPstudio:::.fit_topic_count_source(list())$value))

  base <- data.table::data.table(doc_id = "doc1", Topic001 = 1)
  meta <- data.table::data.table(doc_id = "doc1", Topic001 = 99)
  expect_warning(
    unchanged <- NLPstudio:::.bind_topic_metadata(base, meta),
    "Topic001"
  )
  expect_identical(unchanged, base)
})

test_that("topic model argument and control validation covers rare branches", {
  expect_equal(
    NLPstudio:::.normalize_topic_control(list(model = NULL, fit = NULL, optimizer = NULL)),
    list(model = list(), fit = list(), optimizer = list())
  )

  control <- list(model = list(), fit = list(), optimizer = list())
  expect_error(
    NLPstudio:::.validate_topic_fit_args(
      "seededlda", "seededlda", NULL, NULL, control, NULL, NULL, NULL
    ),
    "dictionary must be supplied"
  )
  expect_error(
    NLPstudio:::.validate_topic_fit_args(
      "seededlda", "lda", NULL, 2L, control,
      quanteda::dictionary(list(a = "x")), NULL, NULL
    ),
    "dictionary is only valid"
  )
  expect_error(
    NLPstudio:::.validate_topic_fit_args(
      "text2vec", "lda", NULL, 2L, control, NULL, NULL, list()
    ),
    "initial_model"
  )
  expect_error(
    NLPstudio:::.validate_topic_fit_args(
      "seededlda", "seededlda", NULL, NULL, control,
      quanteda::dictionary(list(a = "x")), NULL, list()
    ),
    "initial_model"
  )
  expect_error(
    NLPstudio:::.validate_topic_fit_args(
      "topicmodels.etm", "etm", NULL, 2L, control, NULL, NULL, list()
    ),
    "initial_model"
  )
})

test_that("backend-control sanitizer summarizes additional object families", {
  dtm <- coverage_dtm()
  dfm <- quanteda::as.dfm(dtm)

  expect_equal(
    NLPstudio:::.sanitize_backend_control_value(dtm),
    list(class = "dgCMatrix", dim = dim(dtm))
  )
  expect_null(NLPstudio:::.sanitize_backend_control_value(NULL))
  expect_equal(
    NLPstudio:::.sanitize_backend_control_value(dfm),
    list(class = class(dfm)[1L], dim = dim(dfm))
  )

  if (!methods::isClass("CoverageControlS4")) {
    methods::setClass("CoverageControlS4", slots = c(value = "numeric"))
  }
  s4_value <- methods::new("CoverageControlS4", value = 1)
  expect_equal(NLPstudio:::.sanitize_backend_control_value(s4_value)$value, 1)

  skip_if_not_installed("topicmodels")
  topicmodels_input <- quanteda::convert(dfm, to = "topicmodels")
  expect_equal(
    NLPstudio:::.sanitize_backend_control_value(topicmodels_input),
    list(class = class(topicmodels_input)[1L], dim = dim(topicmodels_input))
  )
  fit <- topicmodels::LDA(
    topicmodels_input,
    k = 2L,
    method = "Gibbs",
    control = list(seed = 1L, iter = 10L, burnin = 0L, thin = 1L)
  )
  expect_match(NLPstudio:::.sanitize_backend_control_value(fit), "LDA")
})

test_that("representative candidates and topic fit printing cover fallback branches", {
  empty_dtw <- data.table::data.table(doc_id = character(), Topic001 = numeric())
  empty <- get_representative_candidates(empty_dtw)
  expect_named(empty, c("doc_id", "topic_max_id", "topic_max_int",
                        "topic_max_value", "candidate_band", "topic_rank"))

  fit <- structure(
    list(
      engine = "topicmodels",
      model = "lda",
      method = "Gibbs",
      doc_ids = c("doc1", "doc2"),
      dtw = NULL,
      tww = matrix(1:4, nrow = 2),
      vocab = NULL,
      docvars = NULL,
      doc_data = NULL
    ),
    class = c("nlp_topic_fit", "list")
  )
  out <- utils::capture.output(ret <- print(fit))
  expect_true(any(grepl("Gibbs", out)))
  expect_true(any(grepl("topics: 2", out)))
  expect_identical(ret, fit)

  vocab_fit <- fit
  vocab_fit$tww <- NULL
  vocab_fit$vocab <- c("alpha", "beta", "gamma")
  out <- utils::capture.output(print(vocab_fit))
  expect_true(any(grepl("terms: 3", out)))
})

test_that("JSON family inference and postprocessing cover edge cases", {
  expect_equal(
    NLPstudio:::.infer_json_family(data.table::data.table(item_1 = "x"), "8-K"),
    "8-K"
  )
  expect_equal(
    NLPstudio:::.infer_json_family(data.table::data.table(filing_type = "10-Q"), NULL),
    "10-Q"
  )
  expect_equal(
    NLPstudio:::.infer_json_family(data.table::data.table(filing_type = "8-K"), NULL),
    "8-K"
  )
  expect_equal(
    NLPstudio:::.infer_json_family(data.table::data.table(item_1.01 = "x"), NULL),
    "8-K"
  )
  expect_equal(NLPstudio:::.infer_json_family(data.table::data.table(other = "x"), NULL),
               "10-K")
  expect_equal(NLPstudio:::.measure_vars_for_what("unknown", "bad"), character())

  skipped <- NLPstudio:::.melt_json_record(
    data.table::data.table(cik = "1", other = "x"),
    what = "10-K",
    drop_empty_text = TRUE
  )
  expect_true(skipped$skipped)
  expect_equal(nrow(skipped$data), 0L)

  out <- data.table::data.table(
    cik = "123",
    sic = c("1234", "12A4"),
    filing_date = c("2024-03-01", "2025-03-01"),
    period_of_report = c("2023-12-31", "2022-12-31"),
    item = c("item_1", "item_1"),
    text = c("on time", "late")
  )
  processed <- NLPstudio:::.postprocess_json_dt(out, drop_late_filers = TRUE)
  expect_equal(nrow(processed), 1L)
  expect_type(processed$cik, "integer")
  expect_true(is.integer(processed$sic))
  expect_equal(processed$fyear, 2023L)

  no_period <- data.table::data.table(
    filing_date = "2024-03-01",
    item = "item_1",
    text = "text"
  )
  expect_equal(NLPstudio:::.postprocess_json_dt(no_period, FALSE)$fyear, 2024L)
  expect_equal(nrow(NLPstudio:::.postprocess_json_dt(data.table::data.table(), TRUE)), 0L)
})

test_that("from_json_to_df validates inputs and exposes warning branches", {
  skip_if_not_installed("RcppSimdJson")
  expect_error(from_json_to_df(1), "character vector")
  expect_error(from_json_to_df(character(), drop_empty_text = NA), "drop_empty_text")

  path <- tempfile(fileext = ".json")
  writeLines(c("{", '  "cik": "1",', '  "filing_type": "10-K"', "}"), path)
  expect_warning(
    out <- from_json_to_df(path, ncores = 1L, max_chunk_size = 1L),
    "Skipped 1 JSON"
  )
  expect_equal(nrow(out), 0L)

  dated <- tempfile(fileext = ".json")
  writeLines(c(
    "{",
    '  "cik": "1",',
    '  "filing_type": "10-K",',
    '  "filing_date": "2025-03-01",',
    '  "period_of_report": "2022-12-31",',
    '  "item_1": "late filing"',
    "}"
  ), dated)
  dropped <- from_json_to_df(dated, ncores = 1L, drop_late_filers = TRUE)
  expect_equal(nrow(dropped), 0L)

  first <- tempfile(fileext = ".json")
  second <- tempfile(fileext = ".json")
  writeLines(c("{", '  "item_1": "first"', "}"), first)
  writeLines(c("{", '  "item_1": "second"', "}"), second)

  testthat::with_mocked_bindings(
    .run_parallel = function(chunks, FUN, ncores, socket, export_vars = NULL,
                             export_env = parent.frame(), ...) {
      lapply(chunks, FUN, ...)
    },
    .package = "NLPstudio",
    {
      read_out <- NLPstudio:::.parallel_read_json(c(first, second), 2L, "PSOCK")
      expect_equal(length(read_out), 2L)
      melt_out <- NLPstudio:::.parallel_melt(read_out, 2L, "PSOCK", NULL, TRUE)
      expect_equal(unname(vapply(melt_out, `[[`, logical(1), "skipped")),
                   c(FALSE, FALSE))
    }
  )
})

test_that("selection and evaluation helpers cover summary edge cases", {
  duplicated <- data.table::data.table(
    k = c(2L, 3L),
    metric = c("train_nll", "train_nll"),
    level = c("aggregate", "aggregate"),
    topic_id = c(NA_character_, NA_character_),
    value = c(1, 1),
    supported = c(TRUE, TRUE)
  )
  data.table::setattr(duplicated, "class", c("nlp_k_selection", "data.table", "data.frame"))
  out <- utils::capture.output(print(duplicated))
  expect_true(any(grepl("\\[tied\\]", out)))

  unsupported <- data.table::copy(duplicated)
  unsupported$supported <- FALSE
  out <- utils::capture.output(ret <- print(unsupported))
  expect_true(any(grepl("No supported aggregate", out)))
  expect_identical(ret, unsupported)
  expect_error(plot(unsupported), "No supported aggregate")
  expect_s3_class(plot(duplicated, metrics = "train_nll"), "ggplot")

  skip_if_not_installed("text2vec")
  expect_warning(
    select_k_topics(
      coverage_dtm(),
      engine = "text2vec",
      model = "lda",
      k_grid = c(2L, 2L),
      metrics = "diversity",
      holdout = 0,
      control = list(fit = list(n_iter = 25L, progressbar = FALSE))
    ),
    "Duplicate"
  )
})

test_that("evaluation matrices fall back through cached TWW and prediction", {
  tww <- matrix(c(0.7, 0.3, 0.2, 0.8), nrow = 2, byrow = TRUE)
  rownames(tww) <- c("Topic001", "Topic002")
  colnames(tww) <- c("alpha", "beta")
  dtw <- matrix(c(0.6, 0.4, 0.3, 0.7), nrow = 2, byrow = TRUE)
  rownames(dtw) <- c("doc1", "doc2")
  colnames(dtw) <- rownames(tww)

  fit <- structure(
    list(tww = tww, dtw = dtw, vocab = colnames(tww)),
    class = c("nlp_topic_fit", "list")
  )
  expect_identical(NLPstudio:::.eval_tww_matrix(fit), tww)
  expect_equal(NLPstudio:::.eval_topic_ids(fit), rownames(tww))

  tww_table_fit <- structure(
    list(tww = NULL, model_object = data.table::data.table(topic_id = rownames(tww),
                                                           alpha = tww[, 1],
                                                           beta = tww[, 2])),
    class = c("nlp_topic_fit", "list")
  )
  expect_equal(rownames(NLPstudio:::.eval_tww_matrix(tww_table_fit)), rownames(tww))
  expect_equal(NLPstudio:::.eval_topic_ids(structure(list(), class = c("nlp_topic_fit", "list"))),
               character())

  aligned <- list(sparse = coverage_dtm()[1:2, c("alpha", "beta"), drop = FALSE],
                  doc_ids = c("doc1", "doc2"))
  expect_equal(NLPstudio:::.training_topic_matrix(fit, aligned), dtw)

  empty <- coverage_dtm()
  colnames(empty) <- c("delta", "epsilon", "zeta")
  expect_error(
    suppressWarnings(
      NLPstudio:::.metric_likelihood_nll(
        fit,
        empty,
        epsilon = 1e-12,
        which_metrics = "train_nll",
        sample = "training"
      )
    ),
    "No documents remain"
  )

  no_match <- fit
  rownames(no_match$dtw) <- c("other1", "other2")
  fallback <- NLPstudio:::.training_topic_matrix(no_match, aligned)
  expect_equal(rownames(fallback), aligned$doc_ids)

  no_cache <- fit
  no_cache$dtw <- NULL
  no_cache$engine <- "topicmodels.etm"
  no_cache$model_object <- coverage_fake_etm()
  predicted <- NLPstudio:::.training_topic_matrix(no_cache, aligned)
  expect_equal(colnames(predicted), c("Topic001", "Topic002"))
})

test_that(".run_parallel covers PSOCK worker execution and fallback", {
  chunks <- list(1L, 2L)
  testthat::with_mocked_bindings(
    makeCluster = function(ncores) structure(list(ncores = ncores), class = "coverage_cluster"),
    stopCluster = function(cl) invisible(NULL),
    clusterExport = function(cl, varlist, envir) invisible(NULL),
    clusterApplyLB = function(cl, x, fun, ...) lapply(x, fun, ...),
    .package = "parallel",
    {
      psock <- NLPstudio:::.run_parallel(
        chunks,
        function(x, inc) x + inc,
        ncores = 2L,
        socket = "PSOCK",
        export_vars = "unused",
        inc = 1L
      )
    }
  )
  expect_equal(psock, list(2L, 3L))

  result <- suppressWarnings(
    NLPstudio:::.run_parallel(
      chunks,
      function(x, inc) x + inc,
      ncores = 2L,
      socket = "PSOCK",
      inc = 1L
    )
  )
  expect_equal(result, list(2L, 3L))
})

test_that("tokenization covers user parameters and parallel assembly", {
  corp <- quanteda::corpus(c(doc1 = "Alpha, beta.", doc2 = "Gamma delta."))
  toks <- tokenize_corpus(corp, ncores = 1L, remove_punct = TRUE)
  expect_equal(quanteda::ndoc(toks), 2L)

  testthat::with_mocked_bindings(
    .run_parallel = function(chunks, FUN, ncores, socket, export_vars = NULL,
                             export_env = parent.frame(), ...) {
      lapply(chunks, FUN, ...)
    },
    .package = "NLPstudio",
    {
      parallel_toks <- tokenize_corpus(corp, ncores = 2L, nchunks = 2L,
                                       socket = "PSOCK")
    }
  )
  expect_equal(quanteda::docnames(parallel_toks), quanteda::docnames(corp))
})

test_that("similarity and distance cover default and y-input branches", {
  corp <- quanteda::corpus(c(doc1 = "cat dog", doc2 = "cat bird", doc3 = "dog bird"))
  dfm <- quanteda::dfm(quanteda::tokens(corp))

  expect_s4_class(calculate_similarity(dfm, ncores = 1L), "textstat_simil_symm")
  expect_s4_class(
    calculate_similarity(dfm, ncores = 1L, y = dfm[1, ], margin = "documents"),
    "textstat_simil"
  )
  expect_s4_class(calculate_distance(dfm, ncores = 1L), "textstat_dist_symm")
  expect_s4_class(
    calculate_distance(dfm, ncores = 1L, y = dfm[1, ], margin = "documents"),
    "textstat_dist"
  )
})
