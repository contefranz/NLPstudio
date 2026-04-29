test_that("topic selectors resolve numeric and character inputs", {
  topic_ids <- c("Topic001", "Topic002", "Topic003")

  expect_equal(NLPstudio:::.resolve_topic_selector(topic_ids, NULL), topic_ids)
  expect_equal(NLPstudio:::.resolve_topic_selector(topic_ids, c(3, 1)), c("Topic003", "Topic001"))
  expect_equal(NLPstudio:::.resolve_topic_selector(topic_ids, "Topic002"), "Topic002")
  expect_error(NLPstudio:::.resolve_topic_selector(topic_ids, 0), "positive integer")
  expect_error(NLPstudio:::.resolve_topic_selector(topic_ids, 4), "not available")
  expect_error(NLPstudio:::.resolve_topic_selector(topic_ids, "Topic999"), "not available")
  expect_error(NLPstudio:::.resolve_topic_selector(topic_ids, TRUE), "topics must be NULL")
})

test_that("topic model and method normalizers reject invalid requests", {
  expect_equal(NLPstudio:::.match_topic_model("text2vec", "LDA"), "lda")
  expect_error(NLPstudio:::.match_topic_model("text2vec", NA_character_), "model must be")
  expect_error(NLPstudio:::.match_topic_model("text2vec", "ctm"), "Unsupported model")

  expect_equal(NLPstudio:::.normalize_topic_method("topicmodels", "lda", "gibbs"), "Gibbs")
  expect_equal(NLPstudio:::.normalize_topic_method("topicmodels", "ctm", "VEM"), "VEM")
  expect_error(
    NLPstudio:::.normalize_topic_method("topicmodels", "lda", c("VEM", "Gibbs")),
    "method must be NULL"
  )
  expect_error(
    NLPstudio:::.normalize_topic_method("topicmodels", "lda", "bad"),
    "method must be 'VEM' or 'Gibbs'"
  )
  expect_error(
    NLPstudio:::.normalize_topic_method("text2vec", "lda", "VEM"),
    "method must be NULL"
  )
})

test_that("exported topic helpers validate scalar logical arguments early", {
  x <- matrix(1, nrow = 1, ncol = 1)
  fit <- structure(
    list(vocab = "term1", dtw = matrix(1, nrow = 1, ncol = 1)),
    class = c("nlp_topic_fit", "list")
  )
  dtw <- data.table::data.table(doc_id = "doc1", Topic001 = 1)

  expect_error(fit_topic_model(x, "text2vec", "lda", k = 0), "k must be NULL")
  expect_error(fit_topic_model(x, "text2vec", "lda", k = 1, docvars = "yes"), "docvars")
  expect_error(fit_topic_model(x, "text2vec", "lda", k = 1, return_dtw = NA), "return_dtw")
  expect_error(fit_topic_model(x, "text2vec", "lda", k = 1, return_tww = c(TRUE, FALSE)), "return_tww")

  expect_error(predict_topic_model(list(), x), "fit_topic_model")
  expect_error(predict_topic_model(fit, x, docvars = "yes"), "docvars")
  expect_error(predict_topic_model(fit, x, include_text = c(TRUE, FALSE)), "include_text")

  expect_error(get_dtw(dtw, docvars = "yes"), "docvars")
  expect_error(get_dtw(dtw, include_text = c(TRUE, FALSE)), "include_text")
})

test_that("topic column discovery handles canonical and numeric fallback columns", {
  canonical <- data.table::data.table(
    doc_id = "doc1",
    Topic010 = 0.2,
    Topic002 = 0.8,
    label = "x"
  )
  fallback <- data.table::data.table(
    doc_id = "doc1",
    alpha = 0.2,
    beta = 0.8
  )
  none <- data.table::data.table(
    doc_id = "doc1",
    alpha = "x",
    beta = 0.8
  )

  expect_equal(NLPstudio:::.find_topic_columns(canonical, id_col = "doc_id"), c("Topic002", "Topic010"))
  expect_equal(NLPstudio:::.find_topic_columns(fallback, id_col = "doc_id"), c("alpha", "beta"))
  expect_equal(NLPstudio:::.find_topic_columns(none, id_col = "doc_id"), character())
})

test_that("prediction controls and hyperparameter values normalize predictably", {
  expect_equal(NLPstudio:::.normalize_prediction_control(NULL), list())
  expect_error(NLPstudio:::.normalize_prediction_control("bad"), "control must be a named list")
  expect_error(NLPstudio:::.normalize_prediction_control(list(1)), "control must be a named list")
  expect_equal(NLPstudio:::.normalize_prediction_control(list(batch_size = 2)), list(batch_size = 2))

  expect_true(is.na(NLPstudio:::.normalize_hyperparameter_value(NULL)))
  expect_true(is.na(NLPstudio:::.normalize_hyperparameter_value(numeric())))
  expect_true(is.na(NLPstudio:::.normalize_hyperparameter_value(NA_real_)))
  expect_equal(NLPstudio:::.normalize_hyperparameter_value("asymmetric"), "asymmetric")
  expect_equal(NLPstudio:::.normalize_hyperparameter_value(c(0.1, 0.1)), 0.1)
  expect_equal(NLPstudio:::.normalize_hyperparameter_value(c(0.1, 0.2)), c(0.1, 0.2))
})

test_that("metadata joins protect DTW columns and honor overwrite", {
  dtw <- data.table::data.table(
    doc_id = c("doc1", "doc2"),
    Topic001 = c(0.8, 0.2),
    Topic002 = c(0.2, 0.8),
    group = c("old1", "old2")
  )
  dtw <- NLPstudio:::.add_topic_max_columns(dtw)
  meta <- data.table::data.table(
    doc_id = c("doc1", "doc2"),
    Topic001 = c(99, 99),
    group = c("new1", "new2"),
    source = c("s1", "s2")
  )

  expect_warning(
    expect_warning(
      protected <- NLPstudio:::.bind_topic_metadata(data.table::copy(dtw), meta),
      "Topic001"
    ),
    "group"
  )
  expect_equal(protected$group, c("old1", "old2"))
  expect_equal(protected$source, c("s1", "s2"))

  expect_warning(
    overwritten <- NLPstudio:::.bind_topic_metadata(data.table::copy(dtw), meta, overwrite = TRUE),
    "Dropping metadata columns"
  )
  expect_equal(overwritten$group, c("new1", "new2"))
  expect_equal(overwritten$source, c("s1", "s2"))
})

test_that("cached topic extraction helpers report missing or unsupported inputs", {
  text2vec_fit <- structure(
    list(engine = "text2vec", dtw = NULL),
    class = c("nlp_topic_fit", "list")
  )
  etm_fit <- structure(
    list(engine = "topicmodels.etm", dtw = NULL),
    class = c("nlp_topic_fit", "list")
  )
  tww_fit <- structure(
    list(
      tww = NULL,
      model_object = data.table::data.table(topic_id = "Topic001", term1 = 1)
    ),
    class = c("nlp_topic_fit", "list")
  )

  expect_error(NLPstudio:::.extract_dtw_table(text2vec_fit), "does not contain cached DTW")
  expect_error(NLPstudio:::.extract_dtw_table(etm_fit), "does not contain cached DTW")
  expect_error(
    NLPstudio:::.extract_dtw_table(structure(list(), class = "WarpLDA")),
    "Raw text2vec WarpLDA objects"
  )
  expect_error(NLPstudio:::.extract_dtw_table(list()), "unrecognized object")

  tww <- NLPstudio:::.extract_tww_table(tww_fit)
  expect_named(tww, c("topic_id", "term1"))
  expect_equal(tww$topic_id, "Topic001")
})

test_that("topic control validators reject malformed inputs", {
  expect_equal(
    NLPstudio:::.normalize_topic_control(NULL),
    list(model = list(), fit = list(), optimizer = list())
  )
  expect_error(NLPstudio:::.normalize_topic_control("bad"), "control must be a list")
  expect_error(NLPstudio:::.normalize_topic_control(list(list())), "control must be a named list")
  expect_error(NLPstudio:::.normalize_topic_control(list(extra = list())), "Unknown top-level")
  expect_error(NLPstudio:::.normalize_topic_control(list(model = "bad")), "must all be lists")
  expect_equal(
    NLPstudio:::.normalize_topic_control(list(model = list(), fit = NULL, optimizer = NULL)),
    list(model = list(), fit = list(), optimizer = list())
  )

  control <- list(model = list(), fit = list(), optimizer = list())
  expect_error(
    NLPstudio:::.validate_topic_fit_args(
      "text2vec", "lda", NULL, NULL, control, NULL, NULL, NULL
    ),
    "k must be supplied"
  )
  expect_error(
    NLPstudio:::.validate_topic_fit_args(
      "seededlda", "seededlda", NULL, 2, control, quanteda::dictionary(list(a = "x")), NULL, NULL
    ),
    "k is not used"
  )
  expect_error(
    NLPstudio:::.validate_topic_fit_args(
      "text2vec", "lda", NULL, 2, control, quanteda::dictionary(list(a = "x")), NULL, NULL
    ),
    "dictionary is only valid"
  )
  expect_error(
    NLPstudio:::.validate_topic_fit_args(
      "text2vec", "lda", NULL, 2, control, NULL, NULL, list()
    ),
    "initial_model is not supported"
  )
})

test_that("backend-control sanitizer returns compact stored values", {
  env <- new.env(parent = emptyenv())
  values <- list(
    fun = function(x) x,
    env = env,
    mat = matrix(1:4, nrow = 2),
    nested = list(mat = matrix(1:2, nrow = 1))
  )

  out <- NLPstudio:::.sanitize_backend_control_list(values)

  expect_equal(out$fun, "<function>")
  expect_match(out$env, "^<environment>$")
  expect_equal(out$mat, list(class = "matrix", dim = c(2L, 2L)))
  expect_equal(out$nested$mat, list(class = "matrix", dim = c(1L, 2L)))
})

test_that("ETM input preparation coerces pretrained embeddings to double", {
  x <- Matrix::Matrix(
    matrix(
      c(1, 0, 2,
        0, 1, 1,
        1, 3, 0),
      nrow = 3,
      byrow = TRUE
    ),
    sparse = TRUE
  )
  x <- methods::as(x, "dgCMatrix")
  rownames(x) <- paste0("doc", seq_len(nrow(x)))
  colnames(x) <- paste0("term", seq_len(ncol(x)))
  embeddings <- matrix(
    seq_len(ncol(x) * 3),
    nrow = ncol(x),
    ncol = 3,
    dimnames = list(colnames(x), NULL)
  )

  prep <- NLPstudio:::.prepare_etm_input(x, list(embeddings = embeddings))

  expect_s4_class(prep$x, "dgCMatrix")
  expect_type(prep$model_control$embeddings, "double")
  expect_equal(prep$model_control$vocab, colnames(x))
})

test_that("ETM input preparation preserves survivors after pruning docs and terms", {
  x <- Matrix::Matrix(
    matrix(
      c(1, 0, 0, 0,
        0, 2, 0, 0,
        0, 0, 0, 3,
        0, 1, 0, 1),
      nrow = 4,
      byrow = TRUE
    ),
    sparse = TRUE
  )
  x <- methods::as(x, "dgCMatrix")
  rownames(x) <- paste0("doc", seq_len(nrow(x)))
  colnames(x) <- paste0("term", seq_len(ncol(x)))
  embeddings <- matrix(
    seq_len(2 * 3),
    nrow = 2,
    ncol = 3,
    dimnames = list(c("term2", "term4"), NULL)
  )

  warnings <- character()
  prep <- withCallingHandlers(
    NLPstudio:::.prepare_etm_input(x, list(embeddings = embeddings)),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_true(any(grepl("Dropping 2 terms", warnings, fixed = TRUE)))
  expect_true(any(grepl("Dropping 1 documents", warnings, fixed = TRUE)))
  expect_equal(prep$doc_ids, c("doc2", "doc3", "doc4"))
  expect_equal(prep$term_names, c("term2", "term4"))
  expect_equal(rownames(prep$model_control$embeddings), c("term2", "term4"))
})

test_that("ETM input preparation drops all-zero rows for learned embeddings", {
  x <- Matrix::Matrix(
    matrix(
      c(0, 0, 0,
        1, 0, 1,
        0, 2, 0),
      nrow = 3,
      byrow = TRUE
    ),
    sparse = TRUE
  )
  x <- methods::as(x, "dgCMatrix")
  rownames(x) <- paste0("doc", seq_len(nrow(x)))
  colnames(x) <- paste0("term", seq_len(ncol(x)))

  expect_warning(
    prep <- NLPstudio:::.prepare_etm_input(x, list(embeddings = 2)),
    "Dropping 1 documents"
  )

  expect_equal(prep$doc_ids, c("doc2", "doc3"))
  expect_equal(prep$term_names, colnames(x))
})

test_that("ETM fitting rejects fewer than three surviving documents", {
  x <- Matrix::Matrix(
    matrix(
      c(1, 0,
        0, 0,
        0, 2),
      nrow = 3,
      byrow = TRUE
    ),
    sparse = TRUE
  )
  x <- methods::as(x, "dgCMatrix")

  expect_error(
    NLPstudio:::.fit_etm_model_original(NULL, list(data = x)),
    "requires at least 3 non-empty documents"
  )
})

test_that("ETM train/test splitter keeps all partitions non-empty", {
  set.seed(1)
  idx <- NLPstudio:::.etm_train_test_indices(6)

  expect_setequal(unlist(idx, use.names = FALSE), 1:6)
  expect_true(length(idx$train) >= 1L)
  expect_true(length(idx$test1) >= 1L)
  expect_true(length(idx$test2) >= 1L)
  expect_error(NLPstudio:::.etm_train_test_indices(2), "greater than or equal to 3")
})

test_that("ETM beta output is normalized to topic-by-term orientation", {
  beta <- structure(
    matrix(
      c(0.7, 0.3,
        0.2, 0.8,
        0.4, 0.6),
      nrow = 3,
      byrow = TRUE,
      dimnames = list(c("term1", "term2", "term3"), NULL)
    ),
    class = "fake_etm_beta"
  )
  as.matrix.fake_etm_beta <- function(x, ...) unclass(x)

  out <- NLPstudio:::.etm_beta_tww(beta)

  expect_equal(dim(out$tww), c(2L, 3L))
  expect_equal(out$term_names, c("term1", "term2", "term3"))
  expect_equal(unname(out$tww[1, ]), c(0.7, 0.2, 0.4))
})

test_that("legacy DTW and TWW coercion standardizes supported shapes", {
  dtw <- data.table::data.table(
    rn = c("doc1", "doc2"),
    topic1 = c(0.8, 0.2),
    topic2 = c(0.2, 0.8)
  )
  coerced_dtw <- NLPstudio:::.coerce_existing_dtw_table(dtw)

  expect_named(
    coerced_dtw,
    c("doc_id", "Topic001", "Topic002", "topic_max_id", "topic_max_int", "topic_max_value")
  )
  expect_equal(coerced_dtw$doc_id, c("doc1", "doc2"))
  expect_equal(coerced_dtw$topic_max_id, c("Topic001", "Topic002"))
  expect_equal(coerced_dtw$topic_max_int, c(1L, 2L))

  tww <- data.table::data.table(
    topic_id = 1:2,
    term_a = c(0.7, 0.3),
    term_b = c(0.3, 0.7)
  )
  coerced_tww <- NLPstudio:::.coerce_existing_tww_table(tww)

  expect_named(coerced_tww, c("topic_id", "term_a", "term_b"))
  expect_equal(coerced_tww$topic_id, c("Topic001", "Topic002"))
})

test_that("set_theta_names standardizes legacy topic columns", {
  theta <- data.table::data.table(
    rn = c("doc1", "doc2"),
    topic1 = c(0.7, 0.3),
    topic2 = c(0.3, 0.7)
  )

  out <- NLPstudio:::set_theta_names(theta)

  expect_named(out, c("doc_id", "Topic1", "Topic2"))
  expect_equal(out$doc_id, c("doc1", "doc2"))
})

# ----------------------------------------------------------------------------
# Vocabulary alignment (.align_topic_input_to_vocab)
# ----------------------------------------------------------------------------

make_aligned_dfm <- function(doc_terms, doc_ids = NULL) {
  if (is.null(doc_ids)) doc_ids <- paste0("doc", seq_along(doc_terms))
  corp <- quanteda::corpus(stats::setNames(doc_terms, doc_ids))
  quanteda::dfm(quanteda::tokens(corp))
}

test_that(".align_topic_input_to_vocab pads when input is a strict subset of vocab", {
  x <- make_aligned_dfm(c("alpha beta", "alpha"))
  vocab <- c("alpha", "beta", "gamma")

  out <- expect_silent(
    NLPstudio:::.align_topic_input_to_vocab(x, vocab)
  )

  expect_equal(out$term_names, vocab)
  expect_equal(colnames(out$sparse), vocab)
  expect_equal(unname(out$sparse[, "gamma"]), c(0, 0))
})

test_that(".align_topic_input_to_vocab warns and drops terms when input is a superset", {
  x <- make_aligned_dfm(c("alpha beta extra", "alpha gamma"))
  vocab <- c("alpha", "beta")

  expect_warning(
    out <- NLPstudio:::.align_topic_input_to_vocab(x, vocab),
    "Dropping 2 terms that were not found"
  )

  expect_equal(colnames(out$sparse), vocab)
  expect_setequal(out$doc_ids, c("doc1", "doc2"))
})

test_that(".align_topic_input_to_vocab errors when vocab and input are disjoint", {
  x <- make_aligned_dfm(c("alpha beta", "alpha"))
  vocab <- c("delta", "epsilon")

  expect_error(
    suppressWarnings(NLPstudio:::.align_topic_input_to_vocab(x, vocab)),
    "No documents remain"
  )
})

test_that(".align_topic_input_to_vocab reorders columns to match vocab", {
  x <- make_aligned_dfm("beta alpha gamma")
  vocab <- c("gamma", "alpha", "beta")

  out <- NLPstudio:::.align_topic_input_to_vocab(x, vocab)

  expect_equal(colnames(out$sparse), vocab)
  expect_equal(unname(out$sparse[1, ]), c(1, 1, 1))
})

test_that(".align_topic_input_to_vocab rejects an empty vocab", {
  x <- make_aligned_dfm("alpha beta")

  expect_error(
    NLPstudio:::.align_topic_input_to_vocab(x, character()),
    "vocab must contain at least one term"
  )
})

test_that(".align_topic_input_to_vocab drops empty docs after alignment with a warning", {
  x <- make_aligned_dfm(c("alpha beta", "gamma"))
  vocab <- c("alpha", "beta")

  warnings <- character()
  out <- withCallingHandlers(
    NLPstudio:::.align_topic_input_to_vocab(x, vocab),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_true(any(grepl("Dropping 1 terms", warnings)))
  expect_true(any(grepl("Dropping 1 documents", warnings)))
  expect_equal(out$doc_ids, "doc1")
})
