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
