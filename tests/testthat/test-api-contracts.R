make_api_contract_fit <- function() {
  dtw <- matrix(
    c(
      0.80, 0.20,
      0.45, 0.55,
      0.10, 0.90
    ),
    nrow = 3L,
    byrow = TRUE,
    dimnames = list(
      paste0("doc", 1:3),
      c("Topic001", "Topic002")
    )
  )
  tww <- matrix(
    c(
      0.50, 0.30, 0.15, 0.05,
      0.05, 0.20, 0.35, 0.40
    ),
    nrow = 2L,
    byrow = TRUE,
    dimnames = list(
      c("Topic001", "Topic002"),
      c("growth", "profit", "risk", "debt")
    )
  )

  NLPstudio:::.new_nlp_topic_fit(
    engine = "synthetic",
    model = "lda",
    method = NULL,
    model_object = list(),
    dtw = dtw,
    tww = tww,
    doc_ids = rownames(dtw),
    vocab = colnames(tww),
    docvars = data.table::data.table(
      doc_id = rownames(dtw),
      group = c("a", "a", "b")
    ),
    doc_data = data.table::data.table(
      doc_id = rownames(dtw),
      text = c("growth profit", "profit risk", "risk debt")
    ),
    hyperparameters = data.table::data.table(parameter = "k", value = 2),
    backend_control = list(model = list(), fit = list(seed = 1L), optimizer = list()),
    call = quote(fit_topic_model())
  )
}

make_api_contract_selection <- function() {
  out <- data.table::data.table(
    k = c(2L, 2L, 2L, 2L, 3L, 3L),
    metric = c(
      "diversity", "exclusivity", "exclusivity",
      "held_out_perplexity", "diversity", "held_out_perplexity"
    ),
    level = c("aggregate", "aggregate", "topic", "aggregate", "aggregate", "aggregate"),
    topic_id = c(NA_character_, NA_character_, "Topic001", NA_character_, NA_character_, NA_character_),
    value = c(0.75, 0.45, 0.50, NA, 0.80, NA),
    supported = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)
  )
  data.table::setattr(out, "class", c("nlp_k_selection", "data.table", "data.frame"))
  out[]
}

test_that("nlp_topic_fit keeps the frozen public fields", {
  fit <- make_api_contract_fit()

  expect_s3_class(fit, "nlp_topic_fit")
  expect_named(
    fit,
    c(
      "engine", "model", "method", "model_object", "dtw", "tww",
      "doc_ids", "vocab", "docvars", "doc_data", "hyperparameters",
      "backend_control", "call"
    ),
    ignore.order = FALSE
  )
  expect_equal(colnames(fit$dtw), c("Topic001", "Topic002"))
  expect_equal(rownames(fit$tww), c("Topic001", "Topic002"))
  expect_equal(fit$doc_ids, rownames(fit$dtw))
  expect_equal(fit$vocab, colnames(fit$tww))
})

test_that("evaluation retains standard columns for aggregate and topic rows", {
  fit <- make_api_contract_fit()
  standard_cols <- c("metric", "level", "topic_id", "value", "supported")

  aggregate <- evaluate_topic_model(
    fit,
    metrics = c("diversity", "exclusivity"),
    level = "aggregate",
    top_n = 2L
  )
  expect_s3_class(aggregate, "data.table")
  expect_named(aggregate, standard_cols, ignore.order = FALSE)
  expect_true(all(aggregate$level == "aggregate"))
  expect_true(all(is.na(aggregate$topic_id)))

  all_levels <- evaluate_topic_model(
    fit,
    metrics = c("diversity", "exclusivity"),
    level = "all",
    top_n = 2L
  )
  expect_named(all_levels, standard_cols, ignore.order = FALSE)
  expect_true(all(standard_cols %in% names(all_levels)))
  expect_true(any(all_levels$level == "topic"))
  expect_true(any(all_levels$level == "aggregate" & is.na(all_levels$topic_id)))
})

test_that("selection and selection summaries keep stable schemas", {
  selection <- make_api_contract_selection()
  selection_cols <- c("k", "metric", "level", "topic_id", "value", "supported")

  expect_s3_class(selection, "nlp_k_selection")
  expect_named(selection, selection_cols, ignore.order = FALSE)

  summary <- summarize_k_selection(selection, include_unsupported = TRUE)
  expect_s3_class(summary, "nlp_k_selection_summary")
  expect_named(
    summary,
    c("k", "diversity", "exclusivity", "held_out_perplexity"),
    ignore.order = FALSE
  )

  topic_metrics <- attr(summary, "topic_metrics", exact = TRUE)
  expect_s3_class(topic_metrics, "data.table")
  expect_named(topic_metrics, selection_cols, ignore.order = FALSE)

  unsupported <- attr(summary, "unsupported", exact = TRUE)
  expect_s3_class(unsupported, "data.table")
  expect_named(unsupported, selection_cols, ignore.order = FALSE)
})

test_that("topic stability keeps the frozen matching schema", {
  ref <- make_api_contract_fit()
  candidate <- make_api_contract_fit()
  candidate$tww <- candidate$tww[c("Topic002", "Topic001"), ]
  rownames(candidate$tww) <- c("Topic001", "Topic002")

  out <- assess_topic_stability(list(ref, candidate), seeds = c(1L, 2L))

  expect_s3_class(out, "nlp_topic_stability")
  expect_named(
    out,
    c(
      "run_id", "seed", "reference_run_id", "reference_seed",
      "topic_id", "matched_topic_id", "similarity",
      "topic_stability", "run_stability", "aggregate_stability",
      "k", "engine", "model", "method"
    ),
    ignore.order = FALSE
  )
  expect_equal(out$topic_id, c("Topic001", "Topic002"))
})

test_that("summarize_topics keeps the frozen topic-summary schema", {
  fit <- make_api_contract_fit()

  out <- summarize_topics(fit, top_n = 2L, representative_n = 0L)

  expect_s3_class(out, "data.table")
  expect_named(
    out,
    c(
      "topic_id", "topic_int", "top_terms", "top_term_probabilities",
      "prevalence", "coherence_npmi", "coherence_umass", "diversity",
      "exclusivity", "representative_doc_ids", "representative_documents"
    ),
    ignore.order = FALSE
  )
  expect_equal(out$topic_id, c("Topic001", "Topic002"))
  expect_true(is.list(out$representative_documents))
})
