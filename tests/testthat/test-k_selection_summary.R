make_k_selection_summary_input <- function() {
  out <- data.table::data.table(
    k = c(2L, 3L, 2L, 3L, 2L, 3L, 2L, 3L, 2L),
    metric = c(
      "diversity", "diversity",
      "exclusivity", "exclusivity",
      "held_out_perplexity", "held_out_perplexity",
      "stability", "stability",
      "coherence_npmi"
    ),
    level = rep("aggregate", 9L),
    topic_id = NA_character_,
    value = c(0.75, 0.82, 0.41, 0.44, NA, NA, 0.90, 0.87, NA),
    supported = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE)
  )
  topic <- data.table::data.table(
    k = c(2L, 2L),
    metric = "diversity",
    level = "topic",
    topic_id = c("Topic001", "Topic002"),
    value = c(0.7, 0.8),
    supported = TRUE
  )
  out <- rbind(out, topic)
  data.table::setattr(out, "class", c("nlp_k_selection", "data.table", "data.frame"))
  out
}

test_that("summarize_k_selection returns one wide row per K", {
  selection <- make_k_selection_summary_input()

  out <- summarize_k_selection(selection)

  expect_s3_class(out, "nlp_k_selection_summary")
  expect_s3_class(out, "data.table")
  expect_named(
    out,
    c("k", "diversity", "exclusivity", "stability"),
    ignore.order = FALSE
  )
  expect_equal(out$k, 2:3)
  expect_equal(out$diversity, c(0.75, 0.82))
  expect_equal(out$stability, c(0.90, 0.87))
  expect_null(attr(out, "unsupported", exact = TRUE))

  topic_metrics <- attr(out, "topic_metrics", exact = TRUE)
  expect_s3_class(topic_metrics, "data.table")
  expect_equal(topic_metrics$topic_id, c("Topic001", "Topic002"))
})

test_that("summarize_k_selection can retain unsupported metrics", {
  selection <- make_k_selection_summary_input()

  out <- summarize_k_selection(selection, include_unsupported = TRUE)

  expect_true("held_out_perplexity" %in% names(out))
  expect_true("coherence_npmi" %in% names(out))
  expect_true(all(is.na(out$held_out_perplexity)))
  expect_true(all(is.na(out$coherence_npmi)))

  unsupported <- attr(out, "unsupported", exact = TRUE)
  expect_s3_class(unsupported, "data.table")
  expect_setequal(unsupported$metric, c("held_out_perplexity", "coherence_npmi"))
})

test_that("summarize_k_selection validates selection inputs", {
  selection <- make_k_selection_summary_input()
  bad <- data.table::copy(selection)
  bad[, value := NULL]

  expect_error(
    summarize_k_selection(data.table::data.table(k = 2L)),
    "nlp_k_selection"
  )
  expect_error(
    summarize_k_selection(bad),
    "missing required columns: value"
  )
  expect_error(
    summarize_k_selection(selection, include_unsupported = NA),
    "include_unsupported"
  )

  duplicated <- rbind(selection, selection[metric == "diversity" & k == 2L][1L])
  data.table::setattr(duplicated, "class", c("nlp_k_selection", "data.table", "data.frame"))
  expect_error(
    summarize_k_selection(duplicated),
    "duplicate aggregate metric rows"
  )
})

test_that("summarize_k_selection parses common OpTop outputs", {
  selection <- make_k_selection_summary_input()
  optop <- data.table::data.table(
    topic = c(2, 3),
    OpTop = c(0.12, 0.08),
    pval = c(0.04, 0.12)
  )

  out <- summarize_k_selection(selection, optop = optop)

  expect_named(
    out,
    c("k", "diversity", "exclusivity", "stability", "optop", "optop_pval"),
    ignore.order = FALSE
  )
  expect_equal(out$optop, c(0.12, 0.08))
  expect_equal(out$optop_pval, c(0.04, 0.12))
})

test_that("summarize_k_selection parses cleaned OpTop aliases", {
  selection <- make_k_selection_summary_input()
  optop <- data.frame(
    k = c(2L, 3L),
    optop = c(0.3, 0.2),
    p_value = c(0.7, 0.8)
  )

  out <- summarize_k_selection(selection, optop = optop)

  expect_equal(out$optop, c(0.3, 0.2))
  expect_equal(out$optop_pval, c(0.7, 0.8))
})

test_that("summarize_k_selection parses table-like OpTop lists", {
  selection <- make_k_selection_summary_input()
  optop <- list(topic = c(2L, 3L), OpTop = c(0.5, 0.6))

  out <- summarize_k_selection(selection, optop = optop)

  expect_equal(out$optop, c(0.5, 0.6))
  expect_false("optop_pval" %in% names(out))
})

test_that("summarize_k_selection handles partial and invalid OpTop output", {
  selection <- make_k_selection_summary_input()

  out <- summarize_k_selection(
    selection,
    optop = data.table::data.table(topic = 2L, OpTop = 0.1, pval = 0.05)
  )
  expect_equal(out$optop, c(0.1, NA))
  expect_equal(out$optop_pval, c(0.05, NA))

  expect_warning(
    extra <- summarize_k_selection(
      selection,
      optop = data.table::data.table(topic = c(2L, 4L), OpTop = c(0.1, 0.9))
    ),
    "extra rows were ignored"
  )
  expect_equal(extra$optop, c(0.1, NA))

  expect_error(
    summarize_k_selection(
      selection,
      optop = data.table::data.table(topic = c(2L, 2L), OpTop = c(0.1, 0.2))
    ),
    "at most one row per K"
  )
  expect_error(
    summarize_k_selection(selection, optop = data.table::data.table(OpTop = 0.1)),
    "K column"
  )
  expect_error(
    summarize_k_selection(selection, optop = data.table::data.table(topic = 2L, score = 0.1)),
    "recognizable statistic"
  )
  expect_error(
    summarize_k_selection(selection, optop = data.table::data.table(topic = 2.5, OpTop = 0.1)),
    "positive integers"
  )
  expect_error(
    summarize_k_selection(
      selection,
      optop = data.table::data.table(topic = integer(), OpTop = numeric())
    ),
    "at least one row"
  )
  expect_error(
    summarize_k_selection(selection, optop = function() NULL),
    "table-like list"
  )
})

test_that("summarize_k_selection rejects OpTop input adapters", {
  optop_input <- structure(list(), class = c("nlp_optop_input", "list"))

  expect_error(
    summarize_k_selection(make_k_selection_summary_input(), optop = optop_input),
    "OpTop::optimal_topic"
  )
})

test_that("print.nlp_k_selection_summary is compact", {
  out <- summarize_k_selection(
    make_k_selection_summary_input(),
    optop = data.table::data.table(topic = 2:3, OpTop = c(0.1, 0.2))
  )

  printed <- capture.output(print(out))

  expect_true(any(grepl("<nlp_k_selection_summary>", printed, fixed = TRUE)))
  expect_true(any(grepl("candidate K values: 2, 3", printed, fixed = TRUE)))
  expect_true(any(grepl("OpTop: included", printed, fixed = TRUE)))
  expect_true(any(grepl("topic-level metrics: attached", printed, fixed = TRUE)))

  with_unsupported <- summarize_k_selection(
    make_k_selection_summary_input(),
    include_unsupported = TRUE
  )
  printed_unsupported <- capture.output(print(with_unsupported))
  expect_true(any(grepl("unsupported aggregate metrics: attached", printed_unsupported, fixed = TRUE)))
})
