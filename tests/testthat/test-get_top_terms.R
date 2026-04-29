make_tww_table <- function() {
  data.table::data.table(
    topic_id = c("Topic001", "Topic002", "Topic003"),
    apple = c(0.6, 0.1, 0.2),
    banana = c(0.3, 0.7, 0.2),
    carrot = c(0.1, 0.2, 0.6)
  )
}

test_that("get_top_terms validates n", {
  tww <- make_tww_table()
  expect_error(get_top_terms(tww, n = 0), "n must be a single positive integer")
  expect_error(get_top_terms(tww, n = 1.5), "n must be a single positive integer")
  expect_error(get_top_terms(tww, n = c(1, 2)), "n must be a single positive integer")
})

test_that("get_top_terms returns ranked long output", {
  out <- get_top_terms(make_tww_table(), n = 2, format = "long")

  expect_true(data.table::is.data.table(out))
  expect_named(out, c("rank", "topic", "term", "probability"))
  expect_equal(nrow(out), 6L)
  expect_equal(out[topic == "Topic001", term], c("apple", "banana"))
  expect_equal(out[topic == "Topic002", term], c("banana", "carrot"))
  expect_equal(out[topic == "Topic003", term], c("carrot", "apple"))
})

test_that("get_top_terms supports topic filters and large n", {
  out_numeric <- get_top_terms(make_tww_table(), n = 5, topics = 2)
  out_character <- get_top_terms(make_tww_table(), n = 5, topics = "Topic003")

  expect_equal(unique(out_numeric$topic), "Topic002")
  expect_equal(nrow(out_numeric), 3L)
  expect_equal(unique(out_character$topic), "Topic003")
  expect_equal(nrow(out_character), 3L)
})

test_that("get_top_terms rejects unavailable topic filters", {
  tww <- make_tww_table()
  expect_error(get_top_terms(tww, topics = 4), "Some requested topics are not available")
  expect_error(get_top_terms(tww, topics = "Topic099"), "Some requested topics are not available")
  expect_error(
    get_top_terms(tww, topics = list("Topic001")),
    "topics must be NULL, numeric topic indices, or Topic### identifiers"
  )
  expect_error(
    get_top_terms(tww, topics = NA),
    "topics must be NULL, numeric topic indices, or Topic### identifiers"
  )
  expect_error(
    get_top_terms(tww, topics = NA_integer_),
    "positive integer indices or Topic### identifiers"
  )
})

test_that("get_top_terms returns an empty table for empty topic selectors", {
  tww <- make_tww_table()
  out <- get_top_terms(tww, topics = integer(0))
  expect_true(data.table::is.data.table(out))
  expect_equal(nrow(out), 0L)
})

test_that("get_top_terms returns wide output by rank", {
  out <- get_top_terms(make_tww_table(), n = 2, format = "wide")

  expect_named(
    out,
    c(
      "rank",
      "Topic001_term", "Topic001_prob",
      "Topic002_term", "Topic002_prob",
      "Topic003_term", "Topic003_prob"
    )
  )
  expect_equal(out$rank, 1:2)
  expect_equal(out$Topic001_term, c("apple", "banana"))
  expect_equal(out$Topic002_prob, c(0.7, 0.2))
})
