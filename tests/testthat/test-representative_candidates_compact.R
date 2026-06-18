# Memory-light compact get_representative_candidates() + top_n.

rc_dfm <- function() {
  quanteda::dfm(quanteda::tokens(
    quanteda::corpus(c(
      d1 = "growth revenue revenue profit margin",
      d2 = "risk debt covenant liquidity risk",
      d3 = "revenue subscription customer retention growth",
      d4 = "audit control oversight committee risk",
      d5 = "cloud cost margin profit revenue",
      d6 = "debt interest expense capital allocation",
      d7 = "growth growth revenue customer",
      d8 = "risk risk debt audit"
    )),
    remove_punct = TRUE
  ))
}

test_that("fit output is compact (no per-topic columns) and one row per document", {
  skip_if_not_installed("seededlda")
  m <- seededlda::textmodel_lda(rc_dfm(), k = 3, max_iter = 100, verbose = FALSE)
  fit <- as_nlp_topic_fit(m)

  reps <- get_representative_candidates(fit)
  expect_true(all(c("doc_id", "topic_max_id", "topic_max_int", "topic_max_value",
                    "topic_rank", "candidate_band") %in% names(reps)))
  expect_false(any(grepl("^Topic[0-9]+$", names(reps))))
  expect_equal(nrow(reps), quanteda::ndoc(rc_dfm()))
})

test_that("dominant topic and weight match a direct max.col on the DTW matrix", {
  skip_if_not_installed("seededlda")
  m <- seededlda::textmodel_lda(rc_dfm(), k = 3, max_iter = 100, verbose = FALSE)
  fit <- as_nlp_topic_fit(m)

  mat <- fit$dtw
  idx <- max.col(mat, ties.method = "first")
  ref <- data.table::data.table(
    doc_id = rownames(mat),
    topic_max_id = colnames(mat)[idx],
    topic_max_value = mat[cbind(seq_len(nrow(mat)), idx)]
  )[order(doc_id)]

  reps <- get_representative_candidates(fit)[order(doc_id)]
  expect_identical(reps$topic_max_id, ref$topic_max_id)
  expect_equal(reps$topic_max_value, ref$topic_max_value)
})

test_that("top_n keeps only the strongest documents per topic", {
  skip_if_not_installed("seededlda")
  m <- seededlda::textmodel_lda(rc_dfm(), k = 3, max_iter = 100, verbose = FALSE)
  fit <- as_nlp_topic_fit(m)

  all_reps <- get_representative_candidates(fit)
  top2 <- get_representative_candidates(fit, top_n = 2)

  expect_true(all(top2$topic_rank <= 2L))
  expect_lte(max(top2[, .N, by = topic_max_id]$N), 2L)
  expect_lt(nrow(top2), nrow(all_reps))
  expect_error(get_representative_candidates(fit, top_n = 0), "positive integer")
  expect_error(get_representative_candidates(fit, top_n = 1.5), "positive integer")
})

test_that("reconstruct path (return_dtw = FALSE) matches the cached path", {
  skip_if_not_installed("seededlda")
  m <- seededlda::textmodel_lda(rc_dfm(), k = 3, max_iter = 100, verbose = FALSE)
  cached <- as_nlp_topic_fit(m)
  lazy   <- as_nlp_topic_fit(m, return_dtw = FALSE, return_tww = FALSE)
  expect_equal(
    get_representative_candidates(lazy)[order(doc_id)],
    get_representative_candidates(cached)[order(doc_id)]
  )
})
