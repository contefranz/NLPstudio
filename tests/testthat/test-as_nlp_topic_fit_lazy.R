# Lazy DTW/TWW caching on the adopt methods (return_dtw / return_tww).

make_lazy_dfm <- function() {
  quanteda::dfm(quanteda::tokens(
    quanteda::corpus(c(
      d1 = "growth revenue revenue profit margin",
      d2 = "risk debt covenant liquidity risk",
      d3 = "revenue subscription customer retention growth",
      d4 = "audit control oversight committee risk",
      d5 = "cloud cost margin profit revenue",
      d6 = "debt interest expense capital allocation"
    )),
    remove_punct = TRUE
  ))
}

test_that("return flags are validated", {
  skip_if_not_installed("seededlda")
  m <- seededlda::textmodel_lda(make_lazy_dfm(), k = 3, max_iter = 100, verbose = FALSE)
  expect_error(as_nlp_topic_fit(m, return_tww = "no"), "return_tww must be a single")
  expect_error(as_nlp_topic_fit(m, return_dtw = NA), "return_dtw must be a single")
})

test_that("seededlda lazy TWW: cache skipped but reconstructs identically", {
  skip_if_not_installed("seededlda")
  dfmat <- make_lazy_dfm()
  m <- seededlda::textmodel_lda(dfmat, k = 3, max_iter = 100, verbose = FALSE)
  phi_dimnames <- dimnames(m$phi)

  eager <- as_nlp_topic_fit(m)
  lazy  <- as_nlp_topic_fit(m, return_tww = FALSE)

  expect_false(is.null(eager$tww))
  expect_null(lazy$tww)
  expect_identical(lazy$vocab, colnames(m$phi))            # cheap vocab, no densification
  expect_equal(get_tww(lazy), get_tww(eager))              # lazy reconstruction matches
  expect_equal(get_dtw(lazy), get_dtw(eager))
  expect_identical(dimnames(m$phi), phi_dimnames)          # source not mutated
})

test_that("seededlda lazy DTW reconstructs identically", {
  skip_if_not_installed("seededlda")
  m <- seededlda::textmodel_lda(make_lazy_dfm(), k = 3, max_iter = 100, verbose = FALSE)
  eager <- as_nlp_topic_fit(m)
  lazy  <- as_nlp_topic_fit(m, return_dtw = FALSE)
  expect_null(lazy$dtw)
  expect_equal(get_dtw(lazy), get_dtw(eager))
})

test_that("get_representative_candidates works on a TWW-lazy seededlda fit", {
  skip_if_not_installed("seededlda")
  m <- seededlda::textmodel_lda(make_lazy_dfm(), k = 3, max_iter = 100, verbose = FALSE)
  lazy <- as_nlp_topic_fit(m, return_tww = FALSE)
  reps <- get_representative_candidates(lazy, topics = 1)
  expect_s3_class(reps, "data.table")
  expect_true(nrow(reps) >= 1L)
})

test_that("topicmodels lazy caching reconstructs identically without mutating source", {
  skip_if_not_installed("topicmodels")
  dtm <- quanteda::convert(make_lazy_dfm(), to = "topicmodels")
  lda <- topicmodels::LDA(dtm, k = 3, control = list(seed = 1L))
  beta_dimnames <- dimnames(lda@beta)

  eager <- as_nlp_topic_fit(lda)
  lazy  <- as_nlp_topic_fit(lda, return_tww = FALSE)

  expect_null(lazy$tww)
  expect_identical(lazy$vocab, as.character(lda@terms))
  expect_equal(get_tww(lazy), get_tww(eager))
  expect_equal(get_dtw(lazy), get_dtw(eager))
  expect_identical(dimnames(lda@beta), beta_dimnames)
})

test_that("STM lazy caching reconstructs identically", {
  skip_if_not_installed("stm")
  dfmat <- make_lazy_dfm()
  conv <- quanteda::convert(dfmat, to = "stm")
  fit <- stm::stm(conv$documents, conv$vocab, K = 3, max.em.its = 5,
                  init.type = "Spectral", verbose = FALSE)

  eager <- as_nlp_topic_fit(fit)
  lazy  <- as_nlp_topic_fit(fit, return_tww = FALSE)

  expect_null(lazy$tww)
  expect_identical(lazy$vocab, as.character(fit$vocab))
  expect_equal(get_tww(lazy), get_tww(eager))
  expect_equal(get_dtw(lazy), get_dtw(eager))
})
