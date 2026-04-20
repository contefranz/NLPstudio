make_dtm <- function() {
  m <- Matrix::Matrix(
    matrix(c(1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0), nrow = 3),
    sparse = TRUE
  )
  m <- methods::as(m, "dgCMatrix")
  colnames(m) <- paste0("term", seq_len(ncol(m)))
  rownames(m) <- paste0("doc",  seq_len(nrow(m)))
  m
}

# --- input validation ---

test_that("warp_lda rejects non-dgCMatrix input", {
  expect_error(warp_lda(matrix(1:4, 2, 2), k = 2), "must be a sparse matrix of class dgCMatrix")
})

test_that("warp_lda rejects non-numeric k", {
  expect_error(warp_lda(make_dtm(), k = "two"), "k must be a single positive numeric")
})

test_that("warpLDA deprecated alias emits warning", {
  expect_warning(
    tryCatch(warpLDA(make_dtm(), k = "two"), error = function(e) NULL),
    "deprecated|warp_lda"
  )
})

# --- argument routing contract ---
# text2vec's R5 class silently ignores unknown args rather than erroring,
# so routing tests verify positive contracts: valid args reach the right
# method and the function completes successfully.

test_that("fit_control = list(progressbar = FALSE) is a valid fit arg and succeeds", {
  result <- warp_lda(make_dtm(), k = 2,
                     fit_control = list(n_iter = 10, progressbar = FALSE))
  expect_type(result, "list")
  expect_named(result, c("lda_object", "theta", "phi"), ignore.order = TRUE)
})

test_that("lda_control = list(doc_topic_prior = 0.5) is a valid constructor arg and succeeds", {
  result <- warp_lda(make_dtm(), k = 2,
                     lda_control = list(doc_topic_prior = 0.5),
                     fit_control = list(n_iter = 10, progressbar = FALSE))
  expect_type(result, "list")
  expect_named(result, c("lda_object", "theta", "phi"), ignore.order = TRUE)
})

test_that("k cannot be overridden via lda_control", {
  # Even if lda_control supplies n_topics, the function always uses k
  result <- warp_lda(make_dtm(), k = 2,
                     lda_control = list(n_topics = 99),
                     fit_control = list(n_iter = 10, progressbar = FALSE))
  expect_equal(ncol(result$theta) - 1L, 2L)  # theta has doc_id + k topic columns
})

test_that("return_theta = FALSE omits theta from result", {
  result <- warp_lda(make_dtm(), k = 2, return_theta = FALSE,
                     fit_control = list(n_iter = 10, progressbar = FALSE))
  expect_null(result$theta)
  expect_false(is.null(result$phi))
})

test_that("return_phi = FALSE omits phi from result", {
  result <- warp_lda(make_dtm(), k = 2, return_phi = FALSE,
                     fit_control = list(n_iter = 10, progressbar = FALSE))
  expect_null(result$phi)
  expect_false(is.null(result$theta))
})
