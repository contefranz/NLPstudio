test_that("warp_lda rejects non-dgCMatrix input", {
  expect_error(warp_lda(matrix(1:4, 2, 2), k = 2), "must be a sparse matrix of class dgCMatrix")
})

test_that("warp_lda rejects non-numeric k", {
  m <- Matrix::Matrix(matrix(c(1, 0, 0, 1, 1, 0), nrow = 2), sparse = TRUE)
  m <- methods::as(m, "dgCMatrix")
  expect_error(warp_lda(m, k = "two"), "k must be a numeric")
})

test_that("warpLDA deprecated alias emits warning", {
  m <- Matrix::Matrix(matrix(c(1, 0, 0, 1, 1, 0), nrow = 2), sparse = TRUE)
  m <- methods::as(m, "dgCMatrix")
  expect_warning(
    tryCatch(warpLDA(m, k = "two"), error = function(e) NULL),
    "deprecated|warp_lda"
  )
})
