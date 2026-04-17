local({
  corp <- quanteda::corpus(c(
    doc1 = "cat dog mouse",
    doc2 = "cat bird fish",
    doc3 = "dog cat mouse bird"
  ))
  dfmat <<- quanteda::dfm(quanteda::tokens(corp))
})

test_that("calculate_similarity returns correct S4 class without y", {
  result <- calculate_similarity(dfmat, ncores = 1, method = "cosine", margin = "documents")
  expect_s4_class(result, "textstat_simil_symm")
})

test_that("calculate_similarity produces a symmetric matrix with values in [-1, 1]", {
  result <- calculate_similarity(dfmat, ncores = 1, method = "cosine", margin = "documents")
  mat <- as(result, "matrix")
  # Diagonal must be 1 (self-similarity)
  expect_equal(diag(mat), c(doc1 = 1, doc2 = 1, doc3 = 1), tolerance = 1e-9)
  # All values must be in [-1, 1]
  expect_true(all(mat >= -1 - 1e-9 & mat <= 1 + 1e-9))
  # Symmetry
  expect_equal(mat, t(mat), tolerance = 1e-9)
})

test_that("calculate_similarity with ncores = 2 matches ncores = 1", {
  r1 <- calculate_similarity(dfmat, ncores = 1, method = "cosine", margin = "documents")
  r2 <- calculate_similarity(dfmat, ncores = 2, method = "cosine", margin = "documents")
  expect_equal(as(r1, "matrix"), as(r2, "matrix"), tolerance = 1e-9)
})

test_that("calculate_distance returns correct S4 class without y", {
  result <- calculate_distance(dfmat, ncores = 1, method = "euclidean", margin = "documents")
  expect_s4_class(result, "textstat_dist_symm")
})

test_that("calculate_distance produces a symmetric matrix with non-negative values", {
  result <- calculate_distance(dfmat, ncores = 1, method = "euclidean", margin = "documents")
  mat <- as(result, "matrix")
  # Diagonal must be 0 (self-distance)
  expect_equal(diag(mat), c(doc1 = 0, doc2 = 0, doc3 = 0), tolerance = 1e-9)
  # All values must be non-negative
  expect_true(all(mat >= -1e-9))
  # Symmetry
  expect_equal(mat, t(mat), tolerance = 1e-9)
})

test_that("calculate_distance with ncores = 2 matches ncores = 1", {
  r1 <- calculate_distance(dfmat, ncores = 1, method = "euclidean", margin = "documents")
  r2 <- calculate_distance(dfmat, ncores = 2, method = "euclidean", margin = "documents")
  expect_equal(as(r1, "matrix"), as(r2, "matrix"), tolerance = 1e-9)
})

test_that("calculate_similarity rejects non-dfm input", {
  expect_error(calculate_similarity("not a dfm", ncores = 1), "must be a quanteda dfm")
})

test_that("calculate_distance rejects non-dfm input", {
  expect_error(calculate_distance("not a dfm", ncores = 1), "must be a quanteda dfm")
})

test_that("calculate_similarity rejects invalid ncores", {
  expect_error(calculate_similarity(dfmat, ncores = -1), "ncores must be a single positive integer")
  expect_error(calculate_similarity(dfmat, ncores = 0), "ncores must be a single positive integer")
  expect_error(calculate_similarity(dfmat, ncores = 1.5), "ncores must be a single positive integer")
})
