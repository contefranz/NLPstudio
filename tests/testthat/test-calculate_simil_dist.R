test_that("calculate_similarity works sequentially without y", {
  corp <- quanteda::corpus(c(
    doc1 = "cat dog mouse",
    doc2 = "cat bird fish",
    doc3 = "dog cat mouse bird"
  ))
  dfmat <- quanteda::dfm(quanteda::tokens(corp))
  result <- calculate_similarity(dfmat, ncores = 1, method = "cosine", margin = "documents")
  expect_s4_class(result, "textstat_simil_symm")
})

test_that("calculate_similarity works in parallel", {
  corp <- quanteda::corpus(c(
    doc1 = "cat dog mouse",
    doc2 = "cat bird fish",
    doc3 = "dog cat mouse bird"
  ))
  dfmat <- quanteda::dfm(quanteda::tokens(corp))
  result <- calculate_similarity(dfmat, ncores = 2, method = "cosine", margin = "documents")
  expect_s4_class(result, "textstat_simil_symm")
})

test_that("calculate_distance works sequentially without y", {
  corp <- quanteda::corpus(c(
    doc1 = "cat dog mouse",
    doc2 = "cat bird fish",
    doc3 = "dog cat mouse bird"
  ))
  dfmat <- quanteda::dfm(quanteda::tokens(corp))
  result <- calculate_distance(dfmat, ncores = 1, method = "euclidean", margin = "documents")
  expect_s4_class(result, "textstat_dist_symm")
})

test_that("calculate_distance works in parallel", {
  corp <- quanteda::corpus(c(
    doc1 = "cat dog mouse",
    doc2 = "cat bird fish",
    doc3 = "dog cat mouse bird"
  ))
  dfmat <- quanteda::dfm(quanteda::tokens(corp))
  result <- calculate_distance(dfmat, ncores = 2, method = "euclidean", margin = "documents")
  expect_s4_class(result, "textstat_dist_symm")
})

test_that("calculate_similarity rejects non-dfm input", {
  expect_error(calculate_similarity("not a dfm", ncores = 1), "must be a quanteda dfm")
})

test_that("calculate_distance rejects non-dfm input", {
  expect_error(calculate_distance("not a dfm", ncores = 1), "must be a quanteda dfm")
})

test_that("calculate_similarity validates parallel args", {
  corp <- quanteda::corpus(c(doc1 = "test words"))
  dfmat <- quanteda::dfm(quanteda::tokens(corp))
  expect_error(calculate_similarity(dfmat, ncores = -1), "ncores must be a single positive integer")
})
