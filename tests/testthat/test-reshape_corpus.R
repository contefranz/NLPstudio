test_that("reshape_corpus works sequentially", {
  corp <- quanteda::corpus(c(
    doc1 = "First sentence. Second sentence.",
    doc2 = "Another paragraph. With two sentences."
  ))
  result <- reshape_corpus(corp, to = "sentences", ncores = 1)
  expect_true(quanteda::is.corpus(result))
  expect_gt(quanteda::ndoc(result), 2L)
})

test_that("reshape_corpus works in parallel", {
  corp <- quanteda::corpus(c(
    doc1 = "First sentence. Second sentence.",
    doc2 = "Another paragraph. With two sentences.",
    doc3 = "Third document. Also two sentences."
  ))
  result <- reshape_corpus(corp, to = "sentences", ncores = 2)
  expect_true(quanteda::is.corpus(result))
  expect_gt(quanteda::ndoc(result), 3L)
})

test_that("reshape_corpus rejects non-corpus input", {
  expect_error(reshape_corpus("not a corpus"), "must be a quanteda corpus")
})
