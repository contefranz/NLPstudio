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

test_that("reshape_corpus: parallel output matches sequential numerically", {
  corp <- quanteda::corpus(c(
    doc1 = "First sentence. Second sentence.",
    doc2 = "Another paragraph. With two sentences.",
    doc3 = "Third document. Also two sentences."
  ))
  seq_result <- reshape_corpus(corp, to = "sentences", ncores = 1)
  par_result <- reshape_corpus(corp, to = "sentences", ncores = 2)
  # Same number of sentences and identical text content (order-insensitive)
  expect_equal(quanteda::ndoc(seq_result), quanteda::ndoc(par_result))
  seq_texts <- sort(unname(as.character(seq_result)))
  par_texts <- sort(unname(as.character(par_result)))
  expect_equal(seq_texts, par_texts)
})
