test_that("tokenize_corpus works sequentially", {
  corp <- quanteda::corpus(c(
    doc1 = "The cat sat on the mat",
    doc2 = "Dogs are running fast",
    doc3 = "Birds fly high in the sky"
  ))
  toks <- tokenize_corpus(corp, ncores = 1)
  expect_true(quanteda::is.tokens(toks))
  expect_equal(quanteda::ndoc(toks), 3L)
  expect_equal(quanteda::docnames(toks), c("doc1", "doc2", "doc3"))
})

test_that("tokenize_corpus works in parallel with 2 cores", {
  corp <- quanteda::corpus(c(
    doc1 = "The cat sat on the mat",
    doc2 = "Dogs are running fast",
    doc3 = "Birds fly high in the sky"
  ))
  toks <- tokenize_corpus(corp, ncores = 2)
  expect_true(quanteda::is.tokens(toks))
  expect_equal(quanteda::ndoc(toks), 3L)
  expect_equal(quanteda::docnames(toks), c("doc1", "doc2", "doc3"))
})

test_that("tokenize_corpus handles single-document corpus", {
  corp <- quanteda::corpus(c(doc1 = "Single document test"))
  toks <- tokenize_corpus(corp, ncores = 1)
  expect_true(quanteda::is.tokens(toks))
  expect_equal(quanteda::ndoc(toks), 1L)
})

test_that("tokenize_corpus preserves document order", {
  corp <- quanteda::corpus(c(
    z_doc = "zebra",
    a_doc = "apple",
    m_doc = "mango"
  ))
  toks <- tokenize_corpus(corp, ncores = 2)
  expect_equal(quanteda::docnames(toks), c("z_doc", "a_doc", "m_doc"))
})

test_that("tokenize_corpus rejects non-corpus input", {
  expect_error(tokenize_corpus("not a corpus"), "must be a quanteda corpus")
})

test_that("tokenize_corpus: parallel output matches sequential numerically", {
  corp <- quanteda::corpus(c(
    doc1 = "The cat sat on the mat",
    doc2 = "Dogs are running fast",
    doc3 = "Birds fly high in the sky"
  ))
  seq_toks <- tokenize_corpus(corp, ncores = 1)
  par_toks <- tokenize_corpus(corp, ncores = 2)
  expect_equal(as.list(seq_toks), as.list(par_toks))
})

test_that("tokenize_corpus validates parallel args", {
  corp <- quanteda::corpus(c(doc1 = "test"))
  expect_error(tokenize_corpus(corp, ncores = 0), "ncores must be a single positive integer")
  expect_error(tokenize_corpus(corp, ncores = 1, nchunks = -1), "nchunks must be a single positive integer")
})
