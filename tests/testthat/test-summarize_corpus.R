test_that("summarize_corpus works sequentially", {
  corp <- quanteda::corpus(c(
    doc1 = "The cat sat on the mat. It was warm.",
    doc2 = "Dogs run and play outside. They are happy.",
    doc3 = "Birds fly high. They sing beautiful songs."
  ))
  result <- summarize_corpus(corp, ncores = 1)
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 3L)
  expect_true("doc_id" %in% names(result))
})

test_that("summarize_corpus works in parallel", {
  corp <- quanteda::corpus(c(
    doc1 = "The cat sat on the mat. It was warm.",
    doc2 = "Dogs run and play outside. They are happy.",
    doc3 = "Birds fly high. They sing beautiful songs."
  ))
  result <- summarize_corpus(corp, ncores = 2)
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 3L)
})

test_that("summarize_corpus rejects non-corpus input", {
  expect_error(summarize_corpus("not a corpus"), "must be a quanteda corpus")
})

test_that("summarize_corpus: parallel output matches sequential numerically", {
  corp <- quanteda::corpus(c(
    doc1 = "The cat sat on the mat. It was warm.",
    doc2 = "Dogs run and play outside. They are happy.",
    doc3 = "Birds fly high. They sing beautiful songs."
  ))
  seq_result <- summarize_corpus(corp, ncores = 1)
  par_result <- summarize_corpus(corp, ncores = 2)
  expect_equal(seq_result, par_result)
})
