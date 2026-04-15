test_that("calculate_readability works sequentially", {
  corp <- quanteda::corpus(c(
    doc1 = "The quick brown fox jumps over the lazy dog. It was a sunny day.",
    doc2 = "Complex financial instruments require sophisticated analysis methods.",
    doc3 = "Simple words are easy to read. Short sentences help too."
  ))
  result <- calculate_readability(corp, ncores = 1, measure = "Flesch")
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 3L)
  expect_true("doc_id" %in% names(result))
  expect_true("Flesch" %in% names(result))
})

test_that("calculate_readability works in parallel", {
  corp <- quanteda::corpus(c(
    doc1 = "The quick brown fox jumps over the lazy dog. It was a sunny day.",
    doc2 = "Complex financial instruments require sophisticated analysis methods.",
    doc3 = "Simple words are easy to read. Short sentences help too."
  ))
  result <- calculate_readability(corp, ncores = 2, measure = "Flesch")
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 3L)
})

test_that("calculate_readability preserves document order", {
  corp <- quanteda::corpus(c(
    z_doc = "Zebras run fast on the plains. They are graceful animals.",
    a_doc = "Apples are delicious fruit. They grow on trees in orchards.",
    m_doc = "Mountains are tall and majestic. Snow covers their peaks."
  ))
  result <- calculate_readability(corp, ncores = 2, measure = "Flesch")
  expect_equal(result$doc_id, c("z_doc", "a_doc", "m_doc"))
})

test_that("calculate_readability accepts character input", {
  texts <- c("The cat sat on the mat. It was warm.", "Dogs run and play outside.")
  result <- calculate_readability(texts, ncores = 1, measure = "Flesch")
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2L)
})

test_that("calculate_readability rejects bad input", {
  expect_error(calculate_readability(42, ncores = 1), "must be a quanteda corpus")
})

test_that("calculate_readability validates parallel args", {
  corp <- quanteda::corpus(c(doc1 = "Test sentence here. Another one follows."))
  expect_error(calculate_readability(corp, ncores = 0), "ncores must be a single positive integer")
})
