make_ngram_tokens <- function() {
  quanteda::tokens(
    quanteda::corpus(c(
      doc1 = "the quick brown fox",
      doc2 = "a slow green turtle",
      doc3 = "the quick red fox",
      doc4 = "one more short doc"
    )),
    remove_punct = TRUE
  )
}

test_that("ngram_tokens validates inputs", {
  expect_error(ngram_tokens("not tokens"), "must be a quanteda tokens object")
  toks <- make_ngram_tokens()
  expect_error(ngram_tokens(toks, n = 0), "positive integers")
  expect_error(ngram_tokens(toks, skip = -1), "non-negative integers")
  expect_error(ngram_tokens(toks, concatenator = c("_", "-")), "single string")
})

test_that("ngram_tokens builds bigrams sequentially", {
  out <- ngram_tokens(make_ngram_tokens(), n = 2, ncores = 1)
  expect_true(quanteda::is.tokens(out))
  expect_true("the_quick" %in% quanteda::types(out))
  expect_true(all(grepl("_", quanteda::types(out))))
})

test_that("ngram_tokens keeps unigrams and bigrams when n = 1:2", {
  out <- ngram_tokens(make_ngram_tokens(), n = 1:2, ncores = 1)
  types <- quanteda::types(out)
  expect_true("fox" %in% types)
  expect_true("the_quick" %in% types)
})

test_that("ngram_tokens parallel output matches sequential", {
  toks <- make_ngram_tokens()
  seq_out <- ngram_tokens(toks, n = 2, ncores = 1)
  par_out <- ngram_tokens(toks, n = 2, ncores = 2, nchunks = 4)
  expect_identical(as.list(seq_out), as.list(par_out))
})

test_that("ngram_tokens honors a custom concatenator", {
  out <- ngram_tokens(make_ngram_tokens(), n = 2, concatenator = "+", ncores = 1)
  expect_true("the+quick" %in% quanteda::types(out))
})
