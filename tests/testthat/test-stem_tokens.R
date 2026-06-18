make_stem_tokens <- function() {
  quanteda::tokens(
    quanteda::corpus(c(
      doc1 = "running runners ran easily",
      doc2 = "computational computers compute",
      doc3 = "governments governing governance",
      doc4 = "plain simple words here"
    )),
    remove_punct = TRUE
  )
}

test_that("stem_tokens validates inputs", {
  expect_error(stem_tokens("not tokens"), "must be a quanteda tokens object")
  expect_error(stem_tokens(make_stem_tokens(), language = c("english", "porter")),
               "single string")
})

test_that("stem_tokens stems vocabulary sequentially", {
  out <- stem_tokens(make_stem_tokens(), ncores = 1)
  expect_true(quanteda::is.tokens(out))
  types <- quanteda::types(out)
  expect_true("run" %in% types)       # running/runners -> run
  expect_true("comput" %in% types)    # computers/compute -> comput
})

test_that("stem_tokens parallel output matches sequential", {
  toks <- make_stem_tokens()
  seq_out <- stem_tokens(toks, ncores = 1)
  par_out <- stem_tokens(toks, ncores = 2, nchunks = 4)
  expect_identical(as.list(seq_out), as.list(par_out))
})

test_that("stem_tokens returns input unchanged for empty vocabulary", {
  toks <- quanteda::tokens(quanteda::corpus(c(doc1 = ".")), remove_punct = TRUE)
  out <- stem_tokens(toks, ncores = 1)
  expect_true(quanteda::is.tokens(out))
})
