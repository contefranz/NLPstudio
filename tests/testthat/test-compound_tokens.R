make_compound_tokens <- function() {
  quanteda::tokens(
    quanteda::corpus(c(
      doc1 = "the annual report described cash flow risk",
      doc2 = "annual report disclosures mention cash flow",
      doc3 = "another cash flow line in the annual report",
      doc4 = "padding document without phrases"
    )),
    remove_punct = TRUE
  )
}

test_that("compound_tokens validates inputs", {
  expect_error(compound_tokens("not tokens", pattern = "a b"),
               "must be a quanteda tokens object")
  expect_error(compound_tokens(make_compound_tokens()), "pattern must be supplied")
})

test_that("compound_tokens compounds character phrases", {
  out <- compound_tokens(make_compound_tokens(),
                         pattern = c("annual report", "cash flow"), ncores = 1)
  expect_true(quanteda::is.tokens(out))
  expect_true("annual_report" %in% quanteda::types(out))
  expect_true("cash_flow" %in% quanteda::types(out))
})

test_that("compound_tokens composes with detect_collocations output", {
  toks <- make_compound_tokens()
  col <- detect_collocations(toks, size = 2, min_count = 2)
  out <- compound_tokens(toks, pattern = col, ncores = 1)
  expect_true(any(grepl("_", quanteda::types(out))))
})

test_that("compound_tokens rejects a data.frame without a collocation column", {
  bad <- data.frame(phrase = "annual report")
  expect_error(compound_tokens(make_compound_tokens(), pattern = bad),
               "must contain a 'collocation' column")
})

test_that("compound_tokens parallel output matches sequential", {
  toks <- make_compound_tokens()
  pat <- c("annual report", "cash flow")
  seq_out <- compound_tokens(toks, pattern = pat, ncores = 1)
  par_out <- compound_tokens(toks, pattern = pat, ncores = 2, nchunks = 4)
  expect_identical(as.list(seq_out), as.list(par_out))
})
