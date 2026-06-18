make_colloc_tokens <- function() {
  quanteda::tokens(
    quanteda::corpus(c(
      doc1 = "the annual report covered cash flow and annual report risk",
      doc2 = "cash flow guidance featured in the annual report",
      doc3 = "the annual report mentioned cash flow again"
    )),
    remove_punct = TRUE
  )
}

test_that("detect_collocations validates inputs", {
  expect_error(detect_collocations("not tokens"), "must be a quanteda tokens object")
  toks <- make_colloc_tokens()
  expect_error(detect_collocations(toks, size = 1), "integers >= 2")
  expect_error(detect_collocations(toks, min_count = 0), "single positive integer")
})

test_that("detect_collocations returns an export-ready data.table", {
  out <- detect_collocations(make_colloc_tokens(), size = 2, min_count = 2)
  expect_s3_class(out, "data.table")
  expect_true(all(c("collocation", "count", "length", "lambda", "z") %in% names(out)))
  expect_true(nrow(out) >= 1L)
})

test_that("detect_collocations sorts by descending lambda", {
  out <- detect_collocations(make_colloc_tokens(), size = 2, min_count = 2)
  if (nrow(out) > 1L) {
    expect_true(!is.unsorted(rev(out$lambda)))
  }
})

test_that("detect_collocations honors min_count", {
  high <- detect_collocations(make_colloc_tokens(), size = 2, min_count = 3)
  expect_true(all(high$count >= 3L))
})
