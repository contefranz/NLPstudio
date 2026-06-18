make_weight_dfm <- function() {
  quanteda::dfm(quanteda::tokens(
    quanteda::corpus(c(
      doc1 = "money money risk",
      doc2 = "risk growth growth growth",
      doc3 = "policy policy money growth"
    )),
    remove_punct = TRUE
  ))
}

test_that("weight_dfm validates inputs", {
  expect_error(weight_dfm("not a dfm"), "must be a quanteda dfm object")
  expect_error(weight_dfm(make_weight_dfm(), scheme = "nope"))
})

test_that("weight_dfm applies tf-idf by default", {
  out <- weight_dfm(make_weight_dfm())
  expect_true(quanteda::is.dfm(out))
  # a term appearing in every document gets zero idf weight
  m <- as.matrix(out)
  expect_true(all(dim(m) == dim(as.matrix(make_weight_dfm()))))
})

test_that("weight_dfm prop scheme makes rows sum to one", {
  out <- weight_dfm(make_weight_dfm(), scheme = "prop")
  rs <- rowSums(as.matrix(out))
  expect_equal(unname(rs), rep(1, length(rs)), tolerance = 1e-8)
})

test_that("weight_dfm boolean scheme yields 0/1 entries", {
  out <- weight_dfm(make_weight_dfm(), scheme = "boolean")
  m <- as.matrix(out)
  expect_true(all(m %in% c(0, 1)))
})
