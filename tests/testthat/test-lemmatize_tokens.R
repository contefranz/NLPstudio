make_lemma_tokens <- function() {
  quanteda::tokens(
    quanteda::corpus(c(
      doc1 = "mice were running",
      doc2 = "the geese are flying"
    )),
    remove_punct = TRUE
  )
}

lemma_df <- function() {
  data.frame(
    token = c("mice", "were", "running", "geese", "are", "flying"),
    lemma = c("mouse", "be", "run", "goose", "be", "fly"),
    stringsAsFactors = FALSE
  )
}

test_that("lemmatize_tokens validates inputs", {
  expect_error(lemmatize_tokens("not tokens"), "must be a quanteda tokens object")
  expect_error(lemmatize_tokens(make_lemma_tokens(), engine = "lookup", lemma = NULL),
               "requires a `lemma` map")
  expect_error(lemmatize_tokens(make_lemma_tokens(), engine = "lookup", lemma = 1L),
               "named character vector or a data.frame")
})

test_that("lemmatize_tokens applies a data.frame lemma map", {
  out <- lemmatize_tokens(make_lemma_tokens(), engine = "lookup", lemma = lemma_df())
  types <- quanteda::types(out)
  expect_false("mice" %in% types)
  expect_true("mouse" %in% types)
  expect_true("be" %in% types)
})

test_that("lemmatize_tokens applies a named-vector lemma map", {
  out <- lemmatize_tokens(make_lemma_tokens(), engine = "lookup",
                          lemma = c(mice = "mouse", geese = "goose"))
  types <- quanteda::types(out)
  expect_true(all(c("mouse", "goose") %in% types))
  expect_false(any(c("mice", "geese") %in% types))
})

test_that("lemmatize_tokens rejects an unnamed character map", {
  expect_error(
    lemmatize_tokens(make_lemma_tokens(), engine = "lookup", lemma = c("mouse", "goose")),
    "named vector"
  )
})

test_that("lemmatize_tokens rejects a data.frame missing required columns", {
  bad <- data.frame(word = "mice", base = "mouse")
  expect_error(
    lemmatize_tokens(make_lemma_tokens(), engine = "lookup", lemma = bad),
    "must contain 'token' and 'lemma' columns"
  )
})

test_that("lemmatize_tokens spacy engine requires spacyr", {
  testthat::skip_if(requireNamespace("spacyr", quietly = TRUE),
                    "spacyr installed; spaCy runtime path is exercised separately")
  expect_error(
    lemmatize_tokens(make_lemma_tokens(), engine = "spacy"),
    "requires the 'spacyr' package"
  )
})
