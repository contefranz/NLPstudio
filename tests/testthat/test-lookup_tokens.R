make_lookup_tokens <- function() {
  corp <- quanteda::corpus(c(
    doc_b = "Cats and dogs run quickly.",
    doc_a = "Markets and firms react."
  ))
  quanteda::tokens(corp, remove_punct = TRUE)
}

make_lookup_dictionary <- function() {
  quanteda::dictionary(list(
    animals = c("cat*", "dog*"),
    economics = c("market*", "firm*")
  ))
}

test_that("lookup_tokens validates input and parallel arguments", {
  expect_error(
    lookup_tokens("not tokens", dictionary = make_lookup_dictionary()),
    "x must be a quanteda tokens object"
  )

  toks <- make_lookup_tokens()
  expect_error(
    lookup_tokens(toks, dictionary = make_lookup_dictionary(), ncores = 0),
    "ncores must be a single positive integer"
  )
  expect_error(
    lookup_tokens(toks, dictionary = make_lookup_dictionary(), nchunks = 0),
    "nchunks must be a single positive integer"
  )
})

test_that("lookup_tokens applies dictionary lookup and preserves order", {
  toks <- make_lookup_tokens()
  out <- lookup_tokens(toks, dictionary = make_lookup_dictionary(), ncores = 1)

  expect_true(quanteda::is.tokens(out))
  expect_equal(quanteda::docnames(out), quanteda::docnames(toks))
  expect_equal(
    as.list(out),
    list(
      doc_b = c("animals", "animals"),
      doc_a = c("economics", "economics")
    )
  )
})

test_that("lookup_tokens passes user arguments through to quanteda", {
  toks <- make_lookup_tokens()
  out <- lookup_tokens(
    toks,
    dictionary = make_lookup_dictionary(),
    exclusive = FALSE,
    capkeys = TRUE,
    ncores = 1
  )

  expect_equal(
    as.list(out),
    list(
      doc_b = c("ANIMALS", "and", "ANIMALS", "run", "quickly"),
      doc_a = c("ECONOMICS", "and", "ECONOMICS", "react")
    )
  )
})

test_that("lookup_tokens parallel output matches sequential output", {
  toks <- make_lookup_tokens()
  seq_out <- lookup_tokens(toks, dictionary = make_lookup_dictionary(), ncores = 1)
  par_out <- suppressWarnings(
    lookup_tokens(toks, dictionary = make_lookup_dictionary(), ncores = 2, nchunks = 2)
  )

  expect_equal(as.list(par_out), as.list(seq_out))
  expect_equal(quanteda::docnames(par_out), quanteda::docnames(seq_out))
})
