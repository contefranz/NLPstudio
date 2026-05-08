make_plural_tokens <- function() {
  quanteda::tokens(
    quanteda::corpus(c(
      doc1 = "cats companies 2020s a",
      doc2 = "cars houses"
    )),
    remove_punct = TRUE
  )
}

test_that("singularize_tokens validates token input", {
  expect_error(singularize_tokens("not tokens"), "x must be a quanteda tokens object")
})

test_that("singularize_tokens singularizes vocabulary sequentially", {
  out <- singularize_tokens(make_plural_tokens(), ncores = 1)

  expect_equal(
    as.list(out),
    list(
      doc1 = c("cat", "company", "2020s", "a"),
      doc2 = c("car", "house")
    )
  )
})

test_that("singularize_tokens handles remove_numbers and min_char filters", {
  out <- singularize_tokens(make_plural_tokens(), ncores = 1, remove_numbers = FALSE, min_char = 2)

  expect_equal(
    as.list(out),
    list(
      doc1 = c("cat", "company", "2020"),
      doc2 = c("car", "house")
    )
  )
})

test_that("singularize_tokens handles empty vocabulary after filtering", {
  toks <- quanteda::tokens(quanteda::corpus(c(doc1 = "a 1")), remove_punct = TRUE)
  out <- singularize_tokens(toks, ncores = 1, min_char = 5)

  expect_equal(as.list(out), list(doc1 = character()))
})

test_that("singularize_tokens parallel output matches sequential output", {
  toks <- make_plural_tokens()
  seq_out <- singularize_tokens(toks, ncores = 1)
  par_out <- suppressWarnings(singularize_tokens(toks, ncores = 2, nchunks = 2))

  expect_equal(as.list(par_out), as.list(seq_out))
})

test_that("singularize_tokens exports namespace helper to PSOCK workers", {
  exported <- NULL

  testthat::local_mocked_bindings(
    .run_parallel = function(chunks, FUN, ncores, socket, export_vars = NULL,
                             export_env = parent.frame(), ...) {
      exported <<- export_vars
      lapply(chunks, FUN, ...)
    },
    .package = "NLPstudio"
  )

  out <- singularize_tokens(make_plural_tokens(), ncores = 2, nchunks = 2)

  expect_equal(as.list(out)$doc2, c("car", "house"))
  expect_true(".singularize_chunk" %in% exported)
  expect_true(".singularize" %in% exported)
  expect_true(".singularize_vector" %in% exported)
  expect_true(".singularize_word" %in% exported)
  expect_true(".restore_singular_case" %in% exported)
  expect_true(".nlp_irregular_singulars" %in% exported)
  expect_true(".nlp_uncountable_terms" %in% exported)
})

test_that("singularization helpers use internal rules", {
  chunk <- data.table::data.table(feature = c("cats", "cars"))
  out <- NLPstudio:::.singularize_chunk(chunk)

  expect_equal(out$single, c("cat", "car"))
  expect_equal(NLPstudio:::.singularize(list(feature = "houses")), "house")
})

test_that("internal singularizer handles common English and domain forms", {
  words <- c(
    "companies", "liabilities", "assets", "revenues", "businesses",
    "analyses", "indices", "children", "people", "knives", "shelves",
    "archives", "series", "species", "status", "gas"
  )

  expect_equal(
    NLPstudio:::.singularize_vector(words),
    c(
      "company", "liability", "asset", "revenue", "business",
      "analysis", "index", "child", "person", "knife", "shelf",
      "archive", "series", "species", "status", "gas"
    )
  )
})

test_that("internal singularizer preserves common case shapes", {
  expect_equal(NLPstudio:::.singularize_word("Companies"), "Company")
  expect_equal(NLPstudio:::.singularize_word("CATS"), "CAT")
  expect_equal(NLPstudio:::.singularize_word(2020L), "2020")
  expect_equal(NLPstudio:::.singularize_word("heroes"), "hero")
  expect_equal(NLPstudio:::.singularize_word(NA_character_), NA_character_)
  expect_equal(NLPstudio:::.singularize_word(character()), character())
  expect_equal(NLPstudio:::.restore_singular_case("", "term"), "term")
})
