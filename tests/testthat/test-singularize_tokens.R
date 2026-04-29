make_plural_tokens <- function() {
  quanteda::tokens(
    quanteda::corpus(c(
      doc1 = "cats companies 2020s a",
      doc2 = "cars houses"
    )),
    remove_punct = TRUE
  )
}

fake_singularize <- function(x) {
  map <- c(
    cats = "cat",
    companies = "company",
    cars = "car",
    houses = "house",
    `2020s` = "2020"
  )
  out <- unname(map[x])
  out[is.na(out)] <- x[is.na(out)]
  out
}

test_that("singularize_tokens reports missing pluralize dependency", {
  testthat::local_mocked_bindings(
    .has_namespace = function(pkg) FALSE,
    .package = "NLPstudio"
  )

  expect_error(
    singularize_tokens(make_plural_tokens()),
    "Package 'pluralize' is required"
  )
})

test_that("singularize_tokens validates token input", {
  testthat::local_mocked_bindings(
    .has_namespace = function(pkg) TRUE,
    .package = "NLPstudio"
  )

  expect_error(singularize_tokens("not tokens"), "x must be a quanteda tokens object")
})

test_that("singularize_tokens singularizes vocabulary sequentially", {
  testthat::local_mocked_bindings(
    .has_namespace = function(pkg) TRUE,
    .get_exported_value = function(pkg, name) fake_singularize,
    .package = "NLPstudio"
  )

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
  testthat::local_mocked_bindings(
    .has_namespace = function(pkg) TRUE,
    .get_exported_value = function(pkg, name) fake_singularize,
    .package = "NLPstudio"
  )

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
  testthat::local_mocked_bindings(
    .has_namespace = function(pkg) TRUE,
    .get_exported_value = function(pkg, name) fake_singularize,
    .package = "NLPstudio"
  )

  toks <- quanteda::tokens(quanteda::corpus(c(doc1 = "a 1")), remove_punct = TRUE)
  out <- singularize_tokens(toks, ncores = 1, min_char = 5)

  expect_equal(as.list(out), list(doc1 = character()))
})

test_that("singularize_tokens parallel output matches sequential output", {
  skip_if_not_installed("pluralize")

  toks <- make_plural_tokens()
  seq_out <- singularize_tokens(toks, ncores = 1)
  par_out <- suppressWarnings(singularize_tokens(toks, ncores = 2, nchunks = 2))

  expect_equal(as.list(par_out), as.list(seq_out))
})

test_that("singularize_tokens exports namespace helper to PSOCK workers", {
  exported <- NULL

  testthat::local_mocked_bindings(
    .has_namespace = function(pkg) TRUE,
    .get_exported_value = function(pkg, name) fake_singularize,
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
  expect_true(".get_exported_value" %in% exported)
})

test_that("singularization helpers call the pluralize backend", {
  testthat::local_mocked_bindings(
    .get_exported_value = function(pkg, name) fake_singularize,
    .package = "NLPstudio"
  )

  chunk <- data.table::data.table(feature = c("cats", "cars"))
  out <- NLPstudio:::.singularize_chunk(chunk)

  expect_equal(out$single, c("cat", "car"))
  expect_equal(NLPstudio:::.singularize(list(feature = "houses")), "house")
})
