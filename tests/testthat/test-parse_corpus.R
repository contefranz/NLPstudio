test_that("parse_corpus reports missing spacyr dependency", {
  testthat::local_mocked_bindings(
    .has_namespace = function(pkg) FALSE,
    .package = "NLPstudio"
  )

  corp <- quanteda::corpus(c(doc1 = "A simple document."))
  expect_error(parse_corpus(corp), "Package 'spacyr' is required")
})

test_that("parse_corpus validates corpus input after dependency check", {
  testthat::local_mocked_bindings(
    .has_namespace = function(pkg) TRUE,
    .package = "NLPstudio"
  )

  expect_error(parse_corpus("not a corpus"), "x must be a quanteda corpus object")
})

test_that("parse_corpus forwards parser arguments and finalizes session", {
  corp <- quanteda::corpus(c(
    doc1 = "Alpha beta.",
    doc2 = "Gamma delta."
  ))
  finalized <- FALSE
  parsed_args <- NULL

  fake_parse <- function(x, ...) {
    parsed_args <<- list(...)
    data.frame(
      doc_id = quanteda::docnames(x),
      token = paste0("token", seq_len(quanteda::ndoc(x)))
    )
  }
  fake_get <- function(pkg, name) {
    switch(
      name,
      spacy_finalize = function() {
        finalized <<- TRUE
        invisible(NULL)
      },
      spacy_parse = fake_parse,
      stop("unexpected export")
    )
  }

  testthat::local_mocked_bindings(
    .has_namespace = function(pkg) TRUE,
    .get_exported_value = fake_get,
    .package = "NLPstudio"
  )

  out <- parse_corpus(corp, lemma = TRUE, pos = FALSE, ncores = 1)

  expect_true(data.table::is.data.table(out))
  expect_equal(out$doc_id, c("doc1", "doc2"))
  expect_true(parsed_args$lemma)
  expect_false(parsed_args$pos)
  expect_true(finalized)
})

test_that("parse_corpus combines parsed chunks in the parallel branch", {
  corp <- quanteda::corpus(c(
    doc1 = "Alpha.",
    doc2 = "Beta.",
    doc3 = "Gamma."
  ))
  finalized <- FALSE

  fake_get <- function(pkg, name) {
    switch(
      name,
      spacy_finalize = function() {
        finalized <<- TRUE
        invisible(NULL)
      },
      spacy_parse = function(x, ...) {
        data.frame(doc_id = quanteda::docnames(x), token = quanteda::docnames(x))
      },
      stop("unexpected export")
    )
  }

  testthat::local_mocked_bindings(
    .has_namespace = function(pkg) TRUE,
    .get_exported_value = fake_get,
    .run_parallel = function(chunks, FUN, ncores, socket, export_vars = NULL,
                             export_env = parent.frame(), ...) {
      lapply(chunks, FUN, ...)
    },
    .package = "NLPstudio"
  )

  out <- parse_corpus(corp, ncores = 2, nchunks = 2)

  expect_true(data.table::is.data.table(out))
  expect_equal(sort(out$doc_id), c("doc1", "doc2", "doc3"))
  expect_true(finalized)
})

test_that("parse_corpus exports namespace helper to PSOCK workers", {
  corp <- quanteda::corpus(c(
    doc1 = "Alpha.",
    doc2 = "Beta."
  ))
  exported <- NULL

  fake_get <- function(pkg, name) {
    switch(
      name,
      spacy_finalize = function() invisible(NULL),
      spacy_parse = function(x, ...) {
        data.frame(doc_id = quanteda::docnames(x), token = quanteda::docnames(x))
      },
      stop("unexpected export")
    )
  }

  testthat::local_mocked_bindings(
    .has_namespace = function(pkg) TRUE,
    .get_exported_value = fake_get,
    .run_parallel = function(chunks, FUN, ncores, socket, export_vars = NULL,
                             export_env = parent.frame(), ...) {
      exported <<- export_vars
      lapply(chunks, FUN, ...)
    },
    .package = "NLPstudio"
  )

  parse_corpus(corp, ncores = 2, nchunks = 2)

  expect_true(".parse_chunk" %in% exported)
  expect_true(".get_exported_value" %in% exported)
})

test_that(".parse_chunk returns a data.table", {
  fake_get <- function(pkg, name) {
    function(x, ...) {
      data.frame(doc_id = quanteda::docnames(x), token = "token")
    }
  }
  testthat::local_mocked_bindings(
    .get_exported_value = fake_get,
    .package = "NLPstudio"
  )

  out <- NLPstudio:::.parse_chunk(quanteda::corpus(c(doc1 = "Alpha.")))
  expect_true(data.table::is.data.table(out))
  expect_equal(out$doc_id, "doc1")
})

test_that("parse_corpus integrates with real spacyr backend", {
  skip_on_ci()
  skip_if_not_installed("spacyr")
  if (!nzchar(Sys.getenv("NLPSTUDIO_TEST_SPACYR"))) {
    skip("Set NLPSTUDIO_TEST_SPACYR=1 to run the spacyr integration test.")
  }

  corp <- quanteda::corpus(c(
    doc1 = "Cats chase mice.",
    doc2 = "Markets react quickly."
  ))

  out <- parse_corpus(corp, lemma = TRUE, pos = TRUE)

  expect_true(data.table::is.data.table(out))
  expect_true(nrow(out) > 0L)
  expect_setequal(unique(out$doc_id), c("doc1", "doc2"))
  expect_true("pos" %in% names(out))
  expect_true(any(nzchar(out$pos)))
})
