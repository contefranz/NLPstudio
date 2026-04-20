make_dt <- function(...) {
  base <- data.table::data.table(
    text     = c("First document text.", "Second document text."),
    filename = c("file1.htm", "file2.htm"),
    item     = c("item1A", "item1A")
  )
  mods <- list(...)
  for (col in names(mods)) {
    if (is.null(mods[[col]])) {
      base[[col]] <- NULL
    } else {
      base[[col]] <- mods[[col]]
    }
  }
  base
}

test_that("define_corpus rejects non-data.table input", {
  expect_error(define_corpus(data.frame(text = "a")), "must be a data.table")
  expect_error(define_corpus("text"),                 "must be a data.table")
  expect_error(define_corpus(list(text = "a")),       "must be a data.table")
})

test_that("define_corpus errors on missing 'text' column", {
  dt <- make_dt(text = NULL)
  expect_error(define_corpus(dt), "text")
})

test_that("define_corpus errors on missing 'filename' column", {
  dt <- make_dt(filename = NULL)
  expect_error(define_corpus(dt), "filename")
})

test_that("define_corpus errors on missing 'item' column", {
  dt <- make_dt(item = NULL)
  expect_error(define_corpus(dt), "item")
})

test_that("define_corpus error lists all missing columns at once", {
  dt <- data.table::data.table(x = 1:2)
  err <- tryCatch(define_corpus(dt), error = conditionMessage)
  expect_match(err, "text")
  expect_match(err, "filename")
  expect_match(err, "item")
})

test_that("define_corpus warns on duplicate doc IDs before quanteda errors", {
  dt <- make_dt(filename = c("same.htm", "same.htm"), item = c("item1A", "item1A"))
  # NLPstudio issues the warning; quanteda then errors because docnames must be unique
  warned <- FALSE
  expect_error(
    withCallingHandlers(
      define_corpus(dt),
      warning = function(w) {
        if (grepl("Non-unique doc_id", conditionMessage(w))) warned <<- TRUE
        invokeRestart("muffleWarning")
      }
    )
  )
  expect_true(warned)
})

test_that("define_corpus happy path returns a quanteda corpus", {
  dt <- make_dt()
  corp <- define_corpus(dt)
  expect_true(quanteda::is.corpus(corp))
  expect_equal(quanteda::ndoc(corp), 2L)
})

test_that("define_corpus does not leave temp columns in input data.table", {
  dt <- make_dt()
  define_corpus(dt)
  expect_false("filename2"     %in% names(dt))
  expect_false("doc_id_corpus" %in% names(dt))
  expect_false("checkdup"      %in% names(dt))
})
