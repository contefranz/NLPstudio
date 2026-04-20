write_json_fixture <- function(path, lines) {
  writeLines(lines, con = path, useBytes = TRUE)
  path
}

make_fixture_paths <- function() {
  dir.create(tmp <- tempfile("json-fixtures-"))
  list(
    ten_k = file.path(tmp, "ten_k.json"),
    ten_q = file.path(tmp, "ten_q.json"),
    eight_k = file.path(tmp, "eight_k.json"),
    loan = file.path(tmp, "loan.json"),
    loan_empty = file.path(tmp, "loan_empty.json")
  )
}

write_all_fixtures <- function(paths) {
  write_json_fixture(paths$ten_k, c(
    "{",
    '  "cik": "320193",',
    '  "company": "Apple Inc.",',
    '  "filing_type": "10-K",',
    '  "filing_date": "2022-10-28",',
    '  "period_of_report": "2022-09-24",',
    '  "sic": "3571",',
    '  "filing_html_index": "https://example.com/index.html",',
    '  "htm_filing_link": "https://example.com/filing.htm",',
    '  "complete_text_filing_link": "https://example.com/filing.txt",',
    '  "filename": "320193_10K_2022_0000320193-22-000108.htm",',
    '  "item_1": "Business text",',
    '  "item_1A": "Risk factors text",',
    '  "item_7": "Management discussion text"',
    "}"
  ))
  write_json_fixture(paths$ten_q, c(
    "{",
    '  "cik": "320193",',
    '  "company": "Apple Inc.",',
    '  "filing_type": "10-Q",',
    '  "filing_date": "2024-05-03",',
    '  "period_of_report": "2024-03-30",',
    '  "sic": "3571",',
    '  "filename": "320193_10Q_2024_0000320193-24-000069.htm",',
    '  "part_1": "Part one full text",',
    '  "part_1_item_1": "Part one item one",',
    '  "part_1_item_2": "Part one item two",',
    '  "part_2": "Part two full text",',
    '  "part_2_item_1A": "Part two risk factors"',
    "}"
  ))
  write_json_fixture(paths$eight_k, c(
    "{",
    '  "cik": "320193",',
    '  "company": "Apple Inc.",',
    '  "filing_type": "8-K",',
    '  "filing_date": "2022-08-19",',
    '  "period_of_report": "2022-08-17",',
    '  "sic": "3571",',
    '  "filename": "320193_8K_2022_0001193125-22-225365.htm",',
    '  "item_1.01": "Entry into a material definitive agreement",',
    '  "item_9.01": "Financial statements and exhibits"',
    "}"
  ))
  write_json_fixture(paths$loan, c(
    "{",
    '  "doc_id": "0001747172_EX-10.1_2025_0001213900-25-011427",',
    '  "cik": "1747172",',
    '  "coname": "Kayne Anderson BDC, Inc.",',
    '  "form": "8-K",',
    '  "type": "EX-10.1",',
    '  "description": "LOAN AND SECURITY AGREEMENT",',
    '  "filing_date": "2025-02-10",',
    '  "accession": "0001213900-25-011427",',
    '  "sequence": "2",',
    '  "filename": "1747172_LOAN_2025_000121390025011427_2.htm",',
    '  "index_url": "https://example.com/index.htm",',
    '  "htm_file_link": "https://example.com/exhibit.htm",',
    '  "incorporated_by_reference": false,',
    '  "definitions": "Definitions section text",',
    '  "commitments": "",',
    '  "interest_and_fees": "",',
    '  "conditions_precedent": "Conditions precedent section text",',
    '  "representations": "",',
    '  "covenants": "",',
    '  "guarantees_and_security": "",',
    '  "events_of_default": "",',
    '  "administrative_agents": "",',
    '  "miscellaneous": "Miscellaneous section text"',
    "}"
  ))
  write_json_fixture(paths$loan_empty, c(
    "{",
    '  "doc_id": "loan_empty_doc",',
    '  "cik": "1747172",',
    '  "coname": "Kayne Anderson BDC, Inc.",',
    '  "form": "8-K",',
    '  "type": "EX-10.1",',
    '  "filing_date": "2025-02-10",',
    '  "filename": "loan_empty.htm",',
    '  "definitions": "",',
    '  "commitments": "",',
    '  "interest_and_fees": "",',
    '  "conditions_precedent": "",',
    '  "representations": "",',
    '  "covenants": "",',
    '  "guarantees_and_security": "",',
    '  "events_of_default": "",',
    '  "administrative_agents": "",',
    '  "miscellaneous": ""',
    "}"
  ))
  invisible(paths)
}

test_that("from_json_to_df imports sec-crawler style 10-K JSONs", {
  paths <- write_all_fixtures(make_fixture_paths())
  out <- from_json_to_df(paths$ten_k, what = "10-K", ncores = 1)

  expect_s3_class(out, "data.table")
  expect_equal(out$item, c("item_1", "item_1A", "item_7"))
  expect_equal(out$text, c("Business text", "Risk factors text", "Management discussion text"))
  expect_true(all(out$filing_type == "10-K"))
  expect_true(all(out$fyear == 2022L))
  expect_true(all(c("filing_html_index", "htm_filing_link", "complete_text_filing_link") %in% names(out)))
})

test_that("from_json_to_df imports 10-Q parts and part items", {
  paths <- write_all_fixtures(make_fixture_paths())
  out <- from_json_to_df(paths$ten_q, what = "10-Q", ncores = 1)

  expect_equal(
    out$item,
    c("part_1", "part_1_item_1", "part_1_item_2", "part_2", "part_2_item_1A")
  )
  expect_true(all(out$filing_type == "10-Q"))
  expect_true(all(out$fyear == 2024L))
})

test_that("from_json_to_df preserves decimal 8-K item identifiers", {
  paths <- write_all_fixtures(make_fixture_paths())
  out <- from_json_to_df(paths$eight_k, what = "8-K", ncores = 1)

  expect_equal(out$item, c("item_1.01", "item_9.01"))
  expect_true(all(out$filing_type == "8-K"))
})

test_that("from_json_to_df normalizes loan metadata and derives fyear from filing_date", {
  paths <- write_all_fixtures(make_fixture_paths())
  out <- from_json_to_df(paths$loan, what = "loan", ncores = 1)

  expect_equal(out$item, c("definitions", "conditions_precedent", "miscellaneous"))
  expect_true(all(c("company", "filing_type", "exhibit_type", "doc_id", "description") %in% names(out)))
  expect_false("coname" %in% names(out))
  expect_false("form" %in% names(out))
  expect_false("type" %in% names(out))
  expect_true(all(out$company == "Kayne Anderson BDC, Inc."))
  expect_true(all(out$filing_type == "8-K"))
  expect_true(all(out$exhibit_type == "EX-10.1"))
  expect_true(all(out$fyear == 2025L))
})

test_that("from_json_to_df auto-infers mixed filing families", {
  paths <- write_all_fixtures(make_fixture_paths())
  out <- from_json_to_df(
    c(paths$ten_k, paths$ten_q, paths$eight_k, paths$loan),
    ncores = 1
  )

  expect_equal(nrow(out), 13L)
  expect_true(all(c("item_1A", "part_1_item_1", "item_1.01", "definitions") %in% out$item))
  expect_true(all(c("10-K", "10-Q", "8-K") %in% out$filing_type))
  expect_true("exhibit_type" %in% names(out))
})

test_that("from_json_to_df drops or keeps empty loan sections based on drop_empty_text", {
  paths <- write_all_fixtures(make_fixture_paths())

  dropped <- from_json_to_df(paths$loan_empty, what = "loan", ncores = 1)
  kept <- from_json_to_df(
    paths$loan_empty,
    what = "loan",
    drop_empty_text = FALSE,
    ncores = 1
  )

  expect_equal(nrow(dropped), 0L)
  expect_equal(nrow(kept), 10L)
  expect_true(all(kept$text == ""))
})

test_that("from_json_to_df returns the same result for ncores = 1 and ncores = 2", {
  paths <- write_all_fixtures(make_fixture_paths())
  files <- c(paths$ten_k, paths$ten_q, paths$eight_k, paths$loan)
  cl_ok <- TRUE
  tryCatch({
    cl <- parallel::makeCluster(2)
    parallel::stopCluster(cl)
  }, error = function(e) {
    cl_ok <<- FALSE
  })
  if (!cl_ok) {
    skip("Parallel PSOCK cluster sockets are unavailable in this environment")
  }

  seq_out <- from_json_to_df(files, ncores = 1)
  par_out <- from_json_to_df(files, ncores = 2, socket = "PSOCK")

  data.table::setorderv(seq_out, c("filename", "item", "text"))
  data.table::setorderv(par_out, c("filename", "item", "text"))
  expect_equal(par_out, seq_out)
})
