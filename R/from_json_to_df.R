if ( getRversion() >= "2.15.1" ) {
  utils::globalVariables( c( "jcol", "item", "fyear", ".", "filing_date",
                             "period_of_report", "accession_number",
                             "text", "year_filed", "date_filed", "fyear_end", "sic") )
}
#' Convert JSON to data.table
#'
#' Converts a vector of JSON file paths into a unified [data.table::data.table]
#' suitable for downstream analysis. The function is optimized for large-scale
#' input (thousands of JSON files) and leverages both chunking and user-selected
#' parallel backends to remain efficient and memory-safe.
#' 
#' @inheritParams tokenize_corpus
#' @param files Character vector of JSON file paths.
#' @param nchunks Integer. Number of chunks to split the input file vector into.
#'   Defaults to `ncores`. The chunk size is computed as
#'   `ceiling(length(files) / nchunks)`. Ignored if `max_chunk_size` is
#'   explicitly provided via `...`.
#' @param drop_late_filers Logical. If `TRUE`, removes filings considered
#'   "late" (filing year greater than fiscal year + 1). Default `FALSE`.
#' @param what Character or `NULL`. Recommended explicit selector for the JSON
#'   family being imported. Supported values are `"10-K"`, `"10-Q"`, `"8-K"`,
#'   and `"loan"`. When `NULL` (default), the function infers the family from
#'   the JSON keys for backward compatibility.
#' @param drop_empty_text Logical. If `TRUE` (default), drop rows where the
#'   extracted section text is empty or missing after melting.
#' @param max_chunk_size Integer. If provided, sets the exact number of files
#'   per chunk, overriding the value derived from `nchunks`. Use this for
#'   fine-grained memory control when file sizes vary significantly.
#' @param ... Additional arguments passed to internal processing steps.

#' @details
#' Internally the function proceeds in three phases:
#'
#' 1. **Read & parse** – The input file paths are divided into chunks. By
#'    default, chunk size is derived from `nchunks` as
#'    `ceiling(length(files) / nchunks)`. If `max_chunk_size` is explicitly
#'    provided, it overrides this calculation. Each chunk is read in parallel
#'    with [RcppSimdJson::fload()] and converted to data tables.
#'
#' 2. **Reshape** – Each parsed table is normalized and reshaped from wide to
#'   long format in parallel. The selected text columns depend on `what`:
#'   `"10-K"` uses `item_*`/`section_*`, `"10-Q"` uses `part_*` and
#'   `part_*_item_*`, `"8-K"` uses `item_*`, and `"loan"` uses the canonical
#'   section names produced by `sec-crawler`. When `what = NULL`, the function
#'   infers the family from the JSON keys for backward compatibility.
#'
#' 3. **Combine & clean** – Melted tables are bound together, date columns
#'   converted, fiscal year (`fyear`) derived, late filers optionally dropped,
#'   and column order standardized.
#'
#' Parallel backends are controlled with the `socket` argument:
#'
#' * When `socket = "PSOCK"`, [parallel::clusterApplyLB()] is used, which
#'   dynamically balances work across workers and is portable across operating
#'   systems.
#' * When `socket = "FORK"`, [parallel::mclapply()] is used, which can be
#'   faster on Linux/macOS because it avoids copying large objects to workers.
#'
#' @return A [data.table::data.table] with one row per `(document × item)`
#'   and columns:
#'   - `cik`: Central Index Key (integer)  
#'   - `filing_date`, `period_of_report` (IDate)  
#'   - `fyear`: fiscal year (integer)  
#'   - `sic`: industry code (integer)  
#'   - `item`: item identifier (character)  
#'   - `text`: filing text content (character)  
#'   - plus any additional metadata extracted upstream
#' @section Chunking strategy:
#' The function offers two ways to control chunking:
#' - **`nchunks`** (default): Set the number of batches. With 7000 files
#'   and `nchunks = 4`, each batch contains ~1750 files.
#' - **`max_chunk_size`** (explicit): Set the exact batch size. With 7000
#'   files and `max_chunk_size = 500`, you get 14 batches of 500 files each.
#' If both are relevant, `max_chunk_size` takes precedence.
#' 
#' @section Efficiency considerations:
#' - **RcppSimdJson** is used for parsing, which is significantly faster than
#'   base or jsonlite parsers on large files.
#' - Chunking is controlled by `nchunks` for load balancing, while `max_chunk_size`
#'   provides an upper bound on files per chunk to prevent memory overload.
#' - `socket = "FORK"` is generally preferred on Linux/macOS for speed, while
#'   `socket = "PSOCK"` is more portable and provides dynamic load balancing.
#'   
#' @seealso [RcppSimdJson::fload()], [data.table::melt()],
#'   [parallel::mclapply()], [parallel::clusterApplyLB()]
#'
#' @import data.table
#' @export
from_json_to_df <- function(files, ncores = 1, nchunks = ncores,
                            socket = c("PSOCK", "FORK"), drop_late_filers = FALSE,
                            what = NULL, drop_empty_text = TRUE,
                            max_chunk_size = NULL, ...) {

  if (!requireNamespace("RcppSimdJson", quietly = TRUE)) {
    stop("The 'RcppSimdJson' package is required for fast JSON parsing. Please install it.")
  }
  if (!is.character(files)) {
    stop("`files` must be a character vector of file paths")
  }
  if (!is.null(what)) {
    what <- match.arg(what, choices = c("10-K", "10-Q", "8-K", "loan"))
  }
  if (!is.logical(drop_empty_text) || length(drop_empty_text) != 1L || is.na(drop_empty_text)) {
    stop("`drop_empty_text` must be TRUE or FALSE")
  }

  cli::cli_h2("Flattening JSON files")

  socket <- match.arg(socket)
  .validate_parallel_args(ncores, nchunks)
  # If max_chunk_size is explicitly provided, use it; otherwise derive from nchunks
  if (!is.null(max_chunk_size)) {
    chunk_size <- max_chunk_size
  } else {
    chunk_size <- ceiling(length(files) / nchunks)
  }
  
  # Phase 1: Read
  cli::cli_alert_info("Reading JSON files with RcppSimdJson")
  temp <- .chunked_read_json(files, ncores, chunk_size = chunk_size, socket = socket)

  # Phase 2: Reshape
  cli::cli_alert_info("Reshaping JSON data in parallel with {ncores} cores")
  melt_res <- .parallel_melt(temp, ncores, socket = socket, what = what,
                             drop_empty_text = drop_empty_text)
  out <- data.table::rbindlist(lapply(melt_res, `[[`, "data"), fill = TRUE)
  skipped_no_text <- sum(vapply(melt_res, `[[`, logical(1), "skipped"))
  if (skipped_no_text > 0L) {
    warning(sprintf(
      "Skipped %d JSON file(s) because no recognized text fields were found for the selected `what` value.",
      skipped_no_text
    ), call. = FALSE)
  }
  
  # Phase 3: Post-process
  out <- .postprocess_json_dt(out, drop_late_filers = drop_late_filers)
  
  if (drop_late_filers) {
    cli::cli_alert_info("Late filer filtering applied where filing metadata were available")
  }
  
  cli::cli_alert_success("Conversion has been successful")
  return(out[])
}


#' @keywords internal
.chunked_read_json <- function(files, ncores, chunk_size, socket) {
  chunks <- split(files, ceiling(seq_along(files) / chunk_size))
  out_list <- vector("list", 0L)
  
  for (i in seq_along(chunks)) {
    cli::cli_alert_info("Processing chunk {i}/{length(chunks)} with {length(chunks[[i]])} files")
    temp <- .parallel_read_json(chunks[[i]], ncores, socket = socket)
    out_list <- c(out_list, temp)
    rm(temp); gc(verbose = FALSE)
  }
  out_list
}

#' @keywords internal
.parallel_read_json <- function(files, ncores, socket) {
  # Sequential fast-path: read each file directly, no cluster overhead
  if (ncores < 2L || length(files) <= 1L) {
    fload_fun <- getExportedValue("RcppSimdJson", "fload")
    return(lapply(files, function(f) data.table::as.data.table(fload_fun(f))))
  }
  # Parallel path: distribute file batches across workers so that
  # getExportedValue() is called once per worker, not once per file.
  groups  <- split(files, rep_len(seq_len(ncores), length(files)))
  batches <- Filter(function(b) length(b) > 0L, groups)
  .read_json_batch <- function(batch) {
    fload_fun <- getExportedValue("RcppSimdJson", "fload")
    lapply(batch, function(f) data.table::as.data.table(fload_fun(f)))
  }
  nested <- .run_parallel(batches, .read_json_batch, ncores, socket,
                          export_vars = c(".read_json_batch"),
                          export_env  = environment())
  # Flatten one level: list-of-lists -> flat list of data.tables
  unlist(nested, recursive = FALSE)
}

#' @keywords internal
.parallel_melt <- function(temp, ncores, socket, what, drop_empty_text) {
  # Sequential fast-path
  if (ncores < 2L || length(temp) <= 1L) {
    return(lapply(temp, .melt_json_record, what = what, drop_empty_text = drop_empty_text))
  }
  # Parallel path: export the data and the helper together
  .run_parallel(temp, .melt_json_record, ncores, socket,
                export_vars = c(
                  ".melt_json_record", ".infer_json_family", ".normalize_json_dt",
                  ".measure_vars_for_what", ".loan_measure_vars"
                ),
                export_env  = environment(),
                what = what,
                drop_empty_text = drop_empty_text)
}

#' @keywords internal
.melt_json_record <- function(dt, what, drop_empty_text) {
  resolved_what <- .infer_json_family(dt, what)
  dt <- .normalize_json_dt(dt, resolved_what)
  meas <- .measure_vars_for_what(names(dt), resolved_what)
  if (length(meas) == 0L) {
    return(list(data = data.table::data.table(), skipped = TRUE))
  }
  idv <- setdiff(names(dt), meas)
  melted <- data.table::melt(dt,
                             id.vars         = idv,
                             measure.vars    = meas,
                             variable.name   = "item",
                             value.name      = "text",
                             variable.factor = FALSE)
  melted[, text := as.character(text)]
  if (drop_empty_text) {
    melted <- melted[!is.na(text) & nzchar(trimws(text))]
  }
  list(data = melted, skipped = FALSE)
}

#' @keywords internal
.loan_measure_vars <- function() {
  c(
    "definitions",
    "commitments",
    "interest_and_fees",
    "conditions_precedent",
    "representations",
    "covenants",
    "guarantees_and_security",
    "events_of_default",
    "administrative_agents",
    "miscellaneous"
  )
}

#' @keywords internal
.infer_json_family <- function(dt, what) {
  if (!is.null(what)) {
    return(what)
  }
  nms <- names(dt)
  loan_vars <- .loan_measure_vars()
  if (any(loan_vars %in% nms)) {
    return("loan")
  }
  if (any(grepl("^part_[0-9]+(?:$|_item_)", nms))) {
    return("10-Q")
  }
  if ("filing_type" %in% nms && any(dt$filing_type %in% "10-Q", na.rm = TRUE)) {
    return("10-Q")
  }
  if ("filing_type" %in% nms && any(dt$filing_type %in% "8-K", na.rm = TRUE)) {
    return("8-K")
  }
  if (any(grepl("^item_", nms))) {
    if (any(grepl("^item_[0-9]+\\.", nms))) {
      return("8-K")
    }
    return("10-K")
  }
  "10-K"
}

#' @keywords internal
.measure_vars_for_what <- function(nms, what) {
  if (what == "10-K") {
    return(grep("^(item_|section_)", nms, value = TRUE))
  }
  if (what == "10-Q") {
    return(grep("^part_[0-9]+(?:$|_item_)", nms, value = TRUE))
  }
  if (what == "8-K") {
    return(grep("^item_", nms, value = TRUE))
  }
  if (what == "loan") {
    return(intersect(.loan_measure_vars(), nms))
  }
  character()
}

#' @keywords internal
.normalize_json_dt <- function(dt, what) {
  dt <- data.table::copy(dt)
  if (what == "loan") {
    rename_pairs <- c(
      coname = "company",
      form = "filing_type",
      type = "exhibit_type"
    )
    for (old_name in names(rename_pairs)) {
      new_name <- rename_pairs[[old_name]]
      if (old_name %in% names(dt) && !(new_name %in% names(dt))) {
        data.table::setnames(dt, old = old_name, new = new_name)
      }
    }
  }
  dt
}

#' @keywords internal
.postprocess_json_dt <- function(out, drop_late_filers) {
  if (!nrow(out)) {
    return(out)
  }
  if ("cik" %in% names(out)) {
    out[, cik := suppressWarnings(as.integer(cik))]
  }
  if ("sic" %in% names(out)) {
    out[grepl("\\D", sic), sic := NA_character_]
    out[, sic := suppressWarnings(as.integer(sic))]
  }
  for (date_col in intersect(c("filing_date", "period_of_report"), names(out))) {
    out[, (date_col) := data.table::as.IDate(get(date_col))]
  }
  if (!"fyear" %in% names(out)) {
    if ("period_of_report" %in% names(out)) {
      out[, fyear := data.table::year(period_of_report)]
    } else if ("filing_date" %in% names(out)) {
      out[, fyear := data.table::year(filing_date)]
    }
  }
  if (drop_late_filers &&
      all(c("filing_date", "fyear") %in% names(out))) {
    out[, year_filed := data.table::year(filing_date)]
    if ("period_of_report" %in% names(out)) {
      out <- out[is.na(period_of_report) | is.na(year_filed) | is.na(fyear) | year_filed <= fyear + 1L]
    }
    out[, year_filed := NULL]
  }
  preferred <- c(
    "cik", "company", "filing_type", "exhibit_type", "filing_date",
    "period_of_report", "fyear", "sic", "item", "text"
  )
  present <- intersect(preferred, names(out))
  remainder <- setdiff(names(out), present)
  data.table::setcolorder(out, c(present, remainder))
  out
}
