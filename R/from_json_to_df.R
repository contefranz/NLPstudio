if ( getRversion() >= "2.15.1" ) {
  utils::globalVariables( c( "jcol", "item", "fyear", ".", "filing_date",
                             "period_of_report", "accession_number",
                             "text", "year_filed", "date_filed", "fyear_end", "sic") )
}
#' Convert JSON to data.table
#'
#' Converts a vector of JSON file paths into a unified [data.table] suitable for downstream analysis.
#' The function is optimized for large-scale input (thousands of JSON files)
#' and leverages both chunking and OS-specific parallelization strategies
#' to remain efficient and memory-safe.
#' @param files Character vector of JSON file paths.
#' @param ncores Integer. Number of workers to use in parallel sections.
#'   Default is 1 (sequential).
#' @param chunk_size Integer. Maximum number of files to read in a single
#'   chunk. Default is 500. Lower values reduce peak memory usage at the cost
#'   of more iteration overhead.
#' @param drop_late_filers Logical. If `TRUE`, removes filings considered
#'   "late" (filing year greater than fiscal year + 1). Default `FALSE`.
#'   
#' @details
#' Internally the function proceeds in three phases. First, the vector of file
#' paths is split into chunks of size `chunk_size` to prevent loading thousands
#' of large JSON files at once. Each chunk is read in parallel using
#' [RcppSimdJson][RcppSimdJson::RcppSimdJson] for fast parsing, and immediately converted into data tables.
#' 
#' Second, each table is reshaped from wide to long format, again in parallel.
#' The identifier variables passed to [data.table::melt()] are determined
#' automatically: all columns before the first marker column named either
#' `"item_1"` or `"section_1"` are treated as identifiers, and if no marker
#' is present then all columns are retained as identifiers. Third, the melted
#' tables are combined and cleaned.
#' 
#' Finally, column order is adjusted for convenience and the unified
#' data table is returned. This design avoids excessive memory use, exploits
#' OS-specific parallelism ([mclapply()] on Linux and macOS, [parLapply()] on
#' Windows), and ensures consistent reshaping even when the structure of
#' individual JSON files varies.
#' 
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
#'
#' @section Efficiency considerations:
#' - **RcppSimdJson** is used for parsing, which is much faster than
#'   `jsonlite` parsers for large files.  
#' - Chunking + parallelization means memory scales linearly with
#'   `chunk_size`, not total number of files.  
#' - On Linux/macOS, [mclapply()] avoids copying the entire list of JSONs to
#'   each worker (shared memory via forking).  
#' - On Windows, [parLapply()] is used with explicit cluster export; this is
#'   slightly slower but avoids serialization issues seen with [future][future::future].  
#'
#' @seealso [RcppSimdJson::fload()], [data.table::melt()],
#'   [parallel::mclapply()], [parallel::parLapply()]
#'
#' @import data.table
#' @importFrom stringr str_which str_detect
#' @importFrom cli cli_h2 cli_alert_info cli_alert_success
#' @export

from_json_to_df <- function(files, ncores = 1, chunk_size = 500, drop_late_filers = FALSE) {
  
  if (!requireNamespace("RcppSimdJson", quietly = TRUE)) {
    stop("The 'RcppSimdJson' package is required for fast JSON parsing. Please install it.")
  }
  if (!is.character(files)) {
    stop("`files` must be a character vector of file paths")
  }
  
  cli_h2("Flattening JSON files")
  # Step 1: Sequential read + parse with RcppSimdJson
  cli_alert_info("Reading JSON files sequentially with RcppSimdJson")
  temp <- .chunked_read_json(files, ncores, chunk_size = chunk_size)
  
  cli_alert_info("Reshaping JSON data in parallel with {ncores} workers")
  df_melt <- .parallel_melt(temp, ncores)
  out <- rbindlist(df_melt, fill = TRUE)
  
  # Post-processing (same as before)
  out[, cik := as.integer(cik)]
  out[stringr::str_detect(sic, "\\D"), sic := NA_character_]
  out[, sic := as.integer(sic)]
  out[, `:=` (
    filing_date = as.IDate(filing_date),
    period_of_report = as.IDate(period_of_report)
  )]
  out[, fyear := year(period_of_report)]
  
  if (drop_late_filers) {
    out[, year_filed := year(filing_date)]
    out <- out[year_filed <= fyear + 1L]
    out[, year_filed := NULL]
  }
  
  cli_alert_success("Conversion has been successful")
  setcolorder(out, "fyear", after = "period_of_report")
  setcolorder(out, "item", after = "filing_type")
  return(out[])
}


#' @keywords internal
.chunked_read_json <- function(files, ncores, chunk_size) {
  chunks <- split(files, ceiling(seq_along(files) / chunk_size))
  
  out_list <- vector("list", length(chunks))
  
  for (i in seq_along(chunks)) {
    cli_alert_info("Processing chunk {i}/{length(chunks)} with {length(chunks[[i]])} files")
    
    temp <- .parallel_read_json(chunks[[i]], ncores)
    
    # bind as we go
    out_list[[i]] <- data.table::rbindlist(temp, fill = TRUE)
    
    rm(temp)
    gc(verbose = FALSE)  # force garbage collection
  }
  
  # I return the list of data.tables rather that one single object so that 
  # I can pass it to .parallel_melt via parallel:mclapply
  return(out_list)
}

#' @importFrom parallel mclapply parLapply makeCluster stopCluster
#' @keywords internal
.parallel_read_json <- function(files, ncores) {
  if (.Platform$OS.type != "windows" && requireNamespace("parallel", quietly = TRUE)) {
    # ---- Linux / macOS: forked processes ----
    res <- parallel::mclapply(
      files,
      function(f) {
        # dat <- RcppSimdJson::fload(f)
        fload_fun <- getExportedValue("RcppSimdJson", "fload")
        dat <- fload_fun(f)
        data.table::as.data.table(dat)
      },
      mc.cores = ncores
    )
  } else {
    # ---- Windows: cluster backend ----
    cl <- parallel::makeCluster(ncores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    
    res <- parallel::parLapply(
      cl,
      files,
      function(f) {
        # dat <- RcppSimdJson::fload(f)
        fload_fun <- getExportedValue("RcppSimdJson", "fload")
        dat <- fload_fun(f)
        data.table::as.data.table(dat)
      }
    )
  }
  return(res)
}

#' @importFrom parallel mclapply parLapply makeCluster stopCluster
#' @keywords internal
.parallel_melt <- function(temp, ncores) {
  if (.Platform$OS.type != "windows" && requireNamespace("parallel", quietly = TRUE)) {
    # ---- Linux / macOS ----
    res <- parallel::mclapply(
      seq_along(temp),
      function(jcol) {
        dt <- temp[[jcol]]
        id_vars <- .get_id_cols(dt)
        data.table::melt(
          dt,
          id.vars       = id_vars,
          variable.name = "item",
          value.name    = "text"
        )
      },
      mc.cores = ncores
    )
  } else {
    # ---- Windows ----
    cl <- parallel::makeCluster(ncores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterExport(cl, varlist = "temp", envir = environment())
    
    res <- parallel::parLapply(
      cl,
      seq_along(temp),
      function(jcol) {
        dt <- temp[[jcol]]
        id_vars <- .get_id_cols(dt)
        data.table::melt(
          dt,
          id.vars       = id_vars,
          variable.name = "item",
          value.name    = "text"
        )
      }
    )
  }
  return(res)
}


#' @keywords internal
.get_id_cols <- function(dt) {
  # find the first marker
  pos <- stringr::str_which(names(dt), "^(item_1|section_1)$")
  
  if (length(pos) == 0L) {
    # no marker found → use all columns
    return(names(dt))
  }
  if (pos == 1L) {
    # marker is the very first column → no id columns
    return(character(0))
  }
  # else: all columns before the marker
  return(names(dt)[seq_len(pos - 1L)])
}