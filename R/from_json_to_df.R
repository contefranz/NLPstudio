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
#' @param ... Additional arguments for chunking. 
#'   \describe{
#'     \item{`max_chunk_size`}{Integer. If provided, sets the exact number of
#'       files per chunk, overriding the value derived from `nchunks`. Use this
#'       for fine-grained memory control when file sizes vary significantly.}
#'   }

#' @details
#' Internally the function proceeds in three phases:
#'
#' 1. **Read & parse** – The input file paths are divided into chunks. By
#'    default, chunk size is derived from `nchunks` as
#'    `ceiling(length(files) / nchunks)`. If `max_chunk_size` is explicitly
#'    provided, it overrides this calculation. Each chunk is read in parallel
#'    with [RcppSimdJson::fload()] and converted to data tables.
#'
#' 2. **Reshape** – Each parsed table is reshaped from wide to long format in
#'   parallel. Identifier variables for [data.table::melt()] are determined
#'   automatically: any columns beginning with `"item_"` or `"section_"`
#'   are treated as measure variables, and all others are treated as identifier
#'   variables. If no such columns are found, all columns are treated as
#'   identifiers.
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
#' \itemize{
#'   \item **`nchunks`** (default): Set the number of batches. With 7000 files
#'     and `nchunks = 4`, each batch contains ~1750 files.
#'   \item **`max_chunk_size`** (explicit): Set the exact batch size. With 7000
#'     files and `max_chunk_size = 500`, you get 14 batches of 500 files each.
#' }
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
#' @importFrom stringr str_detect
#' @importFrom cli cli_h2 cli_alert_info cli_alert_success
#' @importFrom parallel mclapply clusterApplyLB makeCluster stopCluster
#' @export
from_json_to_df <- function(files, ncores = 1, nchunks = ncores,
                            socket = c("PSOCK", "FORK"), drop_late_filers = FALSE, ...) {
  
  if (!requireNamespace("RcppSimdJson", quietly = TRUE)) {
    stop("The 'RcppSimdJson' package is required for fast JSON parsing. Please install it.")
  }
  if (!is.character(files)) {
    stop("`files` must be a character vector of file paths")
  }
  
  cli_h2("Flattening JSON files")
  
  socket <- match.arg(socket)
  .validate_parallel_args(ncores, nchunks)
  args <- list(...)
  # If max_chunk_size is explicitly provided, use it; otherwise derive from nchunks
  if (!is.null(args$max_chunk_size)) {
    chunk_size <- args$max_chunk_size
  } else {
    chunk_size <- ceiling(length(files) / nchunks)
  }
  
  # Phase 1: Read
  cli_alert_info("Reading JSON files with RcppSimdJson")
  temp <- .chunked_read_json(files, ncores, chunk_size = chunk_size, socket = socket)
  
  # Phase 2: Reshape
  cli_alert_info("Reshaping JSON data in parallel with {ncores} cores")
  df_melt <- .parallel_melt(temp, ncores, socket = socket)
  out <- rbindlist(df_melt, fill = TRUE)
  
  # Phase 3: Post-process
  out[, cik := as.integer(cik)]
  out[grepl("\\D", sic), sic := NA_character_]
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
.chunked_read_json <- function(files, ncores, chunk_size, socket) {
  chunks <- split(files, ceiling(seq_along(files) / chunk_size))
  out_list <- vector("list", length(chunks))
  
  for (i in seq_along(chunks)) {
    cli_alert_info("Processing chunk {i}/{length(chunks)} with {length(chunks[[i]])} files")
    temp <- .parallel_read_json(chunks[[i]], ncores, socket = socket)
    out_list[[i]] <- data.table::rbindlist(temp, fill = TRUE)
    rm(temp); gc(verbose = FALSE)
  }
  out_list
}

#' @keywords internal
.parallel_read_json <- function(files, ncores, socket) {
  .read_json_file <- function(f) {
    fload_fun <- getExportedValue("RcppSimdJson", "fload")
    dat <- fload_fun(f)
    data.table::as.data.table(dat)
  }
  .run_parallel(files, .read_json_file, ncores, socket,
                export_vars = c(".read_json_file"),
                export_env = environment())
}

#' @keywords internal
.parallel_melt <- function(temp, ncores, socket) {
  .melt_chunk <- function(jcol) {
    dt <- temp[[jcol]]
    meas <- grep("^(item_|section_)", names(dt), value = TRUE)
    if (length(meas) == 0L) {
      return(data.table::data.table())
    }
    idv <- setdiff(names(dt), meas)
    data.table::melt(
      dt,
      id.vars       = idv,
      measure.vars  = meas,
      variable.name = "item",
      value.name    = "text",
      variable.factor = FALSE
    )
  }
  .run_parallel(seq_along(temp), .melt_chunk, ncores, socket,
                export_vars = c(".melt_chunk", "temp"),
                export_env = environment())
}

