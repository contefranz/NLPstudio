if ( getRversion() >= "2.15.1" ) {
  utils::globalVariables( c("cik", "filing_detail", "filing_txt", "temp", "filename",
                            "doc_id_corpus", "checkdup", "filename2") )
}
#' Generate a Quanteda Corpus from Text Data
#'
#' Convert structured text data into a quanteda [corpus], supporting a variety of formats with parallel processing via **[future]**.
#'
#' @param x Input object, either a character indicating a path to stored data or a data.table as created by [from_json_to_df].
#' @param ncores The number of [multisession] workers to be allocated for 
#' the lookup. Default to 1.
#' @param ... Additional arguments passed to [readtext].
#' 
#' @details
#' If `x` points directly to a specific file containing the textual data, this is treated as a single-item
#' list. If `x` is a folder, `define_corpus` looks for the following formats: .txt, .csv, .json, or .xml. This is in compliance with [readtext].
#' 
#' When passing either a single item or multiple small items, setting `ncores > 1` does not necessarily show a tangible improvement in speed. This is why by default `ncores = 1`.
#' 
#' 
#' 
#' @return A quanteda [corpus] object.
#' 
#' @importFrom readtext readtext
#' @importFrom quanteda corpus
#' @importFrom future plan multisession sequential
#' @importFrom future.apply future_lapply
#' @importFrom cli cli_h2 cli_h3 cli_alert_info cli_alert cli_alert_success cli_alert_danger

#' @export
define_corpus <- function(x, ...) {
  UseMethod("define_corpus")
}


# CHARACTER CLASS - READING FROM PATH VIA READTEXT --------------------------------------------


#' @rdname define_corpus
#' @export
define_corpus.character <- function(x, ncores = 1, ...) {
  if (!inherits(x, "character")) {
    stop("x must be a character")
  }
  
  args = list(...)
  # exclude the dictionary as argument as it prints out all the tokens in it
  # Just report anything else by redefining args
  # args = args[!sapply(args, is.dictionary)]
  if ( length(args) < 1 ) {
    cli_alert_info("readtext::readtext() has been called with the default parameters")
  } else {
    cli_alert_info("readtext::readtext() has been called with the following parameters")
    args_active = paste0(names(args), " = ", unlist(args))
    for (iarg in seq_along(args_active) ) {
      cli_alert("{args_active[iarg]}")
    }
  }
  
  # define the number of workers
  plan(multisession, workers = ncores)
  
  
  # If input is a directory, list all text-based files
  if (dir.exists(x)) {
    files <- list.files(x, full.names = TRUE, recursive = TRUE,
                        pattern = "\\.(txt|csv|json|xml)$")  # Add more extensions if needed
  } else {
    files <- x  # If a single file is passed, treat it as a single-item list
  }
  
  # Split the file list into chunks for parallel processing
  file_chunks <- split(files, rep_len(1L:ncores, length(files)))
  
  cli_alert_info(paste("Reading text data using", ncores, "cores"))
  
  # Read files in parallel
  read_objects = future_lapply(file_chunks, function(chunk) {
    readtext::readtext(chunk, ...)
  }, future.seed = TRUE)
  
  # Combine results into a single readtext object
  combined_readtext = do.call(rbind, read_objects)
  
  plan(sequential)  # Reset the plan
  
  cli_h2("Building corpus from readtext object")
  on.exit(cli_alert_success("Corpus created!"))
  return(quanteda::corpus(combined_readtext))
  
}

# DATA.TABLE BASED ON JSON CONTAINER ----------------------------------------------------------


#' @rdname define_corpus
#' @export
define_corpus.data.table <- function(x, ...) {
  if (!inherits(x, "data.table")) {
    stop("x must be a data.table containing a 'text' and 'filename' column")
  }
  required_cols <- c("text", "filename")
  if (!all(required_cols %in% names(x))) {
    stop("data.table must contain columns: text, filename")
  }
  
  x[, filename2 := str_remove(filename, "\\.htm|\\.txt")]
  x[, doc_id_corpus := str_c(filename2, item, sep = "_")]
  
  # Check for duplicate doc IDs
  x[, checkdup := duplicated(x, by = "doc_id_corpus")]
  if (nrow(x[checkdup == TRUE]) > 0) {
    warning("Non-unique doc_id. Check corpus docvar 'filename'")
  }
  x[, checkdup := NULL]
  
  cli_h2("Building corpus from data.table")
  corpus_obj <- corpus(x, text_field = "text", docid_field = "doc_id_corpus")
  
  x[, `:=` (filename2 = NULL, doc_id_corpus = NULL)]
  return(corpus_obj)
}


