if ( getRversion() >= "2.15.1" ) {
  utils::globalVariables( c("cik", "filename", "doc_id_corpus", "checkdup", "filename2") )
}
#' Generate a Quanteda Corpus from a Data Table
#'
#' `define_corpus()` builds a [quanteda::corpus()] from structured text data
#' contained in a [data.table::data.table], typically created by
#' [from_json_to_df()]. The method ensures that each document has a unique
#' identifier and attaches it as a document variable.
#' 
#' @param x A [data.table::data.table] with at least two columns:
#'   `text` (character vector of document texts) and `filename` (character
#'   vector of source file names). Usually this is the output of
#'   [from_json_to_df()].
#' @param ... Currently not used.
#'
#' @details
#' The function constructs a `doc_id_corpus` variable by combining the
#' `filename` (stripped of extensions `.htm` or `.txt`) with the `item`
#' column. This identifier is used as the document ID when building the
#' quanteda corpus. If duplicate IDs are detected, a warning is issued.
#'
#' After the corpus is built, temporary columns (`filename2` and
#' `doc_id_corpus`) are removed from the input table, so that only the corpus
#' object is returned.
#' 
#' Although one could call [quanteda::corpus()] directly on the output of
#' [from_json_to_df()], it is recommended to use `define_corpus()`. This
#' ensures consistent handling of document IDs, automatic duplicate checks,
#' and integration with the rest of the **NLPstudio** pipeline.
#' 
#' @return A [quanteda::corpus()] object with a set of document-level variables (i.e., `docvars`).
#'
#' @examples
#' \dontrun{
#' # Suppose you have a folder with EDGAR JSON filings
#' files <- list.files("data/json_filings", pattern = "\\.json$", full.names = TRUE)
#'
#' # Convert JSONs to a structured data.table
#' dt <- from_json_to_df(files, ncores = 2, chunk_size = 200)
#'
#' # Build a quanteda corpus with stable document IDs
#' corp <- define_corpus(dt)
#'
#' # Inspect the corpus
#' summary(corp)
#' docvars(corp)[1:5, ]
#' }
#' @seealso [corpus()], [docvars()]
#' @import data.table
#' @export
define_corpus <- function(x, ...) {
  UseMethod("define_corpus")
}


#' @rdname define_corpus
#' @method define_corpus data.table
#' @export
define_corpus.data.table <- function(x, ...) {
  if (!inherits(x, "data.table")) {
    stop("x must be a data.table containing a 'text' and 'filename' column")
  }
  required_cols <- c("text", "filename", "item")
  missing_cols  <- setdiff(required_cols, names(x))
  if (length(missing_cols) > 0L) {
    stop(paste0("data.table is missing required column(s): ",
                paste(missing_cols, collapse = ", ")))
  }
  
  x[, filename2 := stringr::str_remove(filename, "\\.htm|\\.txt")]
  x[, doc_id_corpus := stringr::str_c(filename2, item, sep = "_")]
  
  # Check for duplicate doc IDs
  x[, checkdup := duplicated(x, by = "doc_id_corpus")]
  if (nrow(x[checkdup == TRUE]) > 0) {
    warning("Non-unique doc_id. Check corpus docvar 'filename'")
  }
  x[, checkdup := NULL]
  
  cli::cli_h2("Building corpus from data.table")
  corpus_obj <- quanteda::corpus(x, text_field = "text", docid_field = "doc_id_corpus")
  x[, `:=` (filename2 = NULL, doc_id_corpus = NULL)]
  cli::cli_alert_success("Corpus built with {quanteda::ndoc(corpus_obj)} documents")
  return(corpus_obj)
}


