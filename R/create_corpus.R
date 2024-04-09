if ( getRversion() >= "2.15.1" ) {
  utils::globalVariables( c("cik", "filing_detail", "filing_txt", "temp", "filename",
                            "doc_id_corpus", "checkdup", "filename2") )
}
#' Build a SEC EDGAR Corpus
#'
#' Build a quanteda corpus from the data.table container.
#'
#' @param df_container A \code{data.table} containing the raw textual documents
#' as built by \code{\link{from_json_to_df}}.
#'
#' @return A quanteda \code{\link[quanteda]{corpus}} object.
#'
#' @author Francesco Grossetti \email{francesco.grossetti@@unibocconi.it}
#'
#' @import data.table quanteda
#' @importFrom stringr str_remove str_detect str_c
#' @export

create_corpus = function(df_container) {

  # this function takes as input the output of from_json_to_df()

  # this is important for quanteda: create unique_identifiers for the document
  # in addition, add a proper filename which contains the accession number from SEC EDGAR
  # both requires some wrangling so that's why the code below contains some attempts
  df_container[ , filename2 := str_remove(filename, "\\.htm|\\.txt")]
  df_container[ , doc_id_corpus := str_c(filename2, item, sep = "_")]

  # since a quanteda corpus requires unique docnames, we check whether there are duplicated
  # observations at the level of the internal "doc_id_corpus" variable.
  # if the check fails, the function raises a warning
  df_container[ , checkdup := duplicated(df_container, by = "doc_id_corpus")]
  check_not_unique_filenames = nrow(df_container[checkdup == TRUE])
  if ( check_not_unique_filenames > 0 ) {
    warning("Non unique doc_id. Check corpus docvar \"filename\"")
  }
  df_container[ , checkdup := NULL]

  message("Building corpus")
  current_corpus = corpus(x = df_container, text_field = "text", docid_field = "doc_id_corpus")
  df_container[ , `:=` (filename2 = NULL, doc_id_corpus = NULL)]
  return(current_corpus)
}
