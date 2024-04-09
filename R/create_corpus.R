if ( getRversion() >= "2.15.1" ) {
  utils::globalVariables( c("cik", "filing_detail", "filing_txt", "temp", "filename",
                            "doc_id_corpus", "checkdup") )
}
#' Build a SEC EDGAR Corpus
#'
#' Build a quanteda corpus from the data.table container.
#'
#' @param df_container A list of data.table containing the raw textual documents
#' as built by \code{\link{from_json_to_df}}.
#' @param form_type Character specifying the SEC form type that is contained in \code{df_container}.
#' Default to \code{10-K}.
#' @param to_disk Logical declaring whether to save the corpora to disk or not (see 'Details').
#' Default is \code{FALSE}.
#' @param path_out Character specifying the directory where to save the corpora by fiscal year.
#'
#' @details
#' When \code{to_disk = FALSE}, the function returns a, potentially, much larger object in
#' memory in the class \code{\link[quanteda]{corpus}}. In this case, be mindful of RAM consumption.
#' If \code{to_disk = TRUE}, the function returns \code{NULL} and saves to disk
#' the objects at the location specified by \code{path_out}. The execution time will eventually be
#' longer due to the usual I/O hiccups.
#'
#' @return Either \code{NULL} or a quanteda corpus object.
#'
#' @author Francesco Grossetti \email{francesco.grossetti@@unibocconi.it}
#'
#' @import data.table quanteda
#' @importFrom stringr str_remove str_extract str_detect str_c
#' @export

create_corpus = function(df_container, to_disk = FALSE, path_out = NULL) {

  # this function takes as input the output of from_json_to_df()
  # form_type = match.arg(form_type)
  # form_type = str_remove(form_type, "\\-")
  #
  # for ( i_year in seq_along(df_container) ) {
  #   current_year = str_extract(names(df_container)[i_year], "\\d+")
  #   message("Processing ", current_year)
  #   # list element --> fiscal year
  #   current_df = df_container[[i_year]]

  # this is important for quanteda: create unique_identifiers for the document
  # in addition, add a proper filename which contains the accession number from SEC EDGAR
  # both requires some wrangling so that's why the code below contains some attempts
  df_container[ , filename2 := str_remove(filename, "\\.htm|\\.txt")]
  df_container[ , doc_id_corpus := str_c(filename2, item, sep = "_")]
  # filename_df = df_container[ , .(cik,
  #                               fyear,
  #                               item,
  #                               fili,
  #                               filing_txt) ]
  # filename_df[ , form_type := form_type]
  # filename_df[ , temp := str_extract(filing_detail, "(?<=\\d\\/).*?$")]
  # filename_df[ , temp := str_remove(temp, "\\-index\\.html|\\.txt")]
  # filename_df[ str_detect(temp, "\\/"), filename := filing_txt]
  # filename_df[ str_detect(temp, "\\/"), filename := str_remove(filename, "\\..*?$")]
  # filename_df[ is.na(filename), filename := str_c(cik, "_", form_type, "_", fyear, "_", temp)]
  # filename_df[ is.na(filename), filename := str_remove(filing_detail, "\\.htm")]
  # filename_df[ , doc_id_corpus := str_c(filename, "_", item)]

  # since a quanteda corpus requires unique docnames, we check whether there are duplicated
  # observations at the level of the internal "doc_id_corpus" variable.
  # if the check fails, the function raises a warning
  df_container[ , checkdup := duplicated(df_container, by = "doc_id_corpus")]
  check_not_unique_filenames = nrow(df_container[checkdup == TRUE])
  if ( check_not_unique_filenames > 0 ) {
    warning("Non unique doc_id. Check corpus docvar \"filename\"")
  }
  df_container[ , checkdup := NULL]
  # add the filename (with accession number) and internal docname for corpus to the data.table
  # current_df[ , `:=` (filename = filename_df[ , filename],
  #                     doc_id_corpus = filename_df[ , doc_id_corpus])]

  message("Building corpus")
  current_corpus = corpus(x = df_container, text_field = "text", docid_field = "doc_id_corpus")
  df_container[ , `:=` (filename2 = NULL, doc_id_corpus = NULL)]
  return(current_corpus)

  # if ( to_disk ) {
  #   if (is.null(path_out)) {
  #     stop("Argument path_out is required")
  #   }
  #   message("Saving corpus for ", current_year)
  #   if (!dir.exists(path_out)) {
  #     dir.create(path_out)
  #   }
  #   fileout = str_c(current_year, "corpus.rds", sep = "_")
  #   saveRDS(current_corpus, file.path(path_out, fileout))
  # } else {
  #   if (i_year == 1L) {
  #     # define the big corpus
  #     out_corpus = current_corpus
  #   } else {
  #     out_corpus = out_corpus + current_corpus
  #   }
  # }
  # }
  # if ( to_disk ) {
  #   return(NULL)
  # } else {
  #   return(out_corpus)
  # }
}
