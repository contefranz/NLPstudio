if ( getRversion() >= "2.15.1" ) {
  utils::globalVariables( c("fyear_end", "sic", "filing_html", "filing_detail", "filing_txt") )
}
#' SEC Master Files Container
#'
#' Create a container list with the pointers to all the JSON files detected in the input folder.
#'
#' @param root_path Character giving the path to the root folder location (see 'Details').
#' @param pattern An optional [base::regex]. Only file names which match the regular expression 
#' will be returned as in [list.files]. Default to `NULL`.
#' @param fyear Optional numeric vector specifying the time window in fiscal years. When `NULL`,
#' all the fiscal years detected in `root_path` are considered. Default to `NULL`.
#' @param drop_late When `TRUE`, remove filings referring to distant fiscal period. Default to
#' `FALSE`.
#'
#' @details
#' The function works in conjunction with the EDGAR crawler structure. In general, this creates three
#' main folders as containers: (i) the original HTML files, (ii) the metadata of each filing, (iii)
#' the extracted items in JSON format. Each of these structures is usually, and conveniently, organized
#' by fiscal year. This is not an imposition but rather a strong suggestion. Ideally, one could put
#' all the filings into the root_path and work from that.
#'
#' This function works on the metadata files, also called SEC master files. Typically, such data are
#' stored in a .csv file and are located in `root_path` with no sub-directory. It is recommended
#' to specify at least a `pattern` to detect those files.
#'
#' @returns A `data.table` containing all the SEC master files as detected by the function
#' with the following columns:
#'
#' \item{`cik`}{The Central Index Key as given by the SEC EDGAR database. Integer.}
#' \item{`cname`}{The company name. Character.}
#' \item{`type`}{The filing type. Character.}
#' \item{`date_filed`}{The filing date on the SEC. IDat.}
#' \item{`fyear_end`}{The fiscal year end date. IDat.}
#' \item{`sic`}{The four-digit SIC industry code. Integer.}
#' \item{`state_of_inc`}{U.S. state of incorporation (abbreviation). Character.}
#' \item{`state_location`}{U.S. state location (abbreviation). Character.}
#' \item{`filing_detail`}{Pointer to filing summary page in HTML. Character.}
#' \item{`filing_html`}{Pointer to filing in HTML. Character.}
#' \item{`filing_txt`}{Pointer to complete submission file in txt. Character.}
#' \item{`filename`}{Filename. Character.}
#'
#' @author Francesco Grossetti \email{francesco.grossetti@@unibocconi.it}
#'
#' @import data.table
#'
#' @export


get_sec_master_files = function(root_path, pattern = NULL, fyear = NULL, drop_late = FALSE) {

  # remove the trailing "/" at the end of the path to avoid double slashes in the call of list.files
  root_path = stringr::str_remove(root_path, "/$")

  # if fyear specifies a followup, modifies the search pattern
  if (!is.null(fyear)) {
    if (!is.null(pattern)) {
      pattern = stringr::str_c(pattern, ".*", fyear, collapse = "|")
    } else {
      pattern = stringr::str_c(fyear, collapse = "|")
    }
  }
  thefiles = list.files(root_path, pattern = pattern,
                        recursive = FALSE,
                        full.names = TRUE)
  master_all = data.table::rbindlist(lapply(thefiles, function(f) {
    message("Reading file ", basename(f))
    data.table::fread(f)
  }))

  # check that only one filing type exists. If the check fails, stop with an error as the selection
  # of the filing type is inherited from the edgar-crawler and it is not controlled here in R nor
  # by edgartools
  if( data.table::uniqueN(master_all$Type) > 1 ) {
    stop("Multiple filing types detected")
  }

  # fixing column names to match those from the JSON structures as modified by from_json_to_df()
  # 1. replace space with "_"
  data.table::setnames(master_all, new = stringr::str_replace_all(names(master_all), "\\s", "_"))
  # 2. lowercase everything
  data.table::setnames(master_all, new = stringr::str_to_lower(names(master_all)))
  # 3. renaming
  data.table::setnames(master_all,
           c("company", "complete_text_file_link", "html_index", "filing_date", "htm_file_link", "period_of_report"),
           c("cname", "filing_txt", "filing_detail", "date_filed", "filing_html", "fyear_end") )
  # 4. remove useless columns
  master_all[ , `:=` (date = NULL, fiscal_year_end = NULL)]
  # 5. define a column order that resembles that of the JSON structure
  data.table::setcolorder(master_all, c("cik", "cname", "type", "date_filed", "fyear_end", "sic", "state_of_inc",
                            "state_location", "filing_detail", "filing_html", "filing_txt", "filename"))

  if (drop_late) {
    # SUPER IMPORTANT!
    # the following code fixes late filers. It can happen that multiple filings (e.g., a 10-K) are
    # filed on the same date_filed but they refer to different fyear_end. We only want the original
    # filing that is associated to the current fiscal period. To solve the problem we sort
    # by cik, date_filed, fyear_end so that the most current filing is the last observation within the
    # group. We use duplicated() to spot duplicated observations in reverse order and then drop them.
    data.table::setkey(master_all, cik, date_filed, fyear_end, sic, filing_detail, filing_html, filing_txt)
    master_all[ , checkdup := duplicated(master_all, by = c("cik", "date_filed"), fromLast = TRUE)]

    master_all = master_all[ checkdup == FALSE ]
  }

  return(master_all[])

}
