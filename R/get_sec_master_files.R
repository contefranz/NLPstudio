if ( getRversion() >= "2.15.1" ) {
  utils::globalVariables( c("fyear_end", "sic", "filing_html") )
}
#' SEC Master Files Container
#'
#' Create a container list with the pointers to all the JSON files detected in the input folder.
#'
#' @param root_path Character giving the path to the root folder location (see 'Details').
#' @param pattern An optional \link[base]{regex}. Only file names which match the regular expression will be returned
#' as in \code{\link[base]{list.files}}. Default to \code{NULL}.
#' @param fyear Optional numeric vector specifying the time window in fiscal years. When \code{NULL},
#' all the fiscal years detected in \code{root_path} are considered. Default to \code{NULL}.
#' @param drop_late When \code{TRUE}, remove filings referring to distant fiscal period. Default to
#' \code{FALSE}.
#'
#' @details
#' The function works in conjunction with the EDGAR crawler structure. In general, this creates three
#' main folders as containers: (i) the original HTML files, (ii) the metadata of each filing, (iii)
#' the extracted items in JSON format. Each of these structures is usually, and conveniently, organized
#' by fiscal year. This is not an imposition but rather a strong suggestion. Ideally, one could put
#' all the filings into the root_path and work from that.
#'
#' This function works on the metadata files, also called SEC master files. Typically, such data are
#' stored in a .csv file and are located in \code{root_path} with no sub-directory. It is recommended
#' to specify at least a \code{pattern} to detect those files.
#'
#' @return A \code{data.table} containing all the SEC master files as detected by the function
#' with the following columns:
#'
#' \item{\code{cik}}{The Central Index Key as given by the SEC EDGAR database. Integer.}
#' \item{\code{cname}}{The company name. Character.}
#' \item{\code{type}}{The filing type. Character.}
#' \item{\code{date_filed}}{The filing date on the SEC. IDat.}
#' \item{\code{fyear_end}}{The fiscal year end date. IDat.}
#' \item{\code{sic}}{The four-digit SIC industry code. Integer.}
#' \item{\code{state_of_inc}}{U.S. state of incorporation (abbreviation). Character.}
#' \item{\code{state_location}}{U.S. state location (abbreviation). Character.}
#' \item{\code{filing_detail}}{Pointer to filing summary page in HTML. Character.}
#' \item{\code{filing_html}}{Pointer to filing in HTML. Character.}
#' \item{\code{filing_txt}}{Pointer to complete submission file in txt. Character.}
#' \item{\code{filename}}{Filename. Character.}
#'
#' @author Francesco Grossetti \email{francesco.grossetti@@unibocconi.it}
#'
#' @import data.table
#' @importFrom stringr str_remove str_c str_replace_all str_to_lower
#'
#' @export


get_sec_master_files = function(root_path, pattern = NULL, fyear = NULL, drop_late = FALSE) {

  # remove the trailing "/" at the end of the path to avoid double slashes in the call of list.files
  root_path = str_remove(root_path, "/$")

  # if fyear specifies a followup, modifies the search pattern
  if (!is.null(fyear)) {
    if (!is.null(pattern)) {
      pattern = str_c(pattern, ".*", fyear, collapse = "|")
    } else {
      pattern = str_c(fyear, collapse = "|")
    }
  }
  thefiles = list.files(root_path, pattern = pattern,
                        recursive = FALSE,
                        full.names = TRUE)
  n_files = length(thefiles)
  master_collector = vector("list", n_files)

  for ( i_file in seq_len(n_files) ) {
    current_pointer = basename(thefiles[i_file])
    message("Reading file ", current_pointer)
    current = fread(thefiles[i_file])
    master_collector[[ i_file ]] = current
  }

  master_all = rbindlist(master_collector)

  # check that only one filing type exists. If the check fails, stop with an error as the selection
  # of the filing type is inherited from the edgar-crawler and it is not controlled here in R nor
  # by edgartools
  if( uniqueN(master_collector$Type) > 1 ) {
    stop("Multiple filing types detected")
  }

  # fixing column names to match those from the JSON structures as modified by from_json_to_df()
  # 1. replace space with "_"
  setnames(master_all, new = str_replace_all(names(master_all), "\\s", "_"))
  # 2. lowercase everything
  setnames(master_all, new = str_to_lower(names(master_all)))
  # 3. renaming
  setnames(master_all,
           c("company", "complete_text_file_link", "html_index", "filing_date", "htm_file_link", "period_of_report"),
           c("cname", "filing_txt", "filing_detail", "date_filed", "filing_html", "fyear_end") )
  # 4. remove useless columns
  master_all[ , `:=` (date = NULL, fiscal_year_end = NULL)]
  # 5. define a column order that resembles that of the JSON structure
  setcolorder(master_all, c("cik", "cname", "type", "date_filed", "fyear_end", "sic", "state_of_inc",
                            "state_location", "filing_detail", "filing_html", "filing_txt", "filename"))

  if (drop_late) {
    # SUPER IMPORTANT!
    # the following code fixes late filers. It can happen that multiple filings (e.g., a 10-K) are
    # filed on the same date_filed but they refer to different fyear_end. We only want the original
    # filing that is associated to the current fiscal period. To solve the problem we sort
    # by cik, date_filed, fyear_end so that the most current filing is the last observation within the
    # group. We use duplicated() to spot duplicated observations in reverse order and then drop them.
    setkey(master_all, cik, date_filed, fyear_end, sic, filing_detail, filing_html, filing_txt)
    master_all[ , checkdup := duplicated(master_all, by = c("cik", "date_filed"), fromLast = TRUE)]

    master_all = master_all[ checkdup == FALSE ]
  }

  return(master_all[])

}
