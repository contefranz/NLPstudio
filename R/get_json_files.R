#' JSON Files Container
#'
#' Create a container list with the pointers to all the JSON files detected in the input folder.
#'
#' @param root_path Character giving the path to the root folder location (see 'Details').
#' @param pattern An optional \link[base]{regex}. Only file names which match the regular expression will be returned
#' as in \code{\link[base]{list.files}}.
#' @param fyear Optional numeric vector specifying the time window in fiscal years. When \code{NULL},
#' all the fiscal years detected in \code{root_path} are considered. Default to \code{NULL}.
#'
#' @details
#' The function works in conjunction with the EDGAR crawler structure. In general, this creates three
#' main folders as containers: (i) the original HTML files, (ii) the metadata of each filing, (iii)
#' the extracted items in JSON format. Each of these structures is usually, and conveniently, organized
#' by fiscal year. This is not an imposition but rather a strong suggestion. Ideally, one could put
#' all the filings into the root_path
#'
#' @return A list whose elements are the fiscal years. Each element contains the pointers to each JSON file within
#' each fiscal year.
#'
#' @importFrom stringr str_c str_locate str_replace
#'
#' @export

get_json_files = function(root_path, pattern, fyear = NULL) {

  if (!is.null(fyear)) {
    # if fyear specifies a followup, modifies the search pattern
    pattern = str_c(pattern, ".*", fyear, collapse = "|")
  }
  thedirs = list.files(root_path, pattern = pattern,
                       recursive = FALSE,
                       include.dirs = TRUE,
                       full.names = TRUE)
  ndirs = length(thedirs)
  thedirs_list = vector("list", ndirs)
  # attach a name given by the basename of each directory path
  names(thedirs_list) = basename(thedirs)

  # for each of the detected directory, extract the pointers to each JSON file.
  # then, return the container as a named list.
  for ( i_year in seq_along(thedirs_list) ) {
    message("Processing year ", fyear[i_year])
    current_path = thedirs[i_year]
    current_files = list.files(current_path,
                               recursive = FALSE,
                               full.names = TRUE)
    thedirs_list[[ i_year ]] = current_files
  }
  return(thedirs_list)
}
