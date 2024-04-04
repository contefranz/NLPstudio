#' List JSON files within the tree structure
#'
#' Convenient function to list all the JSON files contained in a given folder.
#'
#' @param path_in Character giving the path to folder location.
#' @param fyear Optional numeric vector specifying the time window in fiscal years. Default to \code{NULL}.
#'
#' @return A list whose elements are the fiscal years. Each element contains the pointers to each JSON file within
#' each fiscal year.
#'
#' @importFrom stringr str_c str_locate str_replace
#'
#' @export

get_json_files = function(path_in, fyear = NULL) {

  if (is.null(fyear)) {
    thedirs = list.dirs(path_in, recursive = FALSE)
    fyear = as.integer(thedirs)
  } else {
    fyear_regex = str_c(fyear, collapse = "|")
    thedirs = list.files(path_in, recursive = FALSE, include.dirs = TRUE, pattern = fyear_regex)
  }
  ndirs = length(thedirs)
  thedirs_list = vector("list", ndirs)
  names(thedirs_list) = thedirs

  for ( i_year in seq_along(thedirs_list) ) {
    message("Processing year ", fyear[i_year])
    current_path = file.path(path_in, fyear[i_year])
    current_path2 = file.path(path_in, fyear[i_year])
    find_double_slashes = str_locate(current_path, "//")
    if ( any(!is.na(find_double_slashes)) ) {
      current_path = str_replace(current_path, pattern = "//", replacement = "/")
    }
    current_files = list.files(current_path,
                               recursive = FALSE,
                               full.names = TRUE)
    thedirs_list[[ i_year ]] = current_files
  }
  return(thedirs_list)
}
