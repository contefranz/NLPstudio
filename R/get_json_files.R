#' JSON Files Container
#'
#' @description
#' `r if (requireNamespace("lifecycle", quietly = TRUE)) lifecycle::badge("deprecated")`
#'
#' This function is deprecated and will be removed in a future release.
#' Please use [base::list.files()] directly instead:
#'
#' ```r
#' files <- list.files(root_path, pattern = "\\.json$", recursive = TRUE, full.names = TRUE)
#' ```
#'
#' Then pass the resulting character vector of file paths to [from_json_to_df()].
#' 
#' @param root_path Path to root folder containing JSON files.
#' @param ... Additional parameters passed to `list.files()`.
#'
#' @return A character vector of file paths. (Deprecated.)
#' @export
get_json_files = function(root_path, ...) {
  
  if (requireNamespace("lifecycle", quietly = TRUE)) {
    lifecycle::deprecate_warn(
      "0.1.3",
      "get_json_files()",
      details = "Use list.files(..., pattern = '\\\\.json$', recursive = TRUE, full.names = TRUE) instead, and pass the result to from_json_to_df()."
    )
  }  
  list.files(root_path, ...)
}