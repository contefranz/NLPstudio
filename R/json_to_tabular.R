if ( getRversion() >= "2.15.1" ) {
  utils::globalVariables( c( "file_test", "ifile", "id") )
}
#' Convert JSON Instances to a Tabular Format
#'
#' Converts JSON files into a structured [data.table], handling both single files and entire 
#' folders. It reads raw JSON, cleans malformed values, and extracts metadata and text content. 
#' When processing a folder, it leverages parallel computing via **[foreach]** to efficiently 
#' handle multiple files, ensuring fast and reliable conversion.
#'
#' @param x Path to JSON file.
#' @param ncores The number of cores to assign to [makeCluster]. Default to 1.
#' @param ... Additional arguments passed to [list.files]. This allows customization of file 
#' selection when `x` is a folder, enabling options such as `recursive = TRUE` to search 
#' sub-directories or `pattern = "\\.json$"` to filter specific file types.
#' 
#' @returns A single `data.table` containing
#' several identification columns in addition to the document itself.
#'
#' @import data.table foreach
#' @importFrom stringr str_replace_all
#' @importFrom jsonlite fromJSON
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom iterators iter
#' @importFrom cli cli_h2 cli_alert_info cli_alert_success
#' @export

json_to_tabular <- function(x, ncores = 1, ...) {
  
  cli_h2("Processing JSON instances")
  
  if (file_test("-d", x)){
    cli_alert_info("{x} is a folder")
  } else {
    cli_alert_info("{x} is a file. Converting it directly")
    out = json_to_tabular_int(x = x)
    return(out)
  }
  
  files_list = list.files(x, full.names = TRUE, ...)
  cli_alert_info("Detected {length(files_list)} JSON files. Working on them...")
  n_files = length(files_list)
  it = iter(seq_len(n_files), by = "row")
  cl = makeCluster(ncores)
  registerDoParallel(cl)
  temp = foreach(
    ifile = it,
    .packages = c("iterators", "data.table", "stringr", "jsonlite"),
    .export = "json_to_tabular_int"
  ) %dopar% {
    current_file = files_list[ifile]
    json_to_tabular_int(x = current_file)
  }
  stopCluster(cl)
  
  out = rbindlist(temp, fill = TRUE)
  ############################################################
  # TO-DO: 
  # add a message in case observations are dropped
  # maybe print cik and fdate or just the doc_id directly???
  # TO-DO:
  ############################################################
  setorder(out, cik, fdate)
  out = unique(out, by = "doc_id")
  
  cli_alert_success("JSON conversion successful!")
  return(out)
}


#' @keywords internal
json_to_tabular_int <- function(x) {
  
  # Read the raw text
  json_text <- readLines(x, warn = FALSE)
  json_text <- paste(json_text, collapse = "\n")
  
  # Clean problematic values
  json_text <- str_replace_all(json_text, "-000\\d+", "0")     # Fix invalid negative zeros
  json_text <- str_replace_all(json_text, "\\bNaN\\b", "null")  # Replace NaN with null
  
  # Parse the cleaned JSON
  tryCatch({
    data <- fromJSON(json_text, simplifyVector = FALSE)
    out = as.data.table(data$metadata)
    out[ , text := data$cleaned_content]
    setnames(out, "_id", "doc_id")
    out[ , id := NULL]
    return(out)
  }, error = function(e) {
    stop("Failed to parse JSON: ", e$message)
  })
}
