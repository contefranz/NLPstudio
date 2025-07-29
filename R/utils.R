# Some utility functions

#' @keywords internal
is.textstat_simil_symm = function(x) {
  "textstat_simil_symm" %in% class(x)
}

#' @keywords internal
set_theta_names <- function(theta_dt) {
  
  setnames(theta_dt, "rn", "doc_id")
  
  # Detect the topic columns (assumes doc_id is the only non-topic column)
  topic_cols <- setdiff(names(theta_dt), "doc_id")
  
  # Extract the numeric part from the topic column names
  topic_ids <- as.integer(gsub("^V", "", topic_cols))
  
  # Determine padding width based on the max topic number
  pad_width <- nchar(max(topic_ids))
  
  # Generate new names with padded numbers
  new_names <- sprintf(paste0("Topic%0", pad_width, "d"), topic_ids)
  
  # Apply renaming
  setnames(theta_dt, old = topic_cols, new = new_names)
  
  return(theta_dt)
}