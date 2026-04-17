# Some utility functions

#' Validate parallel arguments
#' @keywords internal
#' @noRd
.validate_parallel_args <- function(ncores, nchunks) {
  if (!is.numeric(ncores) || length(ncores) != 1L || ncores < 1L || ncores != as.integer(ncores)) {
    stop("ncores must be a single positive integer")
  }
  if (!is.numeric(nchunks) || length(nchunks) != 1L || nchunks < 1L || nchunks != as.integer(nchunks)) {
    stop("nchunks must be a single positive integer")
  }
}

#' Run a function in parallel over a list of chunks
#'
#' Encapsulates the PSOCK/FORK branching logic used across the package.
#' On PSOCK, a cluster is created and torn down via `on.exit()`. On FORK,
#' `mclapply()` is used (Linux/macOS only).
#'
#' @param chunks A list of data chunks to process.
#' @param FUN The function to apply to each chunk.
#' @param ncores Integer. Number of cores.
#' @param socket Character. `"PSOCK"` or `"FORK"`.
#' @param export_vars Character vector of variable names to export to the
#'   PSOCK cluster. Ignored when `socket = "FORK"`.
#' @param export_env Environment from which to export variables.
#'   Defaults to the caller's environment.
#' @param ... Additional arguments passed to `FUN`.
#'
#' @return A list of results, one per chunk.
#' @keywords internal
#' @noRd
.run_parallel <- function(chunks, FUN, ncores, socket,
                          export_vars = NULL, export_env = parent.frame(), ...) {
  if (socket == "FORK") {
    if (.Platform$OS.type == "windows") {
      stop("socket = \"FORK\" is not supported on Windows. Use socket = \"PSOCK\" instead.")
    }
    warning("FORK sockets may be unstable with quanteda/C++ internals. Consider using \"PSOCK\".")
    res <- parallel::mclapply(chunks, FUN, mc.cores = ncores, ...)
  } else {
    cl <- parallel::makeCluster(ncores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    if (!is.null(export_vars)) {
      parallel::clusterExport(cl, varlist = export_vars, envir = export_env)
    }
    res <- parallel::clusterApplyLB(cl, chunks, FUN, ...)
  }
  res
}

#' @keywords internal
set_theta_names <- function(theta_dt) {
  
  setnames(theta_dt, "rn", "doc_id")
  
  # Detect the topic columns (assumes doc_id is the only non-topic column)
  topic_cols <- setdiff(names(theta_dt), "doc_id")
  
  # Extract the numeric part from the topic column names
  topic_ids <- as.integer(stringr::str_extract(topic_cols, "\\d+"))
  
  # Determine padding width based on the max topic number
  pad_width <- nchar(max(topic_ids))
  
  # Generate new names with padded numbers
  new_names <- sprintf(paste0("Topic%0", pad_width, "d"), topic_ids)
  
  # Apply renaming
  setnames(theta_dt, old = topic_cols, new = new_names)
  
  return(theta_dt)
}

