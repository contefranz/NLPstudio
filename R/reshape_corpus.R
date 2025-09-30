#' Fast Corpus Reshape
#'
#' Reshape a [quanteda::corpus()] into smaller units (typically sentences or
#' paragraphs) using parallel backends from the **parallel** package.
#' By default, reshaping is parallelized with PSOCK clusters
#' ([parallel::clusterApplyLB()]) for cross-platform stability and dynamic
#' load balancing. Optionally, FORK-based parallelism
#' ([parallel::mclapply()]) may be requested on Linux/macOS, but this can
#' lead to instability with quanteda (see *Note*).
#'
#' @inheritParams tokenize_corpus
#' @param to Character. Reshape target, passed to
#'   [quanteda::corpus_reshape()]. Defaults to `"sentences"`.
#' @param ... Additional arguments passed to [quanteda::corpus_reshape()].
#'
#' @details
#' More details discussing the parallel strategy are given in [tokenize_corpus()].
#' @note
#' By default, `socket = "PSOCK"`. Using `socket = "FORK"` on Linux/macOS
#' may be faster but is discouraged when tokenizing large corpora with
#' quanteda, as it can lead to undefined behavior. If you insist on using
#' `socket = "FORK"`, consider setting environment variables such as
#' `OMP_NUM_THREADS=1` and/or `quanteda_options(threads = 1)`) to reduce conflicts. 
#' On Windows, setting `socket = "FORK"` will result in an error.
#'
#' @return A reshaped [quanteda::corpus()] with the same document variables
#' and reshaped text units as defined by `to`.
#'
#' @importFrom quanteda is.corpus docnames ndoc corpus_reshape
#' @importFrom parallel makeCluster stopCluster clusterApplyLB clusterExport mclapply
#' @importFrom cli cli_h2 cli_alert_info cli_alert_success
#' @export
reshape_corpus <- function(x, to = "sentences", ncores = 1, nchunks = ncores, socket = c("PSOCK", "FORK"), ...) {
  
  if (!quanteda::is.corpus(x)) {
    stop("x must be a quanteda corpus object")
  }
  socket <- match.arg(socket)
  
  # Split corpus into nchunks by doc IDs
  doc_ids <- quanteda::docnames(x)
  groups  <- split(doc_ids, rep_len(seq_len(max(1L, nchunks)), length(doc_ids)))
  chunks  <- lapply(groups, function(ids) if (length(ids)) x[ids] else NULL)
  chunks  <- Filter(Negate(is.null), chunks)
  
  # Sequential fallback
  if (length(chunks) <= 1L || ncores < 2L) {
    cli::cli_alert_info("Reshaping sequentially")
    corp <- quanteda::corpus_reshape(x, to = to, ...)
    return(corp)
  }
  
  cli::cli_alert_info("Reshaping {length(chunks)} chunks in parallel with {ncores} cores")
  
  if (socket == "FORK") {
    if (.Platform$OS.type == "windows") {
      stop("socket = 'FORK' is not supported on Windows. Use 'PSOCK'.")
    }
    warning("FORK sockets may be unstable with quanteda. Consider using 'PSOCK'.")
    corp_list <- parallel::mclapply(chunks, .reshape_chunk, mc.cores = ncores, to = to, ...)
  } else {
    cl <- parallel::makeCluster(ncores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterExport(cl, varlist = c(".reshape_chunk"), envir = environment())
    corp_list <- parallel::clusterApplyLB(cl, chunks, .reshape_chunk, to = to, ...)
  }
  cli::cli_alert_info("Combining the chunks")
  # Combine corpora
  corp <- Reduce(c, corp_list)
  
  cli::cli_alert_success("Corpus successfully reshaped")
  return(corp)
}

#' @keywords internal
.reshape_chunk <- function(corp_chunk, to, ...) {
  quanteda::corpus_reshape(corp_chunk, to = to, ...)
}