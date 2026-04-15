#' Fast Tokens Lookup
#'
#' Lookup tokens in parallel using backends from the **parallel**
#' package. By default, lookup is parallelized with PSOCK clusters
#' ([parallel::clusterApplyLB()]) for stable cross-platform performance
#' and dynamic load balancing. Optionally, FORK-based parallelism
#' ([parallel::mclapply()]) may be requested on Linux/macOS, but this
#' can lead to instability with quanteda (see *Note*).
#' @inheritParams tokenize_corpus
#' @param ... Additional arguments passed to [quanteda::tokens_lookup()].
#'
#' @details
#' More details discussing the parallel strategy are given in [tokenize_corpus()].
#'
#' @note
#' By default, `socket = "PSOCK"`. Using `socket = "FORK"` on Linux/macOS
#' may be faster but is discouraged when tokenizing large corpora with
#' quanteda, as it can lead to undefined behavior. If you insist on using
#' `socket = "FORK"`, consider setting environment variables such as
#' `OMP_NUM_THREADS=1` and/or `quanteda_options(threads = 1)`) to reduce conflicts. 
#' On Windows, setting `socket = "FORK"` will result in an error.
#'
#' @return A [quanteda::tokens()] object with lookups applied and
#' documents in the same order as the input.
#'
#' @importFrom quanteda is.dictionary is.tokens tokens_lookup ndoc docnames
#' @importFrom parallel makeCluster stopCluster clusterApplyLB clusterExport mclapply
#' @importFrom cli cli_h2 cli_alert_info cli_alert_success
#' @export
lookup_tokens <- function(x, ncores = 1, nchunks = ncores, socket = c("PSOCK", "FORK"), ...) {
  if (!quanteda::is.tokens(x)) {
    stop("x must be a quanteda tokens object")
  }
  socket <- match.arg(socket)
  .validate_parallel_args(ncores, nchunks)

  args <- list(...)
  args <- args[!sapply(args, quanteda::is.dictionary)]  # filter dictionaries from logs
  if (length(args) < 1) {
    cli::cli_alert_info("quanteda::tokens_lookup() has been called with default parameters")
  } else {
    cli::cli_alert_info("quanteda::tokens_lookup() has been called with user parameters")
    for (nm in names(args)) cli::cli_alert_info("{nm} = {toString(args[[nm]])}")
  }
  
  # Split into chunks by doc IDs
  doc_ids <- quanteda::docnames(x)
  groups  <- split(doc_ids, rep_len(seq_len(max(1L, nchunks)), length(doc_ids)))
  chunks  <- lapply(groups, function(ids) if (length(ids)) x[ids] else NULL)
  chunks  <- Filter(Negate(is.null), chunks)
  
  if (length(chunks) <= 1L || ncores < 2L) {
    cli::cli_alert_info("Lookup sequentially")
    toks <- quanteda::tokens_lookup(x, ...)
    return(toks)
  }
  
  cli::cli_alert_info("Lookup {nchunks} chunks in parallel with {ncores} cores via {socket}")

  toks_list <- .run_parallel(chunks, .lookup_chunk, ncores, socket,
                             export_vars = c(".lookup_chunk"),
                             export_env = environment(), ...)
  
  toks <- Reduce(c, toks_list)
  toks <- toks[doc_ids]  # restore order
  
  cli::cli_alert_success("Lookup complete")
  return(toks)
}

#' @keywords internal
.lookup_chunk <- function(tok_chunk, ...) {
  quanteda::tokens_lookup(tok_chunk, ...)
}