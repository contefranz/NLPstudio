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
#' @returns A reshaped [quanteda::corpus()] with the same document variables
#' and reshaped text units as defined by `to`.
#'
#' @examples
#' corp <- quanteda::corpus(c(
#'   doc1 = "First sentence. Second sentence.",
#'   doc2 = "Another document. With two parts."
#' ))
#'
#' reshape_corpus(corp, to = "sentences")
#'
#' @export
reshape_corpus <- function(x, to = "sentences", ncores = 1, nchunks = ncores, socket = c("PSOCK", "FORK"), ...) {
  
  if (!quanteda::is.corpus(x)) {
    stop("x must be a quanteda corpus object")
  }
  socket <- match.arg(socket)
  .validate_parallel_args(ncores, nchunks)

  # Split corpus into nchunks by doc IDs
  doc_ids <- quanteda::docnames(x)
  groups  <- split(doc_ids, rep_len(seq_len(max(1L, nchunks)), length(doc_ids)))
  chunks  <- lapply(groups, function(ids) if (length(ids)) x[ids] else NULL)
  chunks  <- Filter(Negate(is.null), chunks)
  
  cli::cli_h2("Reshaping corpus")
  # Sequential fallback
  if (length(chunks) <= 1L || ncores < 2L) {
    cli::cli_alert_info("Reshaping sequentially")
    corp <- quanteda::corpus_reshape(x, to = to, ...)
    cli::cli_alert_success("Corpus successfully reshaped")
    return(corp)
  }
  
  cli::cli_alert_info("Reshaping {length(chunks)} chunks in parallel with {ncores} cores via {socket}")

  corp_list <- .run_parallel(chunks, .reshape_chunk, ncores, socket,
                             export_vars = c(".reshape_chunk"),
                             export_env = environment(), to = to, ...)
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
