#' Fast Corpus Tokenization
#'
#' Tokenize a [quanteda::corpus()] in parallel using backends from the
#' **parallel** package. By default, tokenization is parallelized with
#' PSOCK clusters ([parallel::clusterApplyLB()]) for stability across
#' platforms. Optionally, FORK-based parallelism
#' ([parallel::mclapply()]) may be requested on Linux/macOS, but this
#' can lead to crashes or silent failures because `quanteda` uses C++
#' and OpenMP internally.
#'
#' @param x A [quanteda::corpus()] object.
#' @param ncores Integer. Number of CPU cores to use for parallel
#'   processing. Defaults to 1 (sequential).
#' @param nchunks Integer. Number of chunks to split the corpus into.
#'   Defaults to `ncores`. Setting `nchunks > ncores` can improve load
#'   balancing when documents vary in size. See *Details*.
#' @param socket Character. Parallel backend to use. One of `"PSOCK"`
#'   (default, recommended) or `"FORK"`. On Windows, `"FORK"` is not
#'   supported and will trigger an error.
#' @param ... Additional arguments passed to [quanteda::tokens()].
#' 
#' @details
#' The corpus is first split into balanced chunks of documents depending on `ncores`.
#' Each chunk is tokenized in parallel by [quanteda::tokens()], and the
#' resulting token objects are combined. Original document order is
#' restored before returning the result.

#' The corpus is split into `nchunks` balanced chunks by document IDs.
#' With PSOCK backends, [parallel::clusterApplyLB()] is used to assign
#' chunks dynamically across `ncores` workers. This improves utilization
#' when document sizes are highly variable. With FORK, [parallel::mclapply()]
#' is used, which distributes chunks upfront.
#' 
#' Choosing the relationship between `ncores` and `nchunks` has important
#' performance implications:
#'
#' * When `nchunks == ncores` (the default), each worker processes exactly
#'   one chunk. This minimizes splitting overhead and is appropriate when
#'   documents are relatively homogeneous in length.
#'
#' * When `nchunks > ncores`, there are more chunks than workers. Workers
#'   receive chunks dynamically and pick up additional work as soon as
#'   they finish their current assignment. This improves load balancing
#'   when documents vary widely in size or complexity, but introduces
#'   some overhead from managing more tasks.
#'
#' * When `nchunks < ncores`, some workers will remain idle because there
#'   are not enough chunks to fully occupy all cores. This reduces overhead
#'   but wastes available parallel resources so it's not a recommended setting.
#'
#' In practice, setting `nchunks` slightly larger than `ncores` (e.g.,
#' 2-4x) often gives the best balance between parallel efficiency and
#' scheduling overhead for large, heterogeneous corpora.
#'
#' On small corpora, parallelization may add overhead compared to
#' sequential tokenization. For large corpora, using multiple cores with
#' the PSOCK backend typically yields the best balance of performance and
#' reliability. Although FORK can be faster by avoiding serialization,
#' it is less stable when combined with quanteda's use of C++/OpenMP.
#'
#' @note
#' By default, `socket = "PSOCK"`. Using `socket = "FORK"` on Linux/macOS
#' may be faster but is discouraged when tokenizing large corpora with
#' quanteda, as it can lead to undefined behavior. If you insist on using
#' `socket = "FORK"`, consider setting environment variables such as
#' `OMP_NUM_THREADS=1` and/or `quanteda_options(threads = 1)`) to reduce conflicts. 
#' On Windows, setting `socket = "FORK"` will result in an error.
#'
#' @returns A [quanteda::tokens()] object containing tokenized documents
#' with the same number and order of documents as the input corpus.
#'
#' @examples
#' corp <- quanteda::corpus(
#'   c("Cats are running", "Dogs were barking")
#' )
#'
#' toks <- tokenize_corpus(corp, remove_punct = TRUE)
#' toks
#'
#' @export
tokenize_corpus <- function(x, ncores = 1, nchunks = ncores, socket = c("PSOCK", "FORK"), ...) {
  
  if (!quanteda::is.corpus(x)) {
    stop("x must be a quanteda corpus object")
  }
  
  socket <- match.arg(socket)
  .validate_parallel_args(ncores, nchunks)
  args <- list(...)

  cli::cli_h2("Tokenizing corpus")
  if (length(args) < 1) {
    cli::cli_alert_info("quanteda::tokens() has been called with default parameters")
  } else {
    cli::cli_alert_info("quanteda::tokens() has been called with user parameters")
    args_active <- paste0(names(args), " = ", unlist(args))
    for (iarg in seq_along(args_active)) {
      cli::cli_alert("{args_active[iarg]}")
    }
  }

  # Single-doc fast path
  if (quanteda::ndoc(x) == 1L) {
    cli::cli_alert_info("Corpus has one document - running sequentially")
    return(quanteda::tokens(x, ...))
  }

  # Split into balanced chunks by doc IDs (never split the object itself first)
  doc_ids <- quanteda::docnames(x)
  groups  <- split(doc_ids, rep_len(seq_len(max(1L, nchunks)), length(doc_ids)))
  chunks  <- lapply(groups, function(ids) if (length(ids)) x[ids] else NULL)
  chunks  <- Filter(Negate(is.null), chunks)
  if (length(chunks) <= 1L || ncores < 2L) {
    cli::cli_alert_info("Tokenizing sequentially")
    toks <- quanteda::tokens(x, ...)
  } else {
    cli::cli_alert_info("Tokenizing {nchunks} chunks in parallel using {ncores} cores via {socket}")
    toks_list <- .run_parallel(chunks, .tokenize_chunk, ncores, socket,
                               export_vars = c(".tokenize_chunk"),
                               export_env = environment(), ...)
    toks <- Reduce(c, toks_list)
  }

  # Ensure original ordering
  toks <- toks[doc_ids]
  
  if (quanteda::ndoc(toks) == quanteda::ndoc(x)) {
    cli::cli_alert_success("Corpus successfully tokenized")
  } else {
    cli::cli_alert_danger("Tokenization failed")
    stop("Different document numbers between input and output")
  }
  return(toks)
}

#' Tokenize one corpus chunk
#'
#' Runs quanteda tokenization on a single corpus chunk.
#'
#' @keywords internal
#' @noRd
.tokenize_chunk <- function(corp_chunk, ...) {
  quanteda::tokens(corp_chunk, ...)
}
