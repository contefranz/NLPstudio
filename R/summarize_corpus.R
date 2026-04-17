if ( getRversion() >= "2.15.1" ) {
  utils::globalVariables( c( "org_ord" ) )
}
#' Fast Corpus Summarization
#'
#' Summarize a [quanteda::corpus()] in parallel using backends from the
#' **parallel** package. By default, summarization is parallelized with
#' PSOCK clusters ([parallel::clusterApplyLB()]) for stable cross-platform
#' performance and dynamic load balancing. Optionally, FORK-based
#' parallelism ([parallel::mclapply()]) may be requested on Linux/macOS,
#' but this can lead to instability with quanteda (see *Note*).
#'
#' @inheritParams tokenize_corpus
#' @param ... Additional arguments passed to [quanteda.textstats::textstat_summary().
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

#' @returns A [data.table] object with detailed information about each document.
#'
#' @import data.table
#' @importFrom quanteda is.corpus docnames ndoc
#' @importFrom quanteda.textstats textstat_summary
#' @importFrom parallel makeCluster stopCluster clusterApplyLB clusterExport mclapply
#' @importFrom cli cli_h2 cli_alert_info cli_alert_success
#' @export
summarize_corpus <- function(x, ncores = 1, nchunks = ncores, socket = c("PSOCK", "FORK"), ...) {
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
  
  cli::cli_h2("Summarizing corpus")
  # Sequential fallback
  if (length(chunks) <= 1L || ncores < 2L) {
    cli::cli_alert_info("Summarizing sequentially")
    out <- quanteda.textstats::textstat_summary(x, ...)
    data.table::setDT(out)
    if ("document" %in% names(out)) {
      data.table::setnames(out, "document", "doc_id")
    }
    cli::cli_alert_success("Corpus summarization complete")
    return(out)
  }
  
  cli::cli_alert_info("Summarizing {length(chunks)} chunks in parallel with {ncores} cores via {socket}")

  summaries <- .run_parallel(chunks, .summarize_chunk, ncores, socket,
                             export_vars = c(".summarize_chunk"),
                             export_env = environment(), ...)
  
  out <- data.table::rbindlist(summaries, fill = TRUE)
  # Ensure we have doc_id column
  if (!"doc_id" %in% names(out)) {
    stop("textstat_summary() output does not contain 'doc_id' or 'document'")
  }
  # restore order
  out <- out[match(doc_ids, out$doc_id)]  
  
  cli::cli_alert_success("Corpus summarization complete")
  return(out)
}

#' @keywords internal
.summarize_chunk <- function(corp_chunk, ...) {
  out <- quanteda.textstats::textstat_summary(corp_chunk, ...)
  data.table::setDT(out)
  if ("document" %in% names(out)) {
    data.table::setnames(out, "document", "doc_id")
  }
  out
}