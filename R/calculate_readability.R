#' Fast Calculation of Readability Measures
#'
#' Compute readability measures in parallel using backends from the
#' **parallel** package. By default, computation is parallelized with
#' PSOCK clusters ([parallel::clusterApplyLB()]) for cross-platform
#' stability. Optionally, FORK-based parallelism
#' ([parallel::mclapply()]) may be requested on Linux/macOS.
#'
#' @inheritParams tokenize_corpus
#' @param x A **quanteda** [corpus] or a character vector containing the documents to process.
#' @param ... Additional arguments passed to [textstat_readability].
#'
#' @returns A [data.table] with as many columns as passed to
#' `measure` and `doc_id` as the document identifier.
#'
#' @examples
#' texts <- c(
#'   doc1 = "This is a short and very simple document.",
#'   doc2 = "This second document contains slightly longer sentences for illustration."
#' )
#'
#' calculate_readability(
#'   texts,
#'   measure = c("Flesch", "FOG")
#' )
#'
#' @author Francesco Grossetti <francesco.grossetti@unibocconi.it>
#'
#' @import data.table
#' @export


calculate_readability = function(x, ncores = 1, nchunks = ncores, socket = c("PSOCK", "FORK"), ...) {

  if (!quanteda::is.corpus(x) && !is.character(x)) {
    stop("x must be a quanteda corpus object or a character vector containing the documents")
  }
  if (is.character(x) && !quanteda::is.corpus(x)) {
    x <- quanteda::corpus(x)
  }

  socket <- match.arg(socket)
  .validate_parallel_args(ncores, nchunks)

  cli::cli_h2("Calculating readability")
  args = list(...)
  if ( length(args) < 1 ) {
    cli::cli_alert_info("quanteda.textstats::textstat_readability() has been called with the default parameters")
  } else {
    cli::cli_alert_info("quanteda.textstats::textstat_readability() has been called with the following parameters")
    for (nm in names(args)) {
      cli::cli_alert_info("{nm} = {toString(args[[nm]])}")
    }
  }

  # Split corpus into balanced chunks by doc IDs
  doc_ids <- quanteda::docnames(x)
  groups  <- split(doc_ids, rep_len(seq_len(max(1L, nchunks)), length(doc_ids)))
  chunks  <- lapply(groups, function(ids) if (length(ids)) x[ids] else NULL)
  chunks  <- Filter(Negate(is.null), chunks)

  if (length(chunks) <= 1L || ncores < 2L) {
    cli::cli_alert_info("Computing readability sequentially")
    out_all <- data.table::as.data.table(quanteda.textstats::textstat_readability(x, ...))
  } else {
    cli::cli_alert_info("Computing readability for {length(chunks)} chunks in parallel with {ncores} cores via {socket}")
    results <- .run_parallel(chunks, .readability_chunk, ncores, socket,
                             export_vars = c(".readability_chunk"),
                             export_env = environment(), ...)
    out_all <- data.table::rbindlist(results, fill = TRUE)
  }

  data.table::setnames(out_all, "document", "doc_id")
  out_all[, org_ord := match(doc_id, doc_ids)]
  data.table::setorder(out_all, org_ord)
  out_all[, org_ord := NULL]
  cli::cli_alert_success("Done")
  return(out_all[])
}

#' Compute readability for one corpus chunk
#'
#' Wraps quanteda.textstats readability computation for a single sequential or parallel chunk.
#'
#' @keywords internal
#' @noRd
.readability_chunk <- function(corp_chunk, ...) {
  out <- quanteda.textstats::textstat_readability(corp_chunk, ...)
  data.table::as.data.table(out)
}
