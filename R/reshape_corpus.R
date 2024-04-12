#' Fast Corpus Reshape
#'
#' Reshape a \code{\link[quanteda]{corpus}} in parallel under the \strong{future} paradigm.
#'
#' @param x A \code{\link[quanteda]{corpus}} as built by \code{create_corpus}.
#' @param ncores The number of \code{\link[future]{multisession}} workers to be allocated for the tokenization.
#' @param ... Additional arguments passed to \code{\link[quanteda]{corpus_reshape}} (see 'Details').
#'
#' @details
#' This function is a wrapper of \code{\link[quanteda]{corpus_reshape}} but it is much faster as it
#' uses the \href{https://www.futureverse.org/packages-overview.html}{\strong{future}}
#' paradigm to parallelize the task.
#'
#' Use \code{...} to control the reshaping process. For instance, whether to reshape the corpus to
#' sentences (\code{to = "sentences"}) which is likely the most common usage.
#'
#' @returns A reshaped \code{\link[quanteda]{corpus}} object.
#'
#' @author Francesco Grossetti \email{francesco.grossetti@@unibocconi.it}
#'
#' @importFrom quanteda is.corpus corpus_reshape
#' @importFrom future plan multisession sequential
#' @importFrom stringr str_remove
#' @importFrom future.apply future_lapply
#' @importFrom cli cli_h2 cli_alert_success cli_alert_danger
#' @export


reshape_corpus = function(x, ncores, ...) {

  if ( !is.corpus(x) ) {
    stop("x must be a quanteda corpus object")
  }

  # define the number of workers
  plan(multisession, workers = ncores)
  chunks = split(x, rep_len(1L:ncores, ndoc(x)))

  cli_h2("Reshaping")
  reshaped = do.call(c, future_lapply(chunks, corpus_reshape))
  plan(sequential)

  doc_names = sort(docnames(x))
  doc_names_reshaped = sort(unique(str_remove(docnames(reshaped), "\\.\\d+")))
  if ( all(doc_names == doc_names_reshaped) ) {
    cli_alert_success("Corpus x has been successfully reshaped")
  } else {
    cli_alert_danger("Reshaping failed")
    stop("Likely failed to process some sentences leading to dropping a document")
  }

  return(reshaped)
}
