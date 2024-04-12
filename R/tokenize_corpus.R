#' Fast Corpus Tokenization
#'
#' Tokenize a \code{\link[quanteda]{corpus}} in parallel.
#'
#' @param x A \code{\link[quanteda]{corpus}} as built by \code{create_corpus}.
#' @param ncores The number of \code{\link[future]{multisession}} workers to be allocated for the tokenization.
#' @param ... Additional arguments passed to \code{\link[quanteda]{tokens}}.
#'
#' @returns A \code{\link[quanteda]{tokens}} object.
#'
#' @author Francesco Grossetti \email{francesco.grossetti@@unibocconi.it}
#'
#' @importFrom quanteda is.corpus tokens ndoc
#' @importFrom future plan multisession sequential
#' @importFrom future.apply future_lapply
#' @importFrom cli cli_h2 cli_alert_success cli_alert_danger
#' @export


tokenize_corpus = function(x, ncores, ...) {

  if ( !is.corpus(x) ) {
    stop("x must be a quanteda corpus object")
  }

  # define the number of workers
  plan(multisession, workers = ncores)
  chunks = split(x, rep_len(1L:ncores, ndoc(x)))

  cli_h2("Tokenizing")
  toks = do.call(c, future_lapply(chunks, tokens))
  plan(sequential)

  if ( ndoc(toks) == ndoc(x) ) {
    cli_alert_success("Corpus x has been successfully tokenized")
  } else {
    cli_alert_danger("Tokenization failed")
    stop("Different document numbers")
  }

  return(toks)
}
