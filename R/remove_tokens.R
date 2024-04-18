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
#' @importFrom collapse rsplit
#' @importFrom future plan multisession sequential
#' @importFrom future.apply future_lapply
#' @importFrom cli cli_h2 cli_h3 cli_alert_info cli_alert cli_alert_success cli_alert_danger
#' @export


remove_tokens = function(x, ncores, ...) {

  if ( !is.tokens(x) ) {
    stop("x must be a quanteda tokens object")
  }

  cli_h2("Removing tokens")
  args = list(...)
  if ( length(args) < 1 ) {
    cli_alert_info("quanteda::tokens_remove() has been called with the default parameters")
  } else {
    cli_alert_info("quanteda::tokens_remove() has been called with the following parameters")
    args_active = paste0(names(args), " = ", unlist(args))
    for (iarg in seq_along(args_active) ) {
      cli_alert("{args_active[iarg]}")
    }
  }

  chunks = rsplit(x, rep_len(1L:ncores, ndoc(x)))
  # define the number of workers
  plan(multisession, workers = ncores)
  # chunks = split(x, rep_len(1L:ncores, ndoc(x)))

  # toks = do.call(c, future_lapply(chunks, tokens_remove, future.seed = TRUE,
  #                                 future.packages = "collapse", ...))

  toks = future_lapply(chunks, tokens_remove, future.seed = TRUE, future.packages = "collapse", ...)

  plan(sequential)

  # if ( ndoc(toks) == ndoc(x) ) {
  #   cli_alert_success("Tokens has been successfully removed")
  # } else {
  #   cli_alert_danger("Removal failed")
  #   stop("Different document numbers")
  # }

  return(toks)
}
