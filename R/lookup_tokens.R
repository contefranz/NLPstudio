#' Fast Tokens Lookup
#'
#' Lookup a [tokens] object in parallel via **[future]**.
#'
#' @param x A **quanteda** [tokens] object.
#' @param ncores The number of [multisession] workers to be allocated for 
#' the lookup.
#' @param ... Additional arguments passed to [tokens_lookup].
#'
#' @returns A [tokens] object.
#'
#' @author Francesco Grossetti \email{francesco.grossetti@@unibocconi.it}
#'
#' @importFrom quanteda is.tokens tokens_lookup docnames
#' @importFrom future plan multisession sequential
#' @importFrom future.apply future_lapply
#' @importFrom cli cli_h2 cli_h3 cli_alert_info cli_alert cli_alert_success cli_alert_danger
#' @export


lookup_tokens = function(x, ncores, ...) {
  
  if ( !is.tokens(x) ) {
    stop("x must be a quanteda tokens object")
  }
  
  cli_h2("Looking up tokens")
  args = list(...)
  # exclude the dictionary as argument as it prints out all the tokens in it
  # Just report anything else by redefining args
  args = args[!sapply(args, is.dictionary)]
  if ( length(args) < 1 ) {
    cli_alert_info("quanteda::tokens_lookup() has been called with the default parameters")
  } else {
    cli_alert_info("quanteda::tokens_lookup() has been called with the following parameters")
    for (nm in names(args)) {
      cli_alert("{nm} = {toString(args[[nm]])}")
    }
  }
  
  # define the number of workers
  plan(multisession, workers = ncores)
  chunks = split(x, rep_len(1L:ncores, ndoc(x)))
  
  toks = do.call(c, future_lapply(chunks, tokens_lookup, future.seed = TRUE, ...))
  plan(sequential)
  # ensure original ordering
  doc_order <- docnames(x)
  toks <- toks[doc_order]
  
  cli_alert_success("Lookup complete")  
  return(toks)
}
