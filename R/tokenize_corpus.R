#' Fast Corpus Tokenization
#'
#' Tokenize a [corpus] in parallel via **[future]**.
#'
#' @param x A **quanteda** [corpus] as built by `define_corpus`.
#' @param ncores The number of [multisession] workers to be allocated for the tokenization.
#' @param ... Additional arguments passed to [tokens].
#'
#' @returns A [tokens] object.
#'
#' @author Francesco Grossetti \email{francesco.grossetti@@unibocconi.it}
#'
#' @importFrom quanteda is.corpus tokens ndoc
#' @importFrom future plan multisession sequential
#' @importFrom future.apply future_lapply
#' @importFrom cli cli_h2 cli_h3 cli_alert_info cli_alert cli_alert_success cli_alert_danger
#' @export


tokenize_corpus = function(x, ncores, ...) {

  if ( !is.corpus(x) ) {
    stop("x must be a quanteda corpus object")
  }

  cli_h2("Tokenizing corpus")
  args = list(...)
  if ( length(args) < 1 ) {
    cli_alert_info("quanteda::tokens() has been called with the default parameters")
  } else {
    cli_alert_info("quanteda::tokens() has been called with the following parameters")
    args_active = paste0(names(args), " = ", unlist(args))
    for (iarg in seq_along(args_active) ) {
      cli_alert("{args_active[iarg]}")
    }
  }

  # define the number of workers
  plan(multisession, workers = ncores)
  chunks = split(x, rep_len(1L:ncores, ndoc(x)))

  toks = do.call(c, future_lapply(chunks, tokens, future.seed = TRUE, ...))
  plan(sequential)

  if ( ndoc(toks) == ndoc(x) ) {
    cli_alert_success("Corpus x has been successfully tokenized")
  } else {
    cli_alert_danger("Tokenization failed")
    stop("Different document numbers")
  }

  return(toks)
}
