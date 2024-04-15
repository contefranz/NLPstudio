#' Fast Corpus Parsing via spaCy
#'
#' @description
#' Parse a \code{\link[quanteda]{corpus}} in parallel under the \strong{future} paradigm .
#' This function is a wrapper of \code{\link[spacyr]{spacy_parse}}. Thus, it is critical to have
#' a working installation of __spacyr__. Please refer to the
#' [installation guide](https://github.com/quanteda/spacyr?tab=readme-ov-file#installing-the-package)
#' to troubleshoot issues.
#'
#' @param x A \code{\link[quanteda]{corpus}}.
#' @param ncores The number of \code{\link[future]{multisession}} workers to be allocated for the
#' parsing. Default to 1.
#' @param ... Additional arguments passed to \code{\link[spacyr]{spacy_parse}}.
#'
#' @returns A \code{\link[data.table]{data.table}} of tokenized, parsed, and annotated tokens.
#'
#' @details
#' The workhorse of this function is \code{\link[spacyr]{spacy_parse}} so all the usual parameters
#' can be passed to \code{parse_corpus} too. It is critical to have a proper installation of the
#' [__spaCy__](https://spacy.io/) library and all its components.
#'
#' In particular, one can pass and use any language model as currently supported by version 3.7 via
#' the argument \code{model} in \code{\link[spacyr]{spacy_initialize}}.
#' By default, \code{\link[spacyr]{spacy_install}} downloads and uses the smallest English model
#' \code{en_core_web_sm}. It is recommended to use \code{\link[spacyr]{spacy_download_langmodel}}
#' to properly download and activate the desired model.
#'
#' To avoid any issue, \code{parse_corpus} always calls \code{\link[spacyr]{spacy_finalize}} by
#' invoking \code{on.exit}.
#'
#' @seealso [spacy_install()] [spacy_initialize()] [spacy_parse()]
#'
#' @author Francesco Grossetti \email{francesco.grossetti@@unibocconi.it}
#'
#' @importFrom quanteda is.corpus
#' @importFrom spacyr spacy_parse spacy_finalize
#' @importFrom future plan multisession sequential
#' @importFrom future.apply future_lapply
#' @importFrom stringr str_unique str_remove
#' @importFrom cli cli_h2 cli_alert_info cli_alert cli_alert_success
#' @export


parse_corpus = function(x, ncores = 1, ...) {

  if ( !is.corpus(x) ) {
    stop("x must be a quanteda corpus object")
  }

  cli_h2("Parsing corpus")
  args = list(...)
  if ( length(args) < 1 ) {
    cli_alert_info("spacyr::spacy_parse() has been called with the default parameters")
  } else {
    cli_alert_info("spacyr::spacy_parse() has been called with the following parameters")
    args_active = paste0(names(args), " = ", unlist(args))
    for (iarg in seq_along(args_active) ) {
      cli_alert("{args_active[iarg]}")
    }
  }

  # define the number of workers
  plan(multisession, workers = ncores)
  chunks = split(x, rep_len(1L:ncores, ndoc(x)))

  parsed = do.call(c, future_lapply(chunks, spacy_parse, future.seed = TRUE, ...))
  plan(sequential)

  cli_alert("Structuring data")
  col_names = names(parsed)
  col_names_fixed = str_unique(str_remove(col_names, "^\\d+\\."))
  n_cols = length(col_names)
  n_cols_unique = length(col_names_fixed)
  chunks = n_cols/n_cols_unique
  col_loc = seq(1L, n_cols, by = n_cols_unique)

  collector = vector("list", chunks)
  for ( icol in seq_along(col_loc) ) {
    if (icol < length(col_loc)) {
      temp = as.data.table(parsed[col_loc[icol]:(col_loc[icol + 1L] - 1L)])
    } else {
      temp = as.data.table(parsed[col_loc[icol]:n_cols])
    }
    setnames(temp, col_names_fixed)
    collector[[ icol ]] = temp
  }
  out = rbindlist(collector)
  cli_alert_success("Corpus x has been successfully parsed")
  on.exit(spacy_finalize())
  return(out[])
}
