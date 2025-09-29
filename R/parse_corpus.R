#' Fast Corpus Parsing via spaCy
#'
#' @description
#' Parse a [corpus] in parallel under the **[future]** paradigm .
#' This function is a wrapper of [spacy_parse()]. Thus, it is critical to have
#' a working installation of the **[spacyr]** package. Please refer to the
#' [installation guide](https://github.com/quanteda/spacyr?tab=readme-ov-file#installing-the-package)
#' to troubleshoot issues.
#'
#' @param x A **quanteda** [corpus].
#' @param ncores The number of [multisession] workers to be allocated for the
#' parsing. Default to 1.
#' @param ... Additional arguments passed to [spacy_parse()].
#'
#' @returns A [data.table] of tokenized, parsed, and annotated tokens.
#'
#' @details
#' The workhorse of this function is [spacy_parse()] such that all the usual parameters
#' can be passed to `parse_corpus()` as well. It is critical to have a proper installation of the
#' [__spaCy__](https://spacy.io/) library and all of its components. `parse_corpus()` does not
#' initialize any instance of spaCy so call [spacy_initialize()] beforehand. 
#'
#' In particular, one can pass and use any language model as currently supported by version 3.7 via
#' the argument `model` in [spacy_initialize()].
#' By default, [spacy_install()] downloads and uses the smallest English model
#' `en_core_web_sm`. It is recommended to use [spacy_download_langmodel()]
#' to properly download and activate the desired model.
#'
#' To avoid any issue, `parse_corpus()` finalizes the session if one is active via [spacy_finalize()] `on.exit()`.
#' If no session is active, `parse_corpus()` will error on exit.
#' 
#' @note
#' Although parsing can be parallelized across multiple CPU cores, memory usage
#' grows quickly with both the number of cores and the size of the corpus.
#' On large corpora, allocating too many workers may exhaust available RAM and
#' significantly slow down or even terminate the process. It is recommended to
#' increase `ncores` gradually and monitor memory consumption.
#' 
#' Note that the returned [data.table] may contain a very large number of rows
#' when `x` is large, which also can have implications for memory usage and downstream
#' processing.
#' 
#' @seealso [spacy_install()] [spacy_initialize()] [spacy_parse()]
#'
#' @author Francesco Grossetti \email{francesco.grossetti@@unibocconi.it}
#'
#' @importFrom quanteda is.corpus ndoc
#' @importFrom future plan multisession sequential
#' @importFrom future.apply future_lapply
#' @importFrom stringr str_unique str_remove
#' @importFrom cli cli_h2 cli_alert_info cli_alert cli_alert_success
#' @export


parse_corpus = function(x, ncores = 1, ...) {

  if (!requireNamespace("spacyr", quietly = TRUE)) {
    stop("Package 'spacyr' is required for parse_corpus(). Please install it.")
  }
  if ( !is.corpus(x) ) {
    stop("x must be a quanteda corpus object")
  }

  cli_h2("Parsing corpus")
  args = list(...)
  if ( length(args) < 1 ) {
    cli_alert_info("spacyr::spacy_parse() has been called with the default parameters")
  } else {
    cli_alert_info("spacyr::spacy_parse() has been called with the following parameters")
    for (nm in names(args)) {
      cli_alert("{nm} = {toString(args[[nm]])}")
    }
  }

  # define the number of workers
  parsing_func <- getExportedValue("spacyr", "spacy_parse")
  plan(multisession, workers = ncores)
  chunks = split(x, rep_len(1L:ncores, ndoc(x)))

  parsed = do.call(c, future_lapply(chunks, parsing_func, future.seed = TRUE, ...))
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
  parsing_final <- getExportedValue("spacyr", "spacy_finalize")
  on.exit(parsing_final(), add = TRUE)
  return(out[])
}
