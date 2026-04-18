#' Fast Corpus Parsing via spaCy
#'
#' @description
#' Parse a [corpus] in parallel using backends from the **parallel** package.
#' This function is a wrapper of [spacy_parse()]. Thus, it is critical to have
#' a working installation of the **[spacyr]** package. Please refer to the
#' [installation guide](https://github.com/quanteda/spacyr?tab=readme-ov-file#installing-the-package)
#' to troubleshoot issues.
#'
#' @inheritParams tokenize_corpus
#' @param x A **quanteda** [corpus].
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
#' @import data.table
#' @export


parse_corpus = function(x, ncores = 1, nchunks = ncores, socket = c("PSOCK", "FORK"), ...) {

  if (!requireNamespace("spacyr", quietly = TRUE)) {
    stop("Package 'spacyr' is required for parse_corpus(). Please install it.")
  }
  if (!quanteda::is.corpus(x)) {
    stop("x must be a quanteda corpus object")
  }

  socket <- match.arg(socket)
  .validate_parallel_args(ncores, nchunks)

  cli::cli_h2("Parsing corpus")
  args = list(...)
  if ( length(args) < 1 ) {
    cli::cli_alert_info("spacyr::spacy_parse() has been called with the default parameters")
  } else {
    cli::cli_alert_info("spacyr::spacy_parse() has been called with the following parameters")
    for (nm in names(args)) {
      cli::cli_alert("{nm} = {toString(args[[nm]])}")
    }
  }

  # Ensure spaCy is finalized on exit (before any work that could fail)
  parsing_final <- getExportedValue("spacyr", "spacy_finalize")
  on.exit(parsing_final(), add = TRUE)

  # Split corpus into balanced chunks by doc IDs
  doc_ids <- quanteda::docnames(x)
  groups  <- split(doc_ids, rep_len(seq_len(max(1L, nchunks)), length(doc_ids)))
  chunks  <- lapply(groups, function(ids) if (length(ids)) x[ids] else NULL)
  chunks  <- Filter(Negate(is.null), chunks)

  if (length(chunks) <= 1L || ncores < 2L) {
    cli::cli_alert_info("Parsing sequentially")
    parsing_func <- getExportedValue("spacyr", "spacy_parse")
    out <- data.table::as.data.table(parsing_func(x, ...))
  } else {
    cli::cli_alert_info("Parsing {length(chunks)} chunks in parallel with {ncores} cores via {socket}")
    results <- .run_parallel(chunks, .parse_chunk, ncores, socket,
                             export_vars = c(".parse_chunk"),
                             export_env = environment(), ...)
    out <- data.table::rbindlist(results, fill = TRUE)
  }

  cli::cli_alert_success("Corpus x has been successfully parsed")
  return(out[])
}

#' @keywords internal
.parse_chunk <- function(corp_chunk, ...) {
  parsing_func <- getExportedValue("spacyr", "spacy_parse")
  out <- parsing_func(corp_chunk, ...)
  data.table::as.data.table(out)
}
