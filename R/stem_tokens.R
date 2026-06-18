if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("stem"))
}

#' Stem Tokens
#'
#' Reduce tokens to their stems using the Snowball stemmer, in parallel. This is a
#' parallel-aware wrapper around [quanteda::char_wordstem()] applied to the token
#' vocabulary, so it scales to large corpora. Unlike [singularize_tokens()]
#' (English plurals only), stemming supports many languages via Snowball.
#'
#' @inheritParams tokenize_corpus
#' @param x A [quanteda::tokens] object.
#' @param language Character. Snowball stemmer language. Defaults to `"english"`.
#'   See [SnowballC::getStemLanguages()] for the full list.
#'
#' @details
#' Stemming is applied once per unique token type and then mapped back onto the
#' tokens with [quanteda::tokens_replace()], mirroring the strategy used in
#' [singularize_tokens()]. See [tokenize_corpus()] for the parallel backend.
#'
#' @returns A [quanteda::tokens] object with stemmed tokens.
#'
#' @seealso [singularize_tokens()], [lemmatize_tokens()]
#'
#' @examples
#' corp <- quanteda::corpus(c(
#'   doc1 = "running runners ran easily",
#'   doc2 = "computational computers compute"
#' ))
#' toks <- tokenize_corpus(corp)
#' stem_tokens(toks)
#'
#' @import data.table
#' @export
stem_tokens <- function(x, language = "english", ncores = 1, nchunks = ncores,
                        socket = c("PSOCK", "FORK")) {
  if (!quanteda::is.tokens(x)) {
    stop("x must be a quanteda tokens object", call. = FALSE)
  }
  if (!is.character(language) || length(language) != 1L) {
    stop("language must be a single string", call. = FALSE)
  }
  socket <- match.arg(socket)
  .validate_parallel_args(ncores, nchunks)

  cli::cli_h2("Stemming tokens")
  cli::cli_alert_info("Extracting vocabulary")
  vocabulary <- sort(quanteda::types(x))

  if (length(vocabulary) == 0L) {
    cli::cli_alert_success("Stemming complete")
    return(x)
  }

  if (ncores < 2L) {
    cli::cli_alert_info("Stemming sequentially")
    hash <- data.table::data.table(
      feature = vocabulary,
      stem = quanteda::char_wordstem(vocabulary, language = language)
    )
  } else {
    cli::cli_alert_info("Stemming {nchunks} chunks in parallel with {ncores} cores via {socket}")
    base <- data.table::data.table(feature = vocabulary, stem = "")
    groups <- split(seq_along(vocabulary), rep_len(seq_len(nchunks), length(vocabulary)))
    chunks <- lapply(groups, function(ix) base[ix, ])
    stem_list <- .run_parallel(chunks, .stem_chunk, ncores, socket,
                               export_vars = c(".stem_chunk"),
                               export_env = environment(),
                               language = language)
    hash <- data.table::rbindlist(stem_list)
  }

  hash <- hash[feature != stem & nzchar(stem)]

  out <- if (nrow(hash)) {
    quanteda::tokens_replace(
      x, pattern = hash$feature, replacement = hash$stem, valuetype = "fixed"
    )
  } else {
    x
  }
  cli::cli_alert_success("Stemming complete")
  out
}

#' Stem one vocabulary chunk
#'
#' Stems the `feature` column of a chunk in place, keeping feature/stem pairing.
#'
#' @keywords internal
#' @noRd
.stem_chunk <- function(current_chunk, language) {
  current_chunk[, stem := quanteda::char_wordstem(feature, language = language)]
  current_chunk
}
