#' Compound Multiword Expressions into Single Tokens
#'
#' Join sequences of tokens (phrases or detected collocations) into single
#' compound tokens, in parallel. Wraps [quanteda::tokens_compound()] and composes
#' directly with [detect_collocations()].
#'
#' @inheritParams tokenize_corpus
#' @param x A [quanteda::tokens] object.
#' @param pattern Phrases to compound. One of: a character vector of phrases
#'   (e.g. `c("annual report", "cash flow")`); a `data.frame`/`data.table` with a
#'   `collocation` column, such as the output of [detect_collocations()]; or a
#'   **quanteda** collocations object.
#' @param concatenator Character used to join compounded tokens. Defaults to
#'   `"_"`.
#' @param ... Additional arguments passed to [quanteda::tokens_compound()].
#'
#' @details
#' The tokens object is split into balanced chunks by document and processed with
#' the package's shared parallel backend. See [tokenize_corpus()] for details.
#'
#' @returns A [quanteda::tokens] object with matched phrases compounded, in the
#'   same document order as the input.
#'
#' @seealso [detect_collocations()], [ngram_tokens()]
#'
#' @examples
#' corp <- quanteda::corpus(c(
#'   doc1 = "the annual report described cash flow risk",
#'   doc2 = "annual report disclosures mention cash flow"
#' ))
#' toks <- tokenize_corpus(corp)
#' compound_tokens(toks, pattern = c("annual report", "cash flow"))
#'
#' @export
compound_tokens <- function(x, pattern, concatenator = "_",
                            ncores = 1, nchunks = ncores,
                            socket = c("PSOCK", "FORK"), ...) {
  if (!quanteda::is.tokens(x)) {
    stop("x must be a quanteda tokens object", call. = FALSE)
  }
  if (missing(pattern) || is.null(pattern)) {
    stop("pattern must be supplied", call. = FALSE)
  }
  socket <- match.arg(socket)
  .validate_parallel_args(ncores, nchunks)

  pattern <- .as_compound_pattern(pattern)

  cli::cli_h2("Compounding multiword expressions")

  doc_ids <- quanteda::docnames(x)
  groups  <- split(doc_ids, rep_len(seq_len(max(1L, nchunks)), length(doc_ids)))
  chunks  <- lapply(groups, function(ids) if (length(ids)) x[ids] else NULL)
  chunks  <- Filter(Negate(is.null), chunks)

  if (length(chunks) <= 1L || ncores < 2L) {
    cli::cli_alert_info("Compounding sequentially")
    out <- quanteda::tokens_compound(x, pattern = pattern,
                                     concatenator = concatenator, ...)
  } else {
    cli::cli_alert_info("Compounding {nchunks} chunks in parallel with {ncores} cores via {socket}")
    out_list <- .run_parallel(chunks, .compound_chunk, ncores, socket,
                              export_vars = c(".compound_chunk"),
                              export_env = environment(),
                              pattern = pattern, concatenator = concatenator, ...)
    out <- Reduce(c, out_list)
    out <- out[doc_ids]
  }
  cli::cli_alert_success("Compounding complete")
  out
}

#' Coerce a compound `pattern` argument into phrases
#' @keywords internal
#' @noRd
.as_compound_pattern <- function(pattern) {
  if (inherits(pattern, "collocations")) {
    return(quanteda::phrase(pattern$collocation))
  }
  if (is.data.frame(pattern)) {
    if (!"collocation" %in% names(pattern)) {
      stop("A data.frame `pattern` must contain a 'collocation' column.",
           call. = FALSE)
    }
    return(quanteda::phrase(as.character(pattern$collocation)))
  }
  if (is.character(pattern)) {
    return(quanteda::phrase(pattern))
  }
  # already a phrase / list / dictionary -> pass through
  pattern
}

#' Compound one token chunk
#' @keywords internal
#' @noRd
.compound_chunk <- function(tok_chunk, ...) {
  quanteda::tokens_compound(tok_chunk, ...)
}
