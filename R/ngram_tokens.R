#' Construct Token N-Grams
#'
#' Build n-grams (and optional skip-grams) from a **quanteda** [quanteda::tokens]
#' object in parallel, keeping the workflow inside the NLPstudio API. This is a
#' thin, parallel-aware wrapper around [quanteda::tokens_ngrams()].
#'
#' @inheritParams tokenize_corpus
#' @param x A [quanteda::tokens] object.
#' @param n Integer vector. The number(s) of tokens to concatenate. For example
#'   `n = 2` produces bigrams and `n = 1:2` keeps both unigrams and bigrams.
#' @param skip Integer vector. The number of tokens to skip when forming
#'   n-grams. `skip = 0` (default) yields adjacent n-grams; positive values
#'   yield skip-grams. See [quanteda::tokens_ngrams()].
#' @param concatenator Character used to join tokens within an n-gram. Defaults
#'   to `"_"`.
#'
#' @details
#' The tokens object is split into balanced chunks by document and processed with
#' the package's shared parallel backend. See [tokenize_corpus()] for the
#' chunking strategy and backend details.
#'
#' @returns A [quanteda::tokens] object containing the requested n-grams, with
#'   documents in the same order as the input.
#'
#' @seealso [compound_tokens()], [detect_collocations()]
#'
#' @examples
#' corp <- quanteda::corpus(c(
#'   doc1 = "the quick brown fox",
#'   doc2 = "a slow green turtle"
#' ))
#' toks <- tokenize_corpus(corp)
#' ngram_tokens(toks, n = 2)
#'
#' @export
ngram_tokens <- function(x, n = 2L, skip = 0L, concatenator = "_",
                         ncores = 1, nchunks = ncores,
                         socket = c("PSOCK", "FORK")) {
  if (!quanteda::is.tokens(x)) {
    stop("x must be a quanteda tokens object", call. = FALSE)
  }
  if (!is.numeric(n) || anyNA(n) || any(n < 1L) || any(n != as.integer(n))) {
    stop("n must be a vector of positive integers", call. = FALSE)
  }
  if (!is.numeric(skip) || anyNA(skip) || any(skip < 0L) || any(skip != as.integer(skip))) {
    stop("skip must be a vector of non-negative integers", call. = FALSE)
  }
  if (!is.character(concatenator) || length(concatenator) != 1L) {
    stop("concatenator must be a single string", call. = FALSE)
  }
  socket <- match.arg(socket)
  .validate_parallel_args(ncores, nchunks)

  n <- as.integer(n)
  skip <- as.integer(skip)

  cli::cli_h2("Building token n-grams")
  cli::cli_alert_info("n = {toString(n)}; skip = {toString(skip)}")

  doc_ids <- quanteda::docnames(x)
  groups  <- split(doc_ids, rep_len(seq_len(max(1L, nchunks)), length(doc_ids)))
  chunks  <- lapply(groups, function(ids) if (length(ids)) x[ids] else NULL)
  chunks  <- Filter(Negate(is.null), chunks)

  if (length(chunks) <= 1L || ncores < 2L) {
    cli::cli_alert_info("Building sequentially")
    out <- quanteda::tokens_ngrams(x, n = n, skip = skip, concatenator = concatenator)
  } else {
    cli::cli_alert_info("Building {nchunks} chunks in parallel with {ncores} cores via {socket}")
    # NB: forward under non-colliding names. A bare `n =` would partially match
    # the `ncores` formal of .run_parallel() and scramble argument matching.
    out_list <- .run_parallel(chunks, .ngram_chunk, ncores, socket,
                              export_vars = c(".ngram_chunk"),
                              export_env = environment(),
                              ngram_n = n, ngram_skip = skip,
                              ngram_concatenator = concatenator)
    out <- Reduce(c, out_list)
    out <- out[doc_ids]
  }
  cli::cli_alert_success("N-grams complete")
  out
}

#' Build n-grams for one token chunk
#'
#' Parameters are forwarded under prefixed names to avoid partial-argument-match
#' collisions with the parallel backend's formals (e.g. `n` vs `ncores`).
#'
#' @keywords internal
#' @noRd
.ngram_chunk <- function(tok_chunk, ngram_n, ngram_skip, ngram_concatenator) {
  quanteda::tokens_ngrams(tok_chunk, n = ngram_n, skip = ngram_skip,
                          concatenator = ngram_concatenator)
}
