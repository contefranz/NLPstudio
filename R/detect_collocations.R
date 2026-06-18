#' Detect Candidate Multiword Expressions
#'
#' Score candidate collocations (multiword expressions) in a **quanteda**
#' [quanteda::tokens] object. This wraps
#' [quanteda.textstats::textstat_collocations()] and returns an export-ready
#' [data.table::data.table] that can be fed directly to [compound_tokens()].
#'
#' @param x A [quanteda::tokens] object.
#' @param size Integer vector. Length(s) of collocations to score (e.g. `2:3`
#'   for bigrams and trigrams).
#' @param min_count Integer. Minimum candidate frequency to be scored.
#' @param ... Additional arguments passed to
#'   [quanteda.textstats::textstat_collocations()].
#'
#' @returns A [data.table::data.table] with one row per candidate collocation and
#'   columns including `collocation`, `count`, `length`, `lambda`, and `z`,
#'   sorted by descending association strength (`lambda`).
#'
#' @seealso [compound_tokens()], [ngram_tokens()]
#'
#' @examples
#' corp <- quanteda::corpus(c(
#'   doc1 = "the annual report covered cash flow and annual report risk",
#'   doc2 = "cash flow guidance featured in the annual report"
#' ))
#' toks <- tokenize_corpus(corp)
#' detect_collocations(toks, size = 2, min_count = 2)
#'
#' @import data.table
#' @export
detect_collocations <- function(x, size = 2:3, min_count = 2L, ...) {
  if (!quanteda::is.tokens(x)) {
    stop("x must be a quanteda tokens object", call. = FALSE)
  }
  if (!is.numeric(size) || anyNA(size) || any(size < 2L) || any(size != as.integer(size))) {
    stop("size must be a vector of integers >= 2", call. = FALSE)
  }
  if (!is.numeric(min_count) || length(min_count) != 1L || is.na(min_count) ||
      min_count < 1L || min_count != as.integer(min_count)) {
    stop("min_count must be a single positive integer", call. = FALSE)
  }

  cli::cli_h2("Detecting collocations")
  res <- quanteda.textstats::textstat_collocations(
    x, size = as.integer(size), min_count = as.integer(min_count), ...
  )
  out <- data.table::as.data.table(res)
  if (nrow(out) && "lambda" %in% names(out)) {
    data.table::setorderv(out, "lambda", order = -1L)
  }
  cli::cli_alert_success("Found {nrow(out)} candidate collocation{?s}")
  out[]
}
