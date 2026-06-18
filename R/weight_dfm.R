#' Weight a Document-Feature Matrix
#'
#' Apply a feature-weighting scheme to a **quanteda** [quanteda::dfm]. This wraps
#' [quanteda::dfm_tfidf()] and [quanteda::dfm_weight()] behind a single verb so
#' weighting stays inside the NLPstudio API.
#'
#' @param x A [quanteda::dfm] object.
#' @param scheme Character. Weighting scheme. `"tfidf"` (default) applies
#'   [quanteda::dfm_tfidf()]; every other scheme is passed to
#'   [quanteda::dfm_weight()]: `"count"`, `"prop"`, `"propmax"`, `"logcount"`,
#'   `"boolean"`, `"augmented"`, or `"logave"`.
#' @param ... Additional arguments passed to the underlying **quanteda**
#'   function (e.g. `scheme_tf` or `base` for `"tfidf"`).
#'
#' @returns A weighted [quanteda::dfm].
#'
#' @seealso [quanteda::dfm_tfidf()], [quanteda::dfm_weight()]
#'
#' @examples
#' corp <- quanteda::corpus(c(
#'   doc1 = "money money risk",
#'   doc2 = "risk growth growth growth"
#' ))
#' dfmat <- quanteda::dfm(tokenize_corpus(corp))
#' weight_dfm(dfmat, scheme = "tfidf")
#'
#' @export
weight_dfm <- function(x, scheme = c("tfidf", "count", "prop", "propmax",
                                     "logcount", "boolean", "augmented", "logave"),
                       ...) {
  if (!quanteda::is.dfm(x)) {
    stop("x must be a quanteda dfm object", call. = FALSE)
  }
  scheme <- match.arg(scheme)

  cli::cli_h2("Weighting dfm")
  cli::cli_alert_info("scheme = {scheme}")
  out <- if (scheme == "tfidf") {
    quanteda::dfm_tfidf(x, ...)
  } else {
    quanteda::dfm_weight(x, scheme = scheme, ...)
  }
  cli::cli_alert_success("Weighting complete")
  out
}
