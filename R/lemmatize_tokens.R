if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("token", "lemma", "N"))
}

#' Lemmatize Tokens
#'
#' Replace tokens with their lemmas. Two engines are available: a dependency-free
#' `"lookup"` engine that applies a user-supplied lemma map, and a `"spacy"`
#' engine that derives lemmas with the optional **spacyr** backend. In both cases
#' lemmas are applied at the token-type level via [quanteda::tokens_replace()].
#'
#' @inheritParams tokenize_corpus
#' @param x A [quanteda::tokens] object.
#' @param engine Character. `"lookup"` (default) applies the `lemma` map;
#'   `"spacy"` derives lemmas using **spacyr** (requires a working spaCy
#'   installation and an initialized model; see [parse_corpus()]).
#' @param lemma For `engine = "lookup"`, the lemma map: either a named character
#'   vector (`c(token = "lemma")`) or a `data.frame`/`data.table` with columns
#'   `token` and `lemma`. Ignored for `engine = "spacy"`.
#' @param ... For `engine = "spacy"`, additional arguments passed to
#'   [parse_corpus()] / [spacyr::spacy_parse()].
#'
#' @details
#' Lemmatization here is type-level: each distinct token form is mapped to a
#' single lemma and substituted with [quanteda::tokens_replace()]. The `"spacy"`
#' engine builds that map from a spaCy parse, taking the most frequent lemma per
#' token form, so it approximates fully context-sensitive lemmatization. For
#' richly context-dependent analyses, parse the corpus directly with
#' [parse_corpus()] and work from the returned annotations.
#'
#' The parallel arguments (`ncores`, `nchunks`, `socket`) apply only to the
#' `"spacy"` engine's parsing step.
#'
#' @returns A [quanteda::tokens] object with lemmatized tokens.
#'
#' @seealso [stem_tokens()], [singularize_tokens()], [parse_corpus()]
#'
#' @examples
#' corp <- quanteda::corpus(c(
#'   doc1 = "mice were running",
#'   doc2 = "the geese are flying"
#' ))
#' toks <- tokenize_corpus(corp)
#'
#' # Dependency-free lookup engine with a small custom map
#' lemma_map <- data.frame(
#'   token = c("mice", "were", "running", "geese", "are", "flying"),
#'   lemma = c("mouse", "be", "run", "goose", "be", "fly")
#' )
#' lemmatize_tokens(toks, engine = "lookup", lemma = lemma_map)
#'
#' @import data.table
#' @export
lemmatize_tokens <- function(x, engine = c("lookup", "spacy"), lemma = NULL,
                             ncores = 1, nchunks = ncores,
                             socket = c("PSOCK", "FORK"), ...) {
  if (!quanteda::is.tokens(x)) {
    stop("x must be a quanteda tokens object", call. = FALSE)
  }
  engine <- match.arg(engine)
  socket <- match.arg(socket)
  .validate_parallel_args(ncores, nchunks)

  cli::cli_h2("Lemmatizing tokens")

  if (engine == "lookup") {
    map <- .as_lemma_map(lemma)
    cli::cli_alert_info("Applying lookup lemma map ({nrow(map)} entries)")
    out <- quanteda::tokens_replace(
      x, pattern = map$token, replacement = map$lemma, valuetype = "fixed"
    )
    cli::cli_alert_success("Lemmatization complete")
    return(out)
  }

  # engine == "spacy"
  if (!.has_namespace("spacyr")) {
    stop("engine = \"spacy\" requires the 'spacyr' package and a working spaCy installation.",
         call. = FALSE)
  }
  cli::cli_alert_info("Deriving lemmas with spaCy")
  texts <- vapply(quanteda::as.list(x), paste, character(1L), collapse = " ")
  pseudo <- quanteda::corpus(texts)
  parsed <- parse_corpus(pseudo, ncores = ncores, nchunks = nchunks,
                         socket = socket, lemma = TRUE, ...)
  if (!all(c("token", "lemma") %in% names(parsed))) {
    stop("spaCy parse did not return 'token' and 'lemma' columns.", call. = FALSE)
  }
  map <- data.table::as.data.table(parsed)[
    !is.na(lemma) & nzchar(lemma), .N, by = .(token, lemma)
  ]
  data.table::setorderv(map, "N", order = -1L)
  map <- map[, .SD[1L], by = token]
  map <- map[token != lemma]
  out <- quanteda::tokens_replace(
    x, pattern = map$token, replacement = map$lemma, valuetype = "fixed"
  )
  cli::cli_alert_success("Lemmatization complete")
  out
}

#' Coerce a lemma map argument into a token/lemma data.table
#' @keywords internal
#' @noRd
.as_lemma_map <- function(lemma) {
  if (is.null(lemma)) {
    stop("engine = \"lookup\" requires a `lemma` map (named vector or data.frame with 'token' and 'lemma').",
         call. = FALSE)
  }
  if (is.character(lemma)) {
    if (is.null(names(lemma)) || any(!nzchar(names(lemma)))) {
      stop("A character `lemma` map must be a named vector, e.g. c(mice = \"mouse\").",
           call. = FALSE)
    }
    return(data.table::data.table(token = names(lemma), lemma = unname(lemma)))
  }
  if (is.data.frame(lemma)) {
    if (!all(c("token", "lemma") %in% names(lemma))) {
      stop("A data.frame `lemma` map must contain 'token' and 'lemma' columns.",
           call. = FALSE)
    }
    dt <- data.table::as.data.table(lemma)
    dt <- dt[, .(token = as.character(token), lemma = as.character(lemma))]
    return(dt[nzchar(token) & nzchar(lemma)])
  }
  stop("`lemma` must be a named character vector or a data.frame with 'token' and 'lemma'.",
       call. = FALSE)
}
