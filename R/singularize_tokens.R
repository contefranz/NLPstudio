if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("feature"))
}
#' Fast Tokens Singularization
#'
#' Singularize tokens from a **quanteda** [tokens] object using a parallel
#' hashing strategy and an internal English singularization rule set. Short
#' tokens can optionally be removed.
#'
#' @inheritParams tokenize_corpus
#' @param x A [quanteda::tokens] object containing tokenized text.
#' @param remove_numbers Logical. If `TRUE` (default), removes tokens that
#'   contain any digits. This avoids producing incorrect singular forms (e.g.,
#'   `"000s"` → `"000"`).
#' @param min_char Integer. Minimum number of characters a token must have to
#'   be retained. Tokens shorter than this threshold are removed entirely.
#'   Defaults to 1.
#'
#' @details
#' More details discussing the parallel strategy are given in [tokenize_corpus()].
#'
#' @returns A [quanteda::tokens] object with singularized tokens.
#'
#' @note On Linux/macOS, `"FORK"` may be faster but can be unstable with
#' quanteda’s C++/OpenMP internals. Use `"PSOCK"` for maximum stability. On
#' Windows, `"FORK"` is not available.
#'
#' @examplesIf interactive()
#' corp <- quanteda::corpus(c(
#'   doc1 = "Cats chase birds and cars pass houses.",
#'   doc2 = "Companies file reports and managers review numbers."
#' ))
#' toks <- tokenize_corpus(corp)
#'
#' singularize_tokens(toks, min_char = 3)
#'
#'
#' @import data.table
#' @export
singularize_tokens <- function(x, ncores = 1, nchunks = ncores,
                               socket = c("PSOCK", "FORK"),
                               remove_numbers = TRUE, min_char = 1) {
  if (!quanteda::is.tokens(x)) stop("x must be a quanteda tokens object")
  
  socket <- match.arg(socket)
  .validate_parallel_args(ncores, nchunks)

  cli::cli_h2("Singularizing tokens")
  cli::cli_alert_info("Building DFM and extracting vocabulary")
  
  xdfm <- quanteda::dfm(x)
  vocabulary <- sort(quanteda::featnames(xdfm))
  
  if (remove_numbers) {
    cli::cli_alert_info("Removing tokens containing any number")
    vocabulary <- vocabulary[!grepl("\\d", vocabulary)]
  }
  
  if (min_char > 1) {
    cli::cli_alert_info("Removing tokens shorter than {min_char} characters")
    x <- quanteda::tokens_remove(
      x,
      pattern = paste0("^.{1,", min_char - 1, "}$"),
      valuetype = "regex"
    )
  }
  
  if (ncores < 2 || length(vocabulary) == 0L) {
    cli::cli_alert_info("Processing sequentially")
    hash_vocabulary <- data.table::data.table(
      feature = vocabulary,
      single = vapply(vocabulary, function(tok) .singularize(list(feature = tok)), character(1))
    )
  } else {
    cli::cli_alert_info("Processing {nchunks} chunks in parallel with {ncores} cores")
    
    hash_vocabulary <- data.table::data.table(feature = vocabulary, single = "")
    groups <- split(seq_along(vocabulary), rep_len(seq_len(nchunks), length(vocabulary)))
    chunks <- lapply(groups, function(ix) hash_vocabulary[ix, ])
    
    big_list <- .run_parallel(chunks, .singularize_chunk, ncores, socket,
                              export_vars = c(
                                ".singularize_chunk",
                                ".singularize",
                                ".singularize_vector",
                                ".singularize_word",
                                ".restore_singular_case",
                                ".nlp_uncountable_terms",
                                ".nlp_irregular_singulars"
                              ),
                              export_env = environment())
    
    hash_vocabulary <- data.table::rbindlist(big_list)
  }
  
  # filter out identity mappings
  hash_single <- hash_vocabulary[feature != single]
  
  cli::cli_alert_info("Replacing plural tokens with singulars")
  out <- quanteda::tokens_replace(
    x,
    pattern = hash_single$feature,
    replacement = hash_single$single,
    valuetype = "fixed"
  )
  cli::cli_alert_success("Singularization complete")
  return(out)
}

#' Singularize one token chunk
#'
#' Applies token singularization row by row for a chunk of tokens.
#'
#' @keywords internal
#' @noRd
.singularize_chunk <- function(current_chunk) {
  current_chunk[, single := .singularize_vector(feature)]
  current_chunk
}

#' Singularize one token row
#'
#' Applies internal singularization to one token vector.
#'
#' @keywords internal
#' @noRd
.singularize <- function(row) {
  .singularize_word(row$feature)
}

#' Singularize a character vector
#' @keywords internal
#' @noRd
.singularize_vector <- function(x) {
  vapply(x, .singularize_word, character(1), USE.NAMES = FALSE)
}

#' Common uncountable or already singular terms
#' @keywords internal
#' @noRd
.nlp_uncountable_terms <- c(
  "aircraft", "bison", "deer", "equipment", "fish", "information",
  "gas", "moose", "news", "offspring", "rice", "series", "sheep",
  "software", "species", "staff", "swine"
)

#' Common irregular plural-to-singular mappings
#' @keywords internal
#' @noRd
.nlp_irregular_singulars <- c(
  addenda = "addendum",
  algae = "alga",
  analyses = "analysis",
  appendices = "appendix",
  axes = "axis",
  bacteria = "bacterium",
  children = "child",
  corpora = "corpus",
  criteria = "criterion",
  crises = "crisis",
  diagnoses = "diagnosis",
  dice = "die",
  feet = "foot",
  formulae = "formula",
  geese = "goose",
  hypotheses = "hypothesis",
  indices = "index",
  matrices = "matrix",
  media = "medium",
  men = "man",
  mice = "mouse",
  nuclei = "nucleus",
  oxen = "ox",
  people = "person",
  phenomena = "phenomenon",
  stimuli = "stimulus",
  synopses = "synopsis",
  teeth = "tooth",
  theses = "thesis",
  vertebrae = "vertebra",
  vertices = "vertex",
  women = "woman"
)

#' Singularize one word
#' @keywords internal
#' @noRd
.singularize_word <- function(word) {
  if (length(word) == 0L) return(character())
  if (is.na(word)) return(NA_character_)
  if (!is.character(word)) word <- as.character(word)

  lower <- tolower(word)
  if (!nzchar(lower) || nchar(lower) <= 2L || lower %in% .nlp_uncountable_terms) {
    return(word)
  }

  if (lower %in% names(.nlp_irregular_singulars)) {
    return(.restore_singular_case(word, .nlp_irregular_singulars[[lower]]))
  }

  singular <- lower

  if (grepl("ies$", lower) && nchar(lower) > 4L) {
    singular <- sub("ies$", "y", lower)
  } else if (grepl("(shel|wol|lea|loa|cal|hal|el|sel|thie|scar|hoo|dwar|whar|tur)ves$", lower)) {
    singular <- sub("ves$", "f", lower)
  } else if (grepl("(li|wi|kni)ves$", lower)) {
    singular <- sub("ves$", "fe", lower)
  } else if (grepl("ves$", lower)) {
    singular <- sub("s$", "", lower)
  } else if (grepl("(ch|sh|ss|x|z)es$", lower)) {
    singular <- sub("es$", "", lower)
  } else if (grepl("oes$", lower)) {
    singular <- sub("es$", "", lower)
  } else if (grepl("s$", lower) &&
             !grepl("(ss|us|is|ous)$", lower) &&
             !lower %in% c("this", "his")) {
    singular <- sub("s$", "", lower)
  }

  .restore_singular_case(word, singular)
}

#' Restore common case shape after lower-case matching
#' @keywords internal
#' @noRd
.restore_singular_case <- function(original, singular) {
  if (!nzchar(original) || !nzchar(singular)) return(singular)
  if (identical(original, toupper(original))) {
    return(toupper(singular))
  }
  if (grepl("^[[:upper:]][[:lower:]]+$", original)) {
    return(paste0(toupper(substr(singular, 1L, 1L)), substr(singular, 2L, nchar(singular))))
  }
  singular
}
