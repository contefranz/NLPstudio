if ( getRversion() >= "2.15.1" ) {
  utils::globalVariables( c("ichunk", "feature") )
}
#' Fast Tokens Singularization
#'
#' Singularize tokens from a **quanteda** [tokens] object using a parallel
#' hashing strategy. Internally relies on the [singularize()] function from
#' the **pluralize** package. Short tokens can optionally be removed.
#'
#' @param x A [tokens][quanteda::tokens] object containing tokenized text.
#' @param ncores Integer. Number of CPU cores to use for parallel processing.
#'   On Linux and macOS, [parallel::mclapply()] is used; on Windows,
#'   [parallel::parLapply()] with a PSOCK cluster is used. Defaults to 1.
#'   On small datasets, parallelization may add overhead without speed gains.
#' @param remove_numbers Logical. If `TRUE` (default), removes tokens that
#'   contain any digits. This avoids producing incorrect singular forms
#'   for numeric tokens (e.g., `"000s"` would become `"000"`).
#' @param min_char Integer. Minimum number of characters a token must have
#'   to be retained in the output. Tokens shorter than this threshold are
#'   removed entirely. Defaults to 1.
#'
#' @details
#' Traditional singularization functions operate on character vectors,
#' which is inefficient for large vocabularies. In contrast,
#' `singularize_tokens()` works directly on a [tokens] object,
#' taking advantage of its efficient internal representation.
#'
#' The function first extracts the vocabulary by converting the tokens
#' to a [dfm][quanteda::dfm], then builds a hash table of tokens and
#' their singularized forms. Singularization is carried out in parallel
#' across chunks of the vocabulary, with each token mapped to its
#' singular equivalent using [pluralize::singularize()]. The results are
#' combined into a single mapping and then applied back to the token
#' object with [quanteda::tokens_replace()], producing a singularized
#' token stream.
#'
#' This approach is conceptually similar to lemmatization, but optimized
#' for vocabulary-level replacement. The main computational cost is the
#' initial `dfm` conversion, which is unavoidable.
#'
#' @return A [quanteda::tokens] object with plural tokens replaced by their
#' singular forms and short tokens removed according to `min_char`.
#'
#' @note The function requires the **pluralize** package, which should be
#'   installed separately if not already available.
#'
#' @author Francesco Grossetti \email{francesco.grossetti@@unibocconi.it}
#'
#' @examples
#' \dontrun{
#' library(NLPstudio)
#'
#' # Example tokens with plural words and short tokens
#' toks <- tokens("cats dogs a xx houses")
#'
#' # Singularize and remove tokens shorter than 3 characters
#' singular_toks <- singularize_tokens(toks, min_char = 3)
#' singular_toks
#' # Output will keep "cat", "dog", "house"
#' # and remove "a", "xx"
#' }
#'
#' @import data.table
#' @importFrom quanteda is.tokens dfm featnames tokens_replace tokens_remove
#' @importFrom stringr str_remove str_detect str_c regex
#' @importFrom parallel makeCluster stopCluster mclapply parLapply
#' @importFrom cli cli_h2 cli_alert_info cli_alert_warning cli_alert_success
#' @export

singularize_tokens = function(x, ncores = 1, remove_numbers = TRUE, min_char = 1) {
  
  # SINGULARIZED TOKENS BY HASHING
  # Note. This function works on the raw object "tokens". It uses the package "pluralize"
  # to singularize individual tokens within the quanteda framework.
  # The code uses hashing to apply singularization rules. The rules are defined via parallel backend
  # This allows the function to be super super fast and efficient.
  
  if (!requireNamespace("pluralize", quietly = TRUE)) {
    stop("Package 'pluralize' is required for singularize_tokens(). Please install it.", call. = FALSE)
  }
  
  if ( !is.tokens(x) ) {
    stop("x must be a quanteda tokens object")
  }
  
  cli_h2("Singularizing tokens")
  cli_alert_info("Building DFM and extracting vocabulary")
  xdfm <- dfm(x)
  vocabulary <- sort(featnames(xdfm))
  if ( remove_numbers ) {
    cli_alert_info("Removing tokens containing any number")
    # Remove numbers or elements that contain any number because singularize plays dumb.
    # If it finds the string "000s" it will convert it to "000". This
    vocabulary <- vocabulary[ !str_detect(vocabulary, "\\d")]
  }
  if (min_char > 1) {
    cli::cli_alert_info("Removing tokens shorter than {min_char} characters")
    x <- quanteda::tokens_remove(
      x,
      pattern = paste0("^.{1,", min_char - 1, "}$"),
      valuetype = "regex"
    )
  }  
  cli_alert_info("Defining the singular tokens hash table")
  # Define the hash table for the vocabulary in which I already pre-allocate the new column
  hash_vocabulary <- data.table(feature = vocabulary, single = "")
  chunks <- split(hash_vocabulary, rep_len(1L:ncores, nrow(hash_vocabulary)))
  
  worker_fun <- function(current_chunk) {
    singularize_fun <- getExportedValue("pluralize", "singularize")
    for (ifeat in seq_len(nrow(current_chunk))) {
      data.table::set(
        x = current_chunk,
        i = ifeat,
        j = "single",
        value = .singularize(current_chunk[ifeat])
      )
    }
    current_chunk
  }
  
  if (.Platform$OS.type != "windows" && requireNamespace("parallel", quietly = TRUE)) {
    big_list <- parallel::mclapply(chunks, .singularize_chunk, mc.cores = ncores)
  } else {
    cl <- parallel::makeCluster(ncores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    big_list <- parallel::parLapply(cl, chunks, .singularize_chunk)
  }  
  # collapsing in one data.table
  hash_single <- rbindlist(big_list)
  hash_single <- hash_single[ feature != single ]
  
  cli::cli_alert_info("Hashing singular tokens")
  out <- quanteda::tokens_replace(
    x,
    pattern = hash_single$feature,
    replacement = hash_single$single,
    valuetype = "fixed"
  )
  cli::cli_alert_success("Singularization complete")
  return(out)
}

#' @keywords internal
.singularize_chunk <- function(current_chunk) {
  for (ifeat in seq_len(nrow(current_chunk))) {
    data.table::set(
      x = current_chunk,
      i = ifeat,
      j = "single",
      value = .singularize(current_chunk[ifeat])
    )
  }
  current_chunk
}

#' @keywords internal
.singularize = function(row) {
  # This is an internal function that will be pass in the inner for loop the parlance set()
  # Define the function to be passed to `value`
  # Singularize the value of the `feature` column using `singularize()` from `pluralize`
  singularize_fun <- getExportedValue("pluralize", "singularize")
  singular_feature <- singularize_fun(row$feature)
  # Return the singularized feature value
  singular_feature
}

