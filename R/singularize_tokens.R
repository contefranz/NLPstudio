if ( getRversion() >= "2.15.1" ) {
  utils::globalVariables( c("ichunk", "feature") )
}
#' Fast Tokens Singularization
#'
#' Singularize tokens from a **quanteda** [tokens] object using parallel hashing.
#' Internally relies on the [singularize()] function from the **pluralize** package.
#'
#' @param x A [tokens][quanteda::tokens] object containing tokenized text.
#' @param ncores Integer. Number of CPU cores to use for parallel processing
#'   via a `PSOCK` cluster (see [parallel::makeCluster()]). Defaults to 1.
#'   On small datasets, parallelization may add overhead without speed gains.
#' @param remove_numbers Logical. If `TRUE` (default), removes tokens that
#'   contain any digits. This avoids producing incorrect singular forms
#'   for numeric tokens (e.g., `"000s"` to `"000"`).
#' @param min_char Integer. Minimum number of characters a token must have
#'   to be retained. Defaults to 1, meaning no tokens are removed based
#'   on length.
#'   
#' @details
#' Traditional singularization functions operate on character vectors,
#' which is inefficient for large vocabularies. In contrast,
#' `singularize_tokens()` works directly on a [tokens] object,
#' taking advantage of its efficient internal representation.
#'
#' The function first extracts the vocabulary by converting the tokens
#' to a [dfm][quanteda::dfm], then builds a hash table of tokens and
#' their singularized forms. The actual singularization is performed
#' in parallel using [pluralize::singularize()] across multiple cores
#' (via [foreach], [doParallel], and [parallel]).
#'
#' Finally, the hash table is applied back to the original tokens with
#' [quanteda::tokens_replace()], producing a singularized token stream.
#' This approach is conceptually similar to lemmatization.
#'
#' The main computational cost is the initial `dfm` conversion, which
#' is unavoidable.
#' 
#' @return A [tokens][quanteda::tokens] object with singularized tokens.
#' @note The function requires the **pluralize** package, which should be
#'   installed separately if not already available.
#'   
#' @author Francesco Grossetti \email{francesco.grossetti@@unibocconi.it}
#'
#' @import data.table
#' @importFrom quanteda is.tokens dfm featnames tokens_replace
#' @importFrom stringr str_remove str_detect str_c regex
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom iterators iter
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
  xdfm = dfm(x)
  vocabulary = sort(featnames(xdfm))
  if ( remove_numbers ) {
    cli_alert_info("Removing tokens containing any number")
    # Remove numbers or elements that contain any number because singularize plays dumb.
    # If it finds the string "000s" it will convert it to "000". This
    vocabulary = vocabulary[ !str_detect(vocabulary, "\\d")]
  }
  if ( min_char > 1 ) {
    cli_alert_info("Keeping tokens with at least {min_char} characters")
    # Remove tokens that contain less than min_char characters
    regex_min_char = regex(str_c("\\w{", min_char, ",}"), ignore_case = TRUE)
    vocabulary = vocabulary[ str_which(vocabulary, regex_min_char) ]
  } else {
    cli_alert_warning("You set min_char = {min_char} --> All tokens will be kept!")
  }
  
  cli_alert_info("Defining the singular tokens hash table")
  # Define the hash table for the vocabulary in which I already pre-allocate the new column
  hash_vocabulary = data.table(feature = vocabulary, single = "")
  chunks = split(hash_vocabulary, rep_len(1L:ncores, nrow(hash_vocabulary)))
  Nchunks = length(chunks)
  it = iter(seq_len(Nchunks), by = "row")
  cl = makeCluster(ncores)
  registerDoParallel(cl)
  
  big_list = foreach(
    ichunk = it,
    .packages = c("iterators", "data.table", "quanteda", "pluralize"),
    .export = ".singularize"
  ) %dopar% {
    
    current_chunk = chunks[[ichunk]]
    for ( ifeat in seq_len(nrow(current_chunk)) ) {
      set(x = current_chunk, i = ifeat, j = "single", value = .singularize(current_chunk[ifeat]))
    }
    current_chunk
  }
  
  # collapsing in one data.table
  hash_single = rbindlist(big_list)
  hash_single = hash_single[ feature != single ]
  
  cli_alert_info("Hashing singular tokens")
  out = tokens_replace(x,
                       pattern = hash_single$feature,
                       replacement = hash_single$single,
                       valuetype = "fixed")
  cli_alert_success("Singularization complete")
  on.exit(stopCluster(cl))
  return(out)
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

