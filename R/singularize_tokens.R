if ( getRversion() >= "2.15.1" ) {
  utils::globalVariables( c("ichunk", "feature") )
}
#' Fast Tokens Singularization
#'
#' Singularize a **quanteda** [tokens] class object via parallel hashing using 
#' the [singularize()] function of **pluralize**. 
#'
#' @param x A **quanteda** [tokens] class object.
#' @param ncores Number of cores to dispatch a `PSOCK` cluster for parallel processing. 
#' See [makeCluster()] for more information.
#' @param remove_numbers Logical. Whether to remove any token containing a number of the regex class
#' `\d`. 
#' @param min_char Numeric specifying the minimum length in characters for tokens to be kept. 
#' Default to 1 no removal. 
#'
#' @details
#' By construction, [singularize()] operates on vectors of characters which is not particularly
#' efficient when the size of the vocabulary is large. Conversely, a [tokens] class object is much
#' more efficient as by default it is serialized. To avoid the usage of loops over a potentially 
#' large [tokens] object, `singularize_tokens` creates a hash table as a [data.table] class object 
#' containing the vocabulary set as identified by transforming the [tokens] object 
#' to a [dfm]. 
#' 
#' The singularization via [singularize()] can now be executed in parallel and 
#' and in-place over the `data.table` object via the efficient function [set()]. This effectively
#' adds the singularized form to the hash table which can then be used to replace the original 
#' tokens with a very efficient lookup via [tokens_replace()]. This last step resembles that of
#' lemmatizing. 
#' 
#' The real bottleneck is probably the initial creation of the vocabulary set as a call to [dfm()] 
#' is inevitable. 
#'
#' @returns A **quanteda** [tokens] class object with singularized tokens. 
#'
#' @author Francesco Grossetti \email{francesco.grossetti@@unibocconi.it}
#'
#' @import data.table
#' @importFrom quanteda is.tokens dfm featnames tokens_replace
#' @importFrom pluralize singularize
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
    # Remove tokens that contain less than 4 characters
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
  # stopCluster(cl)
  
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
  singular_feature = pluralize::singularize(row$feature)
  # Return the singularized feature value
  singular_feature
}

