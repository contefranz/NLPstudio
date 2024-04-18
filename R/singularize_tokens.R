#' Build a SEC EDGAR Corpus
#'
#' Build a quanteda corpus from the data.table container.
#'
#' @param df_container A \code{data.table} containing the raw textual documents
#' as built by \code{\link{from_json_to_df}}.
#'
#' @details
#' Even if this function is supposed to be used in conjunction with \code{\link{from_json_to_df}},
#' it can also be used more generally when a \code{data.table} containing some textual documents is
#' available. At the moment, the requirements are that the input object \code{df_container} must have a column
#' \code{"text"} containing the documents and a column \code{"filename"} that contains the document
#' filenames.
#'
#' @returns A quanteda \code{\link[quanteda]{corpus}} object.
#'
#' @author Francesco Grossetti \email{francesco.grossetti@@unibocconi.it}
#'
#' @import data.table
#' @importFrom quanteda is.tokens dfm featnames tokens_replace
#' @importFrom stringr str_remove str_detect str_c
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom iterators iter
#' @importFrom cli cli_h2
#' @export


singularize_tokens = function(x, ncores = 1, ...) {

  # SINGULARIZED TOKENS BY HASHING
  # Note. This function works on the raw object "tokens". It uses the package "pluralize"
  # to singularize individual tokens within the quanteda framework.
  # The code uses hashing to apply singularization rules. The rules are defined via parallel backend
  # This allows the function to be super super fast and efficient.

  if ( !is.tokens(x) ) {
    stop("x must be a quanteda tokens object")
  }

  message("Building the DFM...")
  xdfm = dfm(x)
  message("Extracting the vocabulary and cleaning it...")
  vocabulary = sort(featnames(xdfm))
  # Remove numbers or elements that contain any number because singularize plays dumb.
  # If it finds the string "000s" it will convert it to "000". This
  vocabulary = vocabulary[ !str_detect(vocabulary, "\\d")]
  # Remove tokens that contain less than 4 characters
  vocabulary = vocabulary[ str_which(vocabulary, "\\w{3,}") ]

  message("Defining the singularized tokens...")
  # Define the hash table for the vocabulary in which I already pre-allocate the new column
  hash_vocabulary = data.table(feature = vocabulary, single = "")
  chunks = split(hash_vocabulary, rep_len(1L:ncores, nrow(hash_vocabulary)))
  Nchunks = length(chunks)
  it = iter(seq_len(Nchunks), by = "row")
  cl = makeCluster(ncores)
  registerDoParallel(cl)
  big_list = foreach(
    ichunk = it,
    .packages = c("iterators", "data.table", "quanteda", "stringr", "pluralize"),
    .export = ".singularize"
  ) %dopar% {

    current_chunk = chunks[[ichunk]]

    for ( ifeat in seq_len(nrow(current_chunk)) ) {
      set(x = current_chunk, i = ifeat, j = "single", value = .singularize(current_chunk[ifeat]))
    }
    current_chunk

  }
  stopCluster(cl)
  hash_single = rbindlist(big_list)
  hash_single = hash_single[ feature != single ]

  message("Singularizing tokens by hashing...")
  out = tokens_replace(x,
                       pattern = hash_single$feature,
                       replacement = hash_single$single,
                       valuetype = "fixed")
  return(out)
}


#' @keywords internal
.singularize = function(row) {
  # This is an internal function that will be pass in the inner for loop the parlance set()
  # Define the function to be passed to `value`
  # Singularize the value of the `feature` column using `singularize()` from `pluralize`
  singular_feature = singularize(row$feature)
  # Return the singularized feature value
  singular_feature
}

