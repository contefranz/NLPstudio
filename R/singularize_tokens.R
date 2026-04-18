if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("feature"))
}
#' Fast Tokens Singularization
#'
#' Singularize tokens from a **quanteda** [tokens] object using a parallel
#' hashing strategy. Internally relies on [pluralize::singularize()]. Short
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
#' @return A [quanteda::tokens] object with singularized tokens.
#'
#' @note Requires the **pluralize** package. On Linux/macOS, `"FORK"` may be
#' faster but can be unstable with quanteda’s C++/OpenMP internals. Use `"PSOCK"`
#' for maximum stability. On Windows, `"FORK"` is not available.
#' 
#'
#' @import data.table
#' @export
singularize_tokens <- function(x, ncores = 1, nchunks = ncores,
                               socket = c("PSOCK", "FORK"),
                               remove_numbers = TRUE, min_char = 1) {
  if (!requireNamespace("pluralize", quietly = TRUE)) {
    stop("Package 'pluralize' is required for singularize_tokens(). Please install it.", call. = FALSE)
  }
  if (!quanteda::is.tokens(x)) stop("x must be a quanteda tokens object")
  
  socket <- match.arg(socket)
  .validate_parallel_args(ncores, nchunks)

  cli::cli_h2("Singularizing tokens")
  cli::cli_alert_info("Building DFM and extracting vocabulary")
  
  xdfm <- quanteda::dfm(x)
  vocabulary <- sort(quanteda::featnames(xdfm))
  
  if (remove_numbers) {
    cli::cli_alert_info("Removing tokens containing any number")
    vocabulary <- vocabulary[!stringr::str_detect(vocabulary, "\\d")]
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
                              export_vars = c(".singularize_chunk", ".singularize"),
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

#' @keywords internal
.singularize_chunk <- function(current_chunk) {
  singularize_fun <- getExportedValue("pluralize", "singularize")
  current_chunk[, single := singularize_fun(feature)]
  current_chunk
}

#' @keywords internal
.singularize <- function(row) {
  singularize_fun <- getExportedValue("pluralize", "singularize")
  singularize_fun(row$feature)
}