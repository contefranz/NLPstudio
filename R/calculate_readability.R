#' Fast Calculation of Readability Measures
#'
#' Compute readability measures in parallel with the **[future]** paradigm.
#'
#' @param x A **quanteda** [corpus] or a character vector containing the documents to process.
#' @param ncores The number of [multisession] workers to be allocated for
#' the calculation of readability.
#' @param ... Additional arguments passed to [textstat_readability].
#'
#' @returns A [data.table] with as many columns as passed to
#' `measure` and `doc_id` as the document identifier.
#'
#' @author Francesco Grossetti \email{francesco.grossetti@@unibocconi.it}
#'
#' @import data.table
#' @importFrom quanteda.textstats textstat_readability
#' @importFrom stringr str_c str_which str_remove
#' @importFrom future plan multisession sequential
#' @importFrom future.apply future_lapply
#' @importFrom cli cli_h2 cli_alert_info cli_alert cli_alert_success
#' @export


calculate_readability = function(x, ncores, ...) {
  
  if ( !is.corpus(x) || !is.character(x) ) {
    stop("x must be a quanteda corpus object or a character vector containing the documents")
  }
  
  cli_h2("Calculating readability")
  args = list(...)
  if ( length(args) < 1 ) {
    cli_alert_info("quanteda.textstats::textstat_readability() has been called with the default parameters")
  } else {
    cli_alert_info("quanteda.textstats::textstat_readability() has been called with the following parameters")
    # args_active = paste0(names(args), " = ", unlist(args))
    for (nm in names(args)) {
      val = toString(args[[nm]])
      cli_alert_info("{nm} = {val}")
    }
  }
  
  # define the number of workers
  plan(multisession, workers = ncores)
  chunks = split(x, rep_len(1L:ncores, ndoc(x)))
  readability_measures = do.call(c, future_lapply(chunks, textstat_readability,
                                                  future.seed = TRUE, ...))
  plan(sequential)
  
  Nel = length(readability_measures)
  bucket_names = names(readability_measures)
  out = vector("list", Nel)
  for ( j in seq_len(Nel) ) {
    where = str_which(bucket_names, str_c("^", j, "\\."))
    now = as.data.table(readability_measures[where])
    setnames(now, str_remove(names(now), "^\\d+\\."))
    out[[ j ]] = now
  }
  out_all = rbindlist(out, fill = TRUE)
  setnames(out_all, "document", "doc_id")
  cli_alert_success("Done")
  return(out_all[])
}
