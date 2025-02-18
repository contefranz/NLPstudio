#' Fast Corpus Summarization
#'
#' Summarize a [corpus] in parallel via **[future]**.
#'
#' @param x A **quanteda** [corpus] to be summarized.
#' @param ncores The number of [multisession] workers to be allocated for the tokenization.
#' @param ... Additional arguments passed to [dfm].
#'
#' @returns A [data.table] class object with detailed information about each document.
#'
#' @author Francesco Grossetti \email{francesco.grossetti@@unibocconi.it}
#'
#' @importFrom quanteda is.corpus ndoc
#' @importFrom quanteda.textstats textstat_summary
#' @importFrom future plan multisession sequential
#' @importFrom future.apply future_lapply
#' @importFrom cli cli_h2 cli_h3 cli_alert_info cli_alert cli_alert_success cli_alert_danger
#' @export


summarize_corpus = function(x, ncores, ...) {

  if ( !is.corpus(x) ) {
    stop("x must be a quanteda corpus object")
  }

  cli_h2("Summarizing corpus")
  args = list(...)
  if ( length(args) < 1 ) {
    cli_alert_info("quanteda.textstats::textstat_summary() has been called with the default parameters")
  } else {
    cli_alert_info("quanteda.textstats::textstat_summary() has been called with the following parameters")
    args_active = paste0(names(args), " = ", unlist(args))
    for (iarg in seq_along(args_active) ) {
      cli_alert("{args_active[iarg]}")
    }
  }

  # define the number of workers
  plan(multisession, workers = ncores)
  chunks = split(x, rep_len(1L:ncores, ndoc(x)))

  thesummary = do.call(c, future_lapply(chunks, textstat_summary, future.seed = TRUE, ...))
  plan(sequential)


  cli_alert_info("Reshaping to long format using {ncores} cores")
  Nel = length(thesummary)
  bucket_names = names(thesummary)
  out = vector("list", Nel)
  for ( j in seq_len(Nel) ) {
    where = str_which(bucket_names, str_c("^", j, "\\."))
    now = as.data.table(thesummary[where])
    setnames(now, str_remove(names(now), "^\\d+\\."))
    out[[ j ]] = now
  }
  out_all = rbindlist(out, fill = TRUE)
  setnames(out_all, "document", "doc_id")

  cli_alert_success("Summarization complete")
  return(out_all[])
}
