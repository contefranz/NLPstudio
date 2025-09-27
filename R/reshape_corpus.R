#' Fast Corpus Reshape
#'
#' Reshape a [corpus] in parallel under the **[future]** paradigm.
#'
#' @param x A **quanteda** [corpus].
#' @param ncores The number of [multisession] workers to be allocated for the reshaping.
#' @param to Character; type of reshaping. Default is `"sentences"`. Passed to [corpus_reshape()].
#' @param ... Additional arguments passed to [corpus_reshape()] (see 'Details').
#'
#' @details
#' This function wraps [quanteda::corpus_reshape()] but executes in parallel using the
#' [future] framework. The most common use is reshaping to sentences (`to = "sentences"`),
#' which enables sentence-level text analysis and alignment with document-level metadata.
#'
#' Document order is guaranteed to match the input `corpus`. After reshaping,
#' documents are reordered to ensure consistency with the original corpus ordering.
#'
#' Use `...` to further control the reshaping process. For instance, one can respahe the corpus to
#' paragraphs by passing `to = "paragraphs"`.
#'
#' @returns A reshaped [corpus] object.
#'
#' @author Francesco Grossetti \email{francesco.grossetti@@unibocconi.it}
#'
#' @importFrom quanteda is.corpus corpus_reshape ndoc
#' @importFrom future plan multisession sequential
#' @importFrom stringr str_remove
#' @importFrom future.apply future_lapply
#' @importFrom cli cli_h2 cli_alert_info cli_alert cli_alert_success cli_alert_danger
#' @export


reshape_corpus = function(x, ncores, to = "sentences", ...) {

  if ( !is.corpus(x) ) {
    stop("x must be a quanteda corpus object")
  }
  
  cli_h2("Reshaping corpus")
  args = list(...)
  if ( length(args) < 1 ) {
    cli_alert_info("quanteda::corpus_reshape() has been called with the default parameters")
  } else {
    cli_alert_info("quanteda::corpus_reshape() has been called with the following parameters")
    args_active = paste0(names(args), " = ", unlist(args))
    for (nm in names(args)) {
      cli_alert("{nm} = {toString(args[[nm]])}")
    }
  }

  # define the number of workers
  plan(multisession, workers = ncores)
  chunks = split(x, rep_len(1L:ncores, ndoc(x)))

  reshaped <- do.call(c, future_lapply(
    chunks, corpus_reshape, to = to, future.seed = TRUE, ...
  ))
  plan(sequential)
  
  # enforce original document ordering
  orig_docs <- docnames(x)
  reshaped <- reshaped[order(match(
    str_remove(docnames(reshaped), "\\.\\d+$"), orig_docs
  ))]

  doc_names = sort(docnames(x))
  doc_names_reshaped = sort(unique(str_remove(docnames(reshaped), "\\.\\d+")))
  if ( all(doc_names == doc_names_reshaped) ) {
    cli_alert_success("Corpus x has been successfully reshaped")
  } else {
    cli_alert_danger("Reshaping failed")
    stop("Likely failed to process some sentences leading to dropping a document")
  }

  return(reshaped)
}
