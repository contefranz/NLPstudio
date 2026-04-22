#' Fast Calculation of Similarity and Distance Measures
#'
#' Compute similarity and distance measures using [textstat_simil] or
#' [textstat_dist], optionally routing additional CPU threads through
#' quanteda's built-in OpenMP backend.
#'
#' @param x A **quanteda** [dfm] object.
#' @param ncores Integer. Number of threads to pass to
#'   [quanteda::quanteda_options()] for the duration of the call.
#'   Defaults to 1 (quanteda's own default). The setting is restored on
#'   exit so it does not affect the caller's global state.
#' @param ... Additional arguments passed to [textstat_simil] or
#'   [textstat_dist].
#'
#' @details
#' Earlier versions of these functions attempted to parallelise by
#' splitting the input [dfm] by rows and computing partial matrices on
#' separate workers. That approach is fundamentally incorrect: each chunk
#' only sees its own documents, so cross-chunk pairs are never evaluated
#' and the merged result is block-diagonal rather than a true full
#' pairwise matrix.
#'
#' The correct parallelism for all-pairs similarity/distance is at the
#' linear-algebra level, which quanteda already implements internally via
#' OpenMP. Setting `ncores > 1` therefore exposes that mechanism rather
#' than introducing an external worker pool.
#'
#' If a second matrix (`y`) is not provided, the output is forced into a
#' symmetric structure using [forceSymmetric] and packed into a
#' [dspMatrix-class] for memory efficiency.  The result is wrapped into
#' the appropriate **quanteda.textstats** S4 class
#' ([textstat_simil-class] or [textstat_dist-class]).
#'
#' @returns A sparse matrix as S4 class following [textstat_simil-class]
#'   or [textstat_dist-class] from the **Matrix** package.
#'
#' @examples
#' dfmat <- quanteda::dfm(quanteda::tokens(c(
#'   "this is a test", "another document",
#'   "more text here", "testing similarity"
#' )))
#'
#' result_simil <- calculate_similarity(dfmat,
#'                                      margin = "documents",
#'                                      method = "cosine")
#'
#' result_dist <- calculate_distance(dfmat,
#'                                   margin = "documents",
#'                                   method = "euclidean")
#'
#' @export

calculate_similarity <- function(x, ncores = 1, ...) {

  if (!quanteda::is.dfm(x)) {
    stop("x must be a quanteda dfm object")
  }
  if (!is.numeric(ncores) || length(ncores) != 1L ||
      ncores < 1L || ncores != as.integer(ncores)) {
    stop("ncores must be a single positive integer")
  }

  cli::cli_h2("Calculating similarity")
  args <- list(...)
  has_y <- "y" %in% names(args)
  if (length(args) < 1L) {
    cli::cli_alert_info("textstat_simil() called with default parameters")
  } else {
    cli::cli_alert_info("textstat_simil() called with the following parameters")
    for (nm in names(args)) cli::cli_alert("{nm} = {toString(args[[nm]])}")
  }

  # Delegate threading to quanteda's OpenMP backend; restore on exit
  old_threads <- quanteda::quanteda_options("threads")
  on.exit(quanteda::quanteda_options(threads = old_threads), add = TRUE)
  quanteda::quanteda_options(threads = ncores)
  cli::cli_alert_info("Using {ncores} thread(s) via quanteda")

  out_measures <- quanteda.textstats::textstat_simil(x, ...)

  if (!has_y) {
    doc_order <- quanteda::docnames(x)
    out_matrix <- methods::as(out_measures, "matrix")[doc_order, doc_order]
    out_matrix <- Matrix::forceSymmetric(out_matrix, uplo = "U")
    temp_matrix <- methods::as(out_matrix, "packedMatrix")
    textstat_obj <- methods::new("textstat_simil_symm",
                        temp_matrix,
                        method = args$method,
                        margin = args$margin,
                        type   = "textstat_simil")
  } else {
    textstat_obj <- out_measures
  }

  cli::cli_alert_success("Done")
  return(textstat_obj)
}

#' @rdname calculate_similarity
#' @export

calculate_distance <- function(x, ncores = 1, ...) {

  if (!quanteda::is.dfm(x)) {
    stop("x must be a quanteda dfm object")
  }
  if (!is.numeric(ncores) || length(ncores) != 1L ||
      ncores < 1L || ncores != as.integer(ncores)) {
    stop("ncores must be a single positive integer")
  }

  cli::cli_h2("Calculating distance")
  args <- list(...)
  has_y <- "y" %in% names(args)
  if (length(args) < 1L) {
    cli::cli_alert_info("textstat_dist() called with default parameters")
  } else {
    cli::cli_alert_info("textstat_dist() called with the following parameters")
    for (nm in names(args)) cli::cli_alert("{nm} = {toString(args[[nm]])}")
  }

  old_threads <- quanteda::quanteda_options("threads")
  on.exit(quanteda::quanteda_options(threads = old_threads), add = TRUE)
  quanteda::quanteda_options(threads = ncores)
  cli::cli_alert_info("Using {ncores} thread(s) via quanteda")

  out_measures <- quanteda.textstats::textstat_dist(x, ...)

  if (!has_y) {
    doc_order <- quanteda::docnames(x)
    out_matrix <- methods::as(out_measures, "matrix")[doc_order, doc_order]
    out_matrix <- Matrix::forceSymmetric(out_matrix, uplo = "U")
    temp_matrix <- methods::as(out_matrix, "packedMatrix")
    textstat_obj <- methods::new("textstat_dist_symm",
                        temp_matrix,
                        method = args$method,
                        margin = args$margin,
                        type   = "textstat_dist")
  } else {
    textstat_obj <- out_measures
  }

  cli::cli_alert_success("Done")
  return(textstat_obj)
}
