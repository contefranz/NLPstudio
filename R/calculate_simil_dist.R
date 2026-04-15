#' Fast Calculation of Similarity and Distance Measures
#'
#' Compute similarity and distance measures in parallel using backends from the
#' **parallel** package. By default, computation is parallelized with PSOCK
#' clusters ([parallel::clusterApplyLB()]) for cross-platform stability.
#' Optionally, FORK-based parallelism ([parallel::mclapply()]) may be
#' requested on Linux/macOS.
#'
#' @inheritParams tokenize_corpus
#' @param x A **quanteda** [dfm] object.
#' @param ... Additional arguments passed to [textstat_simil] or [textstat_dist].
#'
#' @details
#' These functions leverage parallel computing via the **parallel** package to efficiently compute
#' similarity and distance measures across documents. By splitting the input [dfm] into balanced
#' chunks, the computation is distributed across multiple CPU cores.
#' This ensures scalability even when handling large corpora.
#'
#' Once the computations are complete, the individual results are internally merged back into a single
#' sparse matrix. If a second matrix (`y`) is not provided, the output
#' is forced into a symmetric structure using [forceSymmetric], ensuring consistency with the
#' default behavior of [textstat_simil] and [textstat_dist].
#'
#' For memory efficiency, the final similarity or distance matrix is converted into a packed
#' symmetric sparse format ([dspMatrix-class]), which significantly reduces storage requirements while
#' maintaining computational speed. Finally, the results are wrapped into the appropriate
#' **[quanteda.textstats]** S4 class ([textstat_simil-class] or [textstat_dist-class]), ensuring full
#' compatibility with downstream quanteda functions.
#'
#'
#' @returns A sparse matrix as S4 class following [textstat_simil-class] or [textstat_dist-class]
#' from the **Matrix** package.
#'
#' @examples
#' \dontrun{
#'
#' # Create a sample dfm
#' dfmat <- dfm(tokens(c("this is a test", "another document", "more text here", "testing similarity")))
#'
#' # Compute cosine similarity in parallel using 2 cores
#' result_simil <- calculate_similarity(dfmat, ncores = 2, margin = "documents", method = "cosine")
#'
#' # Compute euclidean distance in parallel using 2 cores
#' result_dist <- calculate_distance(dfmat, ncores = 2, margin = "documents", method = "euclidean")
#'
#' }
#'
#' @import data.table
#' @importFrom quanteda is.dfm as.dfm ndoc docnames
#' @importFrom quanteda.textstats textstat_simil textstat_dist
#' @importFrom Matrix forceSymmetric
#' @importFrom methods as new
#' @importFrom utils getFromNamespace
#' @importFrom parallel makeCluster stopCluster clusterApplyLB clusterExport mclapply
#' @importFrom cli cli_h2 cli_alert_info cli_alert cli_alert_success
#' @export

calculate_similarity = function(x, ncores = 1, nchunks = ncores, socket = c("PSOCK", "FORK"), ...) {

  if ( !is.dfm(x) ) {
    stop("x must be a quanteda dfm object")
  }

  socket <- match.arg(socket)
  .validate_parallel_args(ncores, nchunks)

  cli_h2("Calculating similarity")
  args = list(...)
  has_y = "y" %in% names(args)
  if ( length(args) < 1 ) {
    cli_alert_info("quanteda.textstats::textstat_simil() has been called with the default parameters")
  } else {
    cli_alert_info("quanteda.textstats::textstat_simil() has been called with the following parameters")
    for (nm in names(args)) {
      cli_alert("{nm} = {toString(args[[nm]])}")
    }
  }

  # Split dfm into balanced chunks by doc IDs
  doc_ids <- docnames(x)
  groups  <- split(doc_ids, rep_len(seq_len(max(1L, nchunks)), length(doc_ids)))
  chunks  <- lapply(groups, function(ids) if (length(ids)) x[ids, ] else NULL)
  chunks  <- Filter(Negate(is.null), chunks)

  if (length(chunks) <= 1L || ncores < 2L) {
    cli_alert_info("Computing similarity sequentially")
    computation_list <- list(textstat_simil(x, ...))
  } else {
    cli_alert_info("Computing similarity for {length(chunks)} chunks in parallel with {ncores} cores via {socket}")
    computation_list <- .run_parallel(chunks, .simil_chunk, ncores, socket,
                                      export_vars = c(".simil_chunk"),
                                      export_env = environment(), ...)
  }

  cli_alert_info("Merging results into a single object")
  computation_list_flat <- do.call(c, computation_list)
  computation_list_dfm = lapply(computation_list_flat, as.dfm)
  rbind_dfm = getFromNamespace("rbind.dfm", "quanteda")
  out_measures = do.call(rbind_dfm, computation_list_dfm)

  # Check whether a second matrix is passed and force symmetry
  if ( !has_y ) {
    doc_order <- docnames(x)
    out_measures <- out_measures[doc_order, doc_order]
    temp_matrix = forceSymmetric(out_measures, uplo = "U")
    temp_matrix = as(temp_matrix, "packedMatrix")
    textstat_obj = new("textstat_simil_symm",
                       temp_matrix,
                       method = args$method,
                       margin = args$margin,
                       type = "textstat_simil")
  } else {
    temp_matrix = out_measures
    textstat_obj = new("textstat_simil_symm",
                       temp_matrix,
                       method = args$method,
                       margin = args$margin,
                       type = "textstat_simil")
  }

  cli_alert_success("Done")
  return(textstat_obj)
}

#' @rdname calculate_similarity
#' @export

calculate_distance = function(x, ncores = 1, nchunks = ncores, socket = c("PSOCK", "FORK"), ...) {

  if ( !is.dfm(x) ) {
    stop("x must be a quanteda dfm object")
  }

  socket <- match.arg(socket)
  .validate_parallel_args(ncores, nchunks)

  cli_h2("Calculating distance")
  args = list(...)
  has_y = "y" %in% names(args)
  if ( length(args) < 1 ) {
    cli_alert_info("quanteda.textstats::textstat_dist() has been called with the default parameters")
  } else {
    cli_alert_info("quanteda.textstats::textstat_dist() has been called with the following parameters")
    for (nm in names(args)) {
      cli_alert("{nm} = {toString(args[[nm]])}")
    }
  }

  # Split dfm into balanced chunks by doc IDs
  doc_ids <- docnames(x)
  groups  <- split(doc_ids, rep_len(seq_len(max(1L, nchunks)), length(doc_ids)))
  chunks  <- lapply(groups, function(ids) if (length(ids)) x[ids, ] else NULL)
  chunks  <- Filter(Negate(is.null), chunks)

  if (length(chunks) <= 1L || ncores < 2L) {
    cli_alert_info("Computing distance sequentially")
    computation_list <- list(textstat_dist(x, ...))
  } else {
    cli_alert_info("Computing distance for {length(chunks)} chunks in parallel with {ncores} cores via {socket}")
    computation_list <- .run_parallel(chunks, .dist_chunk, ncores, socket,
                                      export_vars = c(".dist_chunk"),
                                      export_env = environment(), ...)
  }

  cli_alert_info("Merging results into a single object")
  computation_list_flat <- do.call(c, computation_list)
  computation_list_dfm = lapply(computation_list_flat, as.dfm)
  rbind_dfm = getFromNamespace("rbind.dfm", "quanteda")
  out_measures = do.call(rbind_dfm, computation_list_dfm)

  # Check whether a second matrix is passed and force symmetry
  if ( !has_y ) {
    doc_order <- docnames(x)
    out_measures <- out_measures[doc_order, doc_order]
    temp_matrix = forceSymmetric(out_measures, uplo = "U")
    temp_matrix = as(temp_matrix, "packedMatrix")
    textstat_obj = new("textstat_dist_symm",
                       temp_matrix,
                       method = args$method,
                       margin = args$margin,
                       type = "textstat_dist")
  } else {
    temp_matrix = out_measures
    textstat_obj = new("textstat_dist_symm",
                       temp_matrix,
                       method = args$method,
                       margin = args$margin,
                       type = "textstat_dist")
  }

  cli_alert_success("Done")
  return(textstat_obj)
}

#' @keywords internal
.simil_chunk <- function(dfm_chunk, ...) {
  quanteda.textstats::textstat_simil(dfm_chunk, ...)
}

#' @keywords internal
.dist_chunk <- function(dfm_chunk, ...) {
  quanteda.textstats::textstat_dist(dfm_chunk, ...)
}
