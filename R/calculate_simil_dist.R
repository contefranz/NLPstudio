#' Fast Calculation of Similarity and Distance Measures
#'
#' Compute similarity and distance measures in parallel with the **[future]** paradigm.
#'
#' @param x A **quanteda** [corpus] or a character vector containing the documents to process.
#' @param ncores The number of [multisession] workers to be allocated for
#' the calculation of readability.
#' @param ... Additional arguments passed to [textstat_simil] or [textstat_dist].
#' 
#' @details
#' These functions leverage parallel computing via the **[future]** framework to efficiently compute 
#' similarity and distance measures across documents. By splitting the input [dfm] into balanced 
#' chunks, the computation is distributed across multiple CPU cores using [future_lapply]. 
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
#' @author Francesco Grossetti \email{francesco.grossetti@@unibocconi.it}
#' 
#' @examples
#' \dontrun{
#' 
#' # Create a sample dfm
#' dfmat <- dfm(c("this is a test", "another document", "more text here", "testing similarity"))
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
#' @importFrom quanteda is.dfm as.dfm
#' @importFrom quanteda.textstats textstat_simil textstat_dist
#' @importFrom Matrix forceSymmetric 
#' @importFrom methods as new
#' @importFrom utils getFromNamespace
#' @importFrom stringr str_c str_which str_remove
#' @importFrom future plan multisession sequential
#' @importFrom future.apply future_lapply
#' @importFrom cli cli_h2 cli_alert_info cli_alert cli_alert_success
#' @export

calculate_similarity = function(x, ncores, ...) {
  
  if ( !is.dfm(x) ) {
    stop("x must be a quanteda dfm object")
  }
  
  cli_h2("Calculating similarity")
  args = list(...)
  # check if y is defined
  has_y = "y" %in% names(args)
  if ( length(args) < 1 ) {
    cli_alert_info("quanteda.textstats::textstat_simil() has been called with the default parameters")
  } else {
    cli_alert_info("quanteda.textstats::textstat_simil() has been called with the following parameters")
    for (nm in names(args)) {
      cli_alert("{nm} = {toString(args[[nm]])}")
    }
  }
  
  cli_alert_info("Using {ncores} cores for computations")
  # define the number of workers
  plan(multisession, workers = ncores)
  # Since dfm objects in quanteda behave similarly to matrices, subsetting must use row indices. 
  # I explicitly use docnames(x) for a safer split. 
  # Also, The function remains robust even when ndoc(x) < ncores.  
  doc_groups = rep_len(1L:ncores, ndoc(x))
  doc_ids = split(docnames(x), doc_groups)  # Split docnames instead of dfm directly
  chunks = lapply(doc_ids, function(ids) x[ids, ])
  computation_list = do.call(c, future_lapply(chunks, textstat_simil, future.seed = TRUE, ...))
  plan(sequential)
  
  cli_alert_info("Merging results into a single object")
  computation_list_dfm = lapply(computation_list, as.dfm)
  # Since quanteda:::rbind.dfm() is an internal function, 
  # the best practice is to retrieve it dynamically using getFromNamespace()
  rbind_dfm = getFromNamespace("rbind.dfm", "quanteda")
  out_measures = do.call(rbind_dfm, computation_list_dfm)
  
  # Ensure ordering matches the original dfm
  doc_order <- docnames(x)
  out_measures <- out_measures[doc_order, doc_order]
  
  # Check whether a second matrix is passed and force symmetry 
  if ( !has_y ) {
    # Ensure symmetry
    temp_matrix = forceSymmetric(out_measures, uplo = "U")
  } 
  
  # Convert to a packed symmetric matrix (dspMatrix) for memory efficiency
  temp_matrix = as(temp_matrix, "packedMatrix")
  
  # Wrap into the correct S4 class
  textstat_obj = new("textstat_simil_symm",
                     temp_matrix,
                     method = args$method,
                     margin = args$margin,
                     type = "textstat_simil")
  
  cli_alert_success("Done")
  # this returns the original textstat_simil_symm class
  return(textstat_obj)
}

#' @rdname calculate_similarity 
#' @export

calculate_distance = function(x, ncores, ...) {
  
  if ( !is.dfm(x) ) {
    stop("x must be a quanteda dfm object")
  }
  
  cli_h2("Calculating distance")
  args = list(...)
  # check if y is defined
  has_y = "y" %in% names(args)
  if ( length(args) < 1 ) {
    cli_alert_info("quanteda.textstats::textstat_dist() has been called with the default parameters")
  } else {
    cli_alert_info("quanteda.textstats::textstat_dist() has been called with the following parameters")
    for (nm in names(args)) {
      cli_alert("{nm} = {toString(args[[nm]])}")
    }
  }
  
  cli_alert_info("Using {ncores} cores for computations")
  # define the number of workers
  plan(multisession, workers = ncores)
  # Since dfm objects in quanteda behave similarly to matrices, subsetting must use row indices. 
  # I explicitly use docnames(x) for a safer split. 
  # Also, The function remains robust even when ndoc(x) < ncores.  
  doc_groups = rep_len(1L:ncores, ndoc(x))
  doc_ids = split(docnames(x), doc_groups)  # Split docnames instead of dfm directly
  chunks = lapply(doc_ids, function(ids) x[ids, ])
  computation_list = do.call(c, future_lapply(chunks, textstat_dist, future.seed = TRUE, ...))
  plan(sequential)
  
  cli_alert_info("Merging results into a single object")
  computation_list_dfm = lapply(computation_list, as.dfm)
  # Since quanteda:::rbind.dfm() is an internal function, 
  # the best practice is to retrieve it dynamically using getFromNamespace()
  rbind_dfm = getFromNamespace("rbind.dfm", "quanteda")
  out_measures = do.call(rbind_dfm, computation_list_dfm)
  
  # Ensure ordering matches the original dfm
  doc_order <- docnames(x)
  out_measures <- out_measures[doc_order, doc_order]
  
  # Check whether a second matrix is passed and force symmetry 
  if ( !has_y ) {
    # Ensure symmetry
    temp_matrix = forceSymmetric(out_measures, uplo = "U")
  } 
  
  # Convert to a packed symmetric matrix (dspMatrix) for memory efficiency
  temp_matrix = as(temp_matrix, "packedMatrix")
  
  # Wrap into the correct S4 class
  textstat_obj = new("textstat_dist_symm",
                     temp_matrix,
                     method = args$method,
                     margin = args$margin,
                     type = "textstat_dist")
  
  cli_alert_success("Done")
  # this returns the original textstat_simil_symm class
  return(textstat_obj)
}

