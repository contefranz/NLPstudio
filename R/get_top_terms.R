if ( getRversion() >= "2.15.1" ) {
  utils::globalVariables( c( "probability", "..terms") )
}
#' Extract Topic-Word Probabilities and Terms
#'
#' This function extracts the top `n` most probable terms from each topic in the topic-word
#' distribution matrix (`phi`). It supports both long and wide output
#' formats, making it suitable for downstream tasks such as inspection, visualization, or export.
#'
#' @inheritParams plot_dtw 
#' @param n Integer. Number of top terms to extract per topic. Default is 10.
#' @param topics Optional numeric vector specifying which topics to return. If `NULL` (default), all topics are included.
#' @param format Output format. Either `"long"` (default) for a tidy table, or `"wide"` for a 
#' spreadsheet-like layout with separate columns for each topic's top terms and probabilities.
#' @returns A `data.table` in either:
#'
#' - **Long format**: One row per (topic, term) pair, with columns:
#'     - `rank`: The rank of the term within its topic.
#'     - `topic`: The topic number.
#'     - `term`: The term (column name from `phi`).
#'     - `probability`: The estimated probability of the term in the topic.
#'
#' - **Wide format**: One row per rank, with two columns per topic:
#'     - `topic_k-th_term`: The term at rank `i` for `k-th` topic (zero-padded).
#'     - `topic_k-th_prob`: The corresponding probability value.
#'
#' @details
#' The function reshapes the `phi` matrix to long format and ranks the terms within each topic 
#' by their estimated probability. This is useful for interpreting topics, displaying 
#' top words, or exporting for labeling. Internally, the function uses `data.table` for fast 
#' grouping and sorting. The `probability` column represents the raw conditional probability of observing
#' a word in a given topic (i.e., \eqn{\mathcal{P}(w \mid \phi)})
#'
#' @seealso [warp_lda()], [plot_top_terms()] [plot_dtw()] [LDA()][topicmodels::LDA]
#'
#' @import data.table
#' @export
#' 
#' 

get_top_terms <- function(x, n = 10, topics = NULL, format = c("long", "wide")) {
  format <- match.arg(format)
  if (!is.numeric(n) || length(n) != 1L || n < 1) stop("n must be a positive integer")
  
  # --- Extract phi as matrix + term names ---
  if (is.list(x) && inherits(x$lda_object, "WarpLDA")) {
    phi_mat <- as.matrix(x$phi)
    term_names <- colnames(phi_mat)
  } else if (!is.list(x) && (inherits(x, "VEM") || inherits(x, "Gibbs"))) {
    phi_mat <- exp(x@beta)
    term_names <- x@terms
  } else if (inherits(x, "textmodel_lda")) {
    phi_mat <- as.matrix(x$phi)
    term_names <- colnames(phi_mat)
  } else {
    stop("x is an unrecognized object")
  }
  
  if (is.null(term_names)) term_names <- as.character(seq_len(ncol(phi_mat)))
  
  # Topics: rows of phi
  K <- nrow(phi_mat)
  if (is.null(topics)) {
    topic_ids <- seq_len(K)
  } else {
    if (!is.numeric(topics) || anyNA(topics)) stop("topics must be numeric")
    if (!all(topics %in% seq_len(K))) stop("Some specified topics are not available in phi.")
    topic_ids <- as.integer(topics)
  }
  
  pad_width <- nchar(as.character(max(topic_ids)))
  
  # Helper for top-n indices
  .top_idx <- function(v, nn) utils::head(order(v, decreasing = TRUE), nn)
  
  # -------- LONG --------
  if (format == "long") {
    out <- data.table::rbindlist(lapply(topic_ids, function(k) {
      probs <- phi_mat[k, ]
      nn <- min(n, length(probs))
      idx <- .top_idx(probs, nn)
      data.table::data.table(
        rank = seq_len(nn),
        topic = k,
        term = term_names[idx],
        probability = as.numeric(probs[idx])
      )
    }))
    data.table::setcolorder(out, c("rank", "topic", "term", "probability"))
    return(out[])
  }
  
  # -------- WIDE --------
  wide_list <- lapply(topic_ids, function(k) {
    probs <- phi_mat[k, ]
    nn <- min(n, length(probs))
    idx <- .top_idx(probs, nn)
    data.table::data.table(
      rank = seq_len(nn),
      term = term_names[idx],
      probability = as.numeric(probs[idx])
    )
  })
  
  for (i in seq_along(wide_list)) {
    topic_number <- topic_ids[i]
    padded_id <- stringr::str_pad(topic_number, width = pad_width, pad = "0")
    data.table::setnames(
      wide_list[[i]],
      old = c("term", "probability"),
      new = c(paste0("topic_", padded_id, "_term"),
              paste0("topic_", padded_id, "_prob"))
    )
  }
  
  wide_out <- Reduce(function(a, b) merge(a, b, by = "rank", all = TRUE), wide_list)
  data.table::setkey(wide_out, NULL)
  wide_out
}
