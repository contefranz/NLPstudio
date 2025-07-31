if ( getRversion() >= "2.15.1" ) {
  utils::globalVariables( c( "probability", "..terms") )
}
#' Extract Topic-Word Probabilities and Terms
#'
#' This function extracts the top `n` most probable terms from each topic in the topic-word
#' distribution matrix (`phi`) returned by [warpLDA()]. It supports both long and wide output
#' formats, making it suitable for downstream tasks such as inspection, visualization, or export.
#'
#' @param phi A `data.table` representing the topic-word distribution matrix ( \eqn{\phi} ), typically from 
#' `model$phi` after running [warpLDA()]. Each row corresponds to a topic, and each column to a term.
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
#' @seealso [warpLDA()], [plot_top_terms()] [plot_dtw()]
#'
#' @import data.table
#' @importFrom utils head
#' @export
#' 
#' 

get_top_terms <- function(phi, n = 10, topics = NULL, format = c("long", "wide")) {
  
  format <- match.arg(format)
  
  if (!is.data.table(phi)) stop("phi must be a data.table containing topic-word probabilities")
  if (!is.numeric(n)) stop("n must be numeric")
  
  phi[, topic := .I]
  
  
  # Optional topic filter
  if (!is.null(topics)) {
    if (!all(topics %in% phi$topic)) {
      stop("Some specified topics are not available in phi.")
    }
    phi <- phi[topic %in% topics]
    # Determine padding width based on max topic index
    n_topics <- nrow(phi)
  }
  
  # actual topic numbers
  topic_ids = phi$topic
  pad_width <- nchar(as.character(max(topic_ids)))
  
  # -------------------- LONG FORMAT --------------------
  if (format == "long") {
    phi_long <- melt(phi,
                     id.vars = "topic",
                     variable.name = "term",
                     value.name = "probability",
                     variable.factor = FALSE)
    
    top_terms <- phi_long[order(topic, -probability), head(.SD, n), by = topic]
    top_terms[, rank := seq_len(.N), by = topic]
    setcolorder(top_terms, c("rank", "topic", "term", "probability"))
    setkey(top_terms, NULL)
    return(top_terms[])
  }
  
  # -------------------- WIDE FORMAT --------------------
  # Store original term names
  terms <- names(phi)[!names(phi) %in% "topic"]
  
  # Pre-extract matrix of probabilities
  prob_matrix <- as.matrix(phi[, ..terms])
  
  # Build list of per-topic tables
  wide_list <- lapply(seq_along(topic_ids), function(i) {
    probs <- prob_matrix[i, ]
    top_idx <- order(probs, decreasing = TRUE)[seq_len(n)]
    data.table(
      rank = seq_len(n),
      term = terms[top_idx],
      probability = probs[top_idx]
    )
  })
  
  # Rename columns with padded topic numbers
  for (i in seq_along(wide_list)) {
    topic_number <- topic_ids[i]
    padded_id <- stringr::str_pad(topic_number, width = pad_width, pad = "0")
    setnames(wide_list[[i]],
             old = c("term", "probability"),
             new = c(paste0("topic_", padded_id, "_term"),
                     paste0("topic_", padded_id, "_prob")))
  }
  
  # Merge all tables by rank
  wide_out <- Reduce(function(x, y) merge(x, y, by = "rank", all = TRUE), wide_list)
  setkey(wide_out, NULL)
  return(wide_out)
}