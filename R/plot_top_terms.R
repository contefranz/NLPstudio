if ( getRversion() >= "2.15.1" ) {
  utils::globalVariables( c("term") )
}
#' Visualize Topic-Word Probabilities
#'
#' This function plots the top-ranked terms for each topic based on the long-format output from
#' [get_top_terms()]. It produces a faceted horizontal bar chart where each facet represents 
#' one topic, and the height of each bar reflects the term's estimated topic-word probability 
#' ( \eqn{\phi} ).
#'
#' @param top_terms A `data.table` returned by [get_top_terms()], containing the columns 
#' `rank`, `topic`, `term`, and `probability`.
#' @param facet_args A named list of additional arguments passed to [facet_wrap()]. 
#' By default, `scales = "free_y"` is used.
#' @param ... Additional arguments passed to [geom_col()].
#'
#' @details
#' The function uses **[ggplot2]** to create a faceted bar chart showing the most relevant terms 
#' for each topic. Terms are ranked and sorted within each facet by their associated probability 
#' in the topic-word distribution matrix (\eqn{\phi}). Internally, the term ordering is adjusted using 
#' **[tidytext]** `reorder_within()` to ensure per-topic ranking. This function is typically 
#' used in conjunction with [get_top_terms()] with `format = "long"`, which prepares the data for 
#' suitable visualization.
#'
#' @returns A `ggplot` object showing one horizontal bar chart per topic. Each bar reflects 
#' a term’s estimated contribution to that topic.
#'
#' @seealso [get_top_terms()], [plot_dtw()]
#'
#' @import ggplot2
#' @import data.table
#' @importFrom tidytext reorder_within scale_y_reordered
#' @export

plot_top_terms = function(top_terms, facet_args = list(scales = "free_y"), ...) {
  
  # Detect long format structure
  required_cols <- c("rank", "topic", "term", "probability")
  if (!all(required_cols %in% names(top_terms))) {
    stop("The input does not appear to be in long format. \n",
         "Use get_top_terms(..., format = \"long\") before calling this function.")
  }  
  # Ensure 'topic' is a factor so facet order is preserved
  top_terms[, topic := factor(topic, levels = sort(unique(topic)))]
  
  # Reorder term within each topic by descending probability
  top_terms[, term := reorder_within(term, probability, topic)]
  
  # Base plot
  p = ggplot(top_terms, aes(x = probability, y = term)) +
    geom_col(...) +
    scale_y_reordered() +  # restore correct labels in each facet
    labs(
      title = "Top Terms per Topic",
      x = "Topic-Word Probability",
      y = "Term"
    ) +
    theme_minimal(base_size = 12)
  
  # Apply facetting
  facet_args$facets = as.formula("~ topic")
  p = p + do.call(facet_wrap, facet_args)
  
  return(p)
}