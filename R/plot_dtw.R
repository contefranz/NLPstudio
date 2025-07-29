if ( getRversion() >= "2.15.1" ) {
  utils::globalVariables( c( "topic", "density") )
}
#' Plot Distribution of Document-Topic-Weights
#'
#' `plot_dtw()` visualizes the distribution of topic proportions across documents
#' from a `theta` matrix, typically returned by a topic model such as [warpLDA()].
#' Each topic is shown as a separate histogram, allowing users to assess the sparsity,
#' dominance, or spread of each topic across the corpus.
#' 
#' @param theta A `data.table` of document-topic proportions, with one row per document and one 
#' column per topic. The first column must be `doc_id`, and all other columns should contain 
#' numeric topic weights. 
#' @param facet_args A named list of additional arguments passed to [facet_wrap()], 
#' such as `ncol`, `nrow`, or `strip.position`. By default, `scales = "free_y"` 
#' is used to allow individual y-axis scaling for each topic. 
#' @param ... Additional arguments passed to [geom_histogram()], 
#' such as `binwidth`, `fill`, or `color`.
#'
#' @details
#' Internally, the function reshapes the `theta` matrix to long format and constructs a faceted
#' histogram using [ggplot2]. Each facet corresponds to a topic and shows the density of the topic
#' proportions across all documents. This is useful for diagnosing topic quality, sparsity, and 
#' prevalence.
#'
#' The `theta` matrix is expected to follow the structure returned by [warpLDA()], where `doc_id`
#' is the document identifier and remaining columns represent topic probabilities 
#' (`Topic001`, `Topic002`, etc.). 
#' 
#' If one ensures the presence of the column `doc_id`, then `plot_dtw()` can be used on a 
#' [TopicModel-class][topicmodels::LDA-class] class object from the package **topicmodels**. 
#'
#' @returns A `ggplot` object representing the faceted histogram of topic proportions.
#'
#' @seealso [warpLDA()], [topicmodels::LDA()], [geom_histogram()], [facet_wrap()]
#'
#' @import ggplot2 data.table
#' @importFrom stats as.formula
#' @export

plot_dtw = function(theta, facet_args = list(scales = "free_y"), ...) {
  
  # Melt the data.table to long format
  long_theta = melt(
    theta,
    id.vars = "doc_id",
    variable.name = "topic",
    value.name = "theta"
  )
  
  long_theta[, topic := factor(topic, levels = unique(topic))]
  
  # Base ggplot object
  p = ggplot(long_theta, aes(x = theta, y = after_stat(density))) +
    geom_histogram(...) +
    labs(
      title = "Distribution of Document-Topic Weights",
      x = "Document Topic Proportion",
      y = "Density"
    ) +
    theme_minimal(base_size = 12)
  
  # Handle facet_wrap with additional arguments
  facet_args$facets = as.formula("~ topic")
  # facet_args$scales <- scales
  p = p + do.call(facet_wrap, facet_args)
  
  return(p)
}