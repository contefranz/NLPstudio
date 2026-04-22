if ( getRversion() >= "2.15.1" ) {
  utils::globalVariables( c("term") )
}
#' Visualize Topic-Word Probabilities
#'
#' Create a faceted bar chart of the highest-probability terms for each topic
#' using the long-format output from [get_top_terms()] via **[ggplot2][ggplot2::ggplot2]**. Each facet corresponds
#' to one topic, and bars represent the estimated topic–word probabilities
#' (\eqn{\phi}).
#' @param top_terms A [data.table][data.table::data.table] returned by
#'   [get_top_terms()] with `format = "long"`. Must contain the columns
#'   `rank`, `topic`, `term`, and `probability`.
#' @param facet_args A named list of additional arguments passed to [facet_wrap()][ggplot2::facet_wrap]. 
#' Defaults to `list(scales = "free_y")`, which allows each facet to have its own y-axis scale.
#' @param ... Additional arguments passed to [geom_col()][ggplot2::geom_col].
#'
#' @details
#' The function visualizes topic–word probabilities in a tidy, per-topic
#' format. Terms are ranked within each topic by descending probability
#' and reordered internally using [tidytext::reorder_within] to ensure
#' correct sorting within facets. Typically, this function is used in
#' combination with [get_top_terms()] (with `format = "long"`) to prepare
#' the input data.
#'
#' @returns A [ggplot][ggplot2::ggplot] object: a faceted horizontal bar chart with
#'   one facet per topic. Each bar shows the contribution of a term to that
#'   topic, as estimated by the topic–word distribution matrix (\eqn{\phi}).
#'
#' @seealso [get_top_terms()], [plot_dtw()]
#'
#' @examplesIf interactive()
#' # Requires the optional tidytext package.
#' dtm <- methods::as(
#'   Matrix::Matrix(
#'     matrix(
#'       c(1, 0, 1,
#'         1, 1, 0,
#'         0, 1, 1,
#'         1, 1, 1),
#'       nrow = 4,
#'       byrow = TRUE
#'     ),
#'     sparse = TRUE
#'   ),
#'   "dgCMatrix"
#' )
#' colnames(dtm) <- paste0("term", 1:3)
#' rownames(dtm) <- paste0("doc", 1:4)
#'
#' model <- fit_topic_model(
#'   dtm,
#'   engine = "text2vec",
#'   model = "lda",
#'   k = 2,
#'   fit_control = list(n_iter = 25, progressbar = FALSE)
#' )
#' top_terms <- get_top_terms(model, n = 3, format = "long")
#'
#' plot_top_terms(top_terms)
#'
#' @import ggplot2
#' @import data.table
#' @export

plot_top_terms = function(top_terms, facet_args = list(scales = "free_y"), ...) {
  
  if (!requireNamespace("tidytext", quietly = TRUE)) {
    stop("Package 'tidytext' is required for plot_top_terms(). Please install it.", call. = FALSE)
  }
  # Detect long format structure
  required_cols <- c("rank", "topic", "term", "probability")
  if (!all(required_cols %in% names(top_terms))) {
    stop("The input does not appear to be in long format. \n",
         "Use get_top_terms(..., format = \"long\") before calling this function.")
  }  
  # Ensure 'topic' is a factor so facet order is preserved
  top_terms[, topic := factor(topic, levels = sort(unique(topic)))]
  
  # Look up tidytext functions
  reorder_within <- getExportedValue("tidytext", "reorder_within")
  scale_y_reordered <- getExportedValue("tidytext", "scale_y_reordered")
  
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
  facet_args$facets = stats::as.formula("~ topic")
  p = p + do.call(ggplot2::facet_wrap, facet_args)
  
  return(p)
}
