if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("doc_id", "density", "theta", "topic"))
}

#' Plot the Distribution of Document Topic Weights
#'
#' Visualize the distribution of standardized DTW values as faceted histograms,
#' one facet per topic.
#'
#' @param x A supported topic-model object accepted by `get_dtw()`, or an
#'   already standardized DTW table returned by `get_dtw()`.
#' @param topics Optional topic filter supplied either as numeric indices or as
#'   `Topic###` identifiers. If `NULL`, all topics are plotted.
#' @param stat Character string. Either `"density"` (default) or `"count"`.
#' @param facet_args A named list of additional arguments passed to
#'   [facet_wrap()][ggplot2::facet_wrap]. Defaults to
#'   `list(scales = "free_y")`.
#' @param ... Additional arguments passed to
#'   [geom_histogram()][ggplot2::geom_histogram].
#'
#' @returns A [ggplot][ggplot2::ggplot] object.
#'
#' @seealso `fit_topic_model()`, `get_dtw()`, [get_top_terms()], [plot_top_terms()]
#'
#' @examples
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
#' rownames(dtm) <- paste0("doc", 1:4)
#' colnames(dtm) <- paste0("term", 1:3)
#'
#' fit <- fit_topic_model(
#'   dtm,
#'   engine = "text2vec",
#'   model = "lda",
#'   k = 2,
#'   fit_control = list(n_iter = 25, progressbar = FALSE)
#' )
#'
#' plot_dtw(fit, topics = 1:2, bins = 5)
#'
#' @import ggplot2
#' @export
plot_dtw <- function(x, topics = NULL, stat = c("density", "count"),
                     facet_args = list(scales = "free_y"), ...) {
  stat <- match.arg(stat)

  dtw <- get_dtw(x)
  topic_cols <- .find_topic_columns(dtw, id_col = "doc_id")
  sel_cols <- .resolve_topic_selector(topic_cols, topics)
  dtw <- dtw[, c("doc_id", sel_cols), with = FALSE]

  long_dtw <- data.table::melt(
    dtw,
    id.vars = "doc_id",
    variable.name = "topic",
    value.name = "theta"
  )
  long_dtw[, topic := factor(topic, levels = unique(topic))]

  p <- ggplot(long_dtw, aes(x = theta, y = after_stat(.data[[stat]]))) +
    geom_histogram(...) +
    labs(
      title = "Distribution of Document Topic Weights",
      x = "Document Topic Weight",
      y = tools::toTitleCase(stat)
    ) +
    theme_minimal(base_size = 12)

  facet_args$facets <- stats::as.formula("~ topic")
  p + do.call(ggplot2::facet_wrap, facet_args)
}
