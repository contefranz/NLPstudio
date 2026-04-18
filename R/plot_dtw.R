if ( getRversion() >= "2.15.1" ) {
  utils::globalVariables( c("doc_id", "topic", "theta", "density") )
}
#' Plot Distribution of Document-Topic-Weights
#'
#' `plot_dtw()` visualizes the distribution of topic proportions across documents.
#' It accepts different model classes. Each topic is shown as a
#' separate histogram, allowing you to assess sparsity, dominance, and spread
#' across the corpus.
#'
#' @param x Either the output of [warp_lda()], a fitted [topicmodels::LDA()] object of class 
#' [TopicModel-class][topicmodels::LDA-class], or a sequential LDA fitted by [textmodel_seqlda()].
#' @param topics Optional numeric vector specifying which topic proportions to plot. 
#' If `NULL` (default), all topics will be plotted.
#' @param stat Character string, either `"density"` (default) or `"count"`, 
#' controlling the y-axis statistic in the histogram.
#' @param facet_args A named list of additional arguments passed to [facet_wrap()], 
#' such as `ncol`, `nrow`, or `strip.position`. By default, `scales = "free_y"` is used 
#' to allow per-topic y-axis scaling. 
#' @param ... Additional arguments passed to [geom_histogram()], 
#' such as `binwidth`, `fill`, or `color`.
#'
#' @details
#' Internally, the function reshapes the `theta` matrix to long format and constructs a faceted
#' histogram using [ggplot2]. Each facet corresponds to a topic and shows the density of the topic
#' proportions across all documents. This is useful for diagnosing topic quality, sparsity, and 
#' prevalence.
#' 
#' For `warp_lda()` input, the function uses `x$theta` directly. 
#' For [TopicModel-class][topicmodels::LDA-class] input, the function constructs a `data.table` from the fitted object 
#' (e.g., using `x@documents` and `x@gamma`), and then ensures the presence of a `doc_id` column 
#' and standardized topic column names (e.g., `Topic001`, `Topic002`, `...`).
#'
#' @returns A [ggplot] object representing the faceted histograms of document–topic proportions.
#'
#' @seealso [warp_lda()], [topicmodels::LDA()], [seededlda::textmodel_seqlda()], [geom_histogram()]
#'
#' @import ggplot2 data.table
#' @export

plot_dtw = function(x, topics = NULL, stat = c("density", "count"), 
                    facet_args = list(scales = "free_y"), ...) {
  
  stat <- match.arg(stat)
  
  # Support for direct output from warp_lda() and LDA_VEM/LDA_Gibbs classes from topicmodels
  if ( is.list(x) && inherits(x$lda_object, "WarpLDA") ) {
    theta = x$theta  
  } else if ( !is.list(x) && (inherits(x, "VEM") || inherits(x, "Gibbs"))) {
    if (!requireNamespace("topicmodels", quietly = TRUE)) {
      stop("Package 'topicmodels' must be installed to handle VEM/Gibbs objects.", call. = FALSE)
    }
    theta = data.table::data.table(rn = x@documents, x@gamma)
    set_theta_names(theta_dt = theta)
  } else if (inherits(x, "textmodel_lda")) {
    theta = data.table::as.data.table(x$theta, keep.rownames = TRUE)
    set_theta_names(theta_dt = theta)
  } else {
    stop("x is an unrecognized object")
  }
  
  # --- Topic selection ---
  topic_cols <- setdiff(names(theta), "doc_id")
  
  if (is.null(topics)) {
    sel_cols <- topic_cols
  } else {
    if (!is.numeric(topics))
      stop("topics must be NULL or a numeric vector of indices.")
    if (any(is.na(topics)))
      stop("topics contains NA.")
    if (any(topics < 1 | topics > length(topic_cols)))
      stop(paste0("Topic indices must be in the range [1, ", length(topic_cols), "]"))
    # allow non-integers like 1.0 but forbid fractional indices
    if (any(topics != floor(topics)))
      stop("topics must contain integer indices (e.g., 1, 2, 3).")
    sel_cols <- topic_cols[topics]
  }
  
  theta <- theta[, c("doc_id", sel_cols), with = FALSE]
  
  # Melt the data.table to long format
  long_theta = data.table::melt(
    theta,
    id.vars = "doc_id",
    variable.name = "topic",
    value.name = "theta"
  )
  long_theta[, topic := factor(topic, levels = unique(topic))]
  
  # Base ggplot object
  p = ggplot(long_theta, aes(x = theta, y = after_stat(.data[[stat]]))) +
    geom_histogram(...) +
    labs(
      title = "Distribution of Document-Topic Weights",
      x = "Document Topic Proportion",
      y = tools::toTitleCase(stat)
    ) +
    theme_minimal(base_size = 12)
  
  # Handle facet_wrap with additional arguments
  facet_args$facets = stats::as.formula("~ topic")
  p = p + do.call(ggplot2::facet_wrap, facet_args)
  
  return(p)
}