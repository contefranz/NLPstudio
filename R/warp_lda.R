#' Fit a WarpLDA Class Topic Model
#'
#' Fit a Latent Dirichlet Allocation model using [WarpLDA][text2vec::LDA] class from the package **text2vec**.
#' Conveniently returns utility objects such as \eqn{\theta} and \eqn{\phi} matrices for subsequent
#' analysis.
#'
#' @param x A sparse document-term matrix of class [dgCMatrix-class].
#' @param k The number of latent topics to estimate.
#' @param return_theta Should the Document-Topic-Weights matrix ( \eqn{\theta} ) be returned? Default to `TRUE`.
#' @param return_phi Should the Topic-Word-Weights matrix ( \eqn{\phi} ) be returned? Default to `TRUE`.
#' @param lda_control A named list of arguments forwarded to [`LDA$new()`][text2vec::LDA].
#'   Overrides the defaults `doc_topic_prior = 0.1` and `topic_word_prior = 0.001`.
#'   `n_topics` is always set from `k` and cannot be overridden here.
#' @param fit_control A named list of arguments forwarded to
#'   [`LDA$fit_transform()`][text2vec::LDA]. Overrides the defaults
#'   `n_iter = 1000`, `convergence_tol = 0.001`, `n_check_convergence = 25`,
#'   and `progressbar = TRUE`.
#'
#' @details
#' This function wraps the [text2vec::LDA] class to fit a probabilistic topic model using the
#' WarpLDA algorithm — a highly efficient method for estimating Latent Dirichlet Allocation (LDA)
#' models on large and sparse document-term matrices. Internally, the function initializes the
#' LDA model via `LDA$new()` and fits it using `LDA$fit_transform()`.
#'
#' Constructor arguments (`lda_control`) and fitting arguments (`fit_control`) are kept separate
#' so that options valid only at construction time (e.g., `doc_topic_prior`) cannot accidentally
#' be forwarded to the fitting step, and vice versa (e.g., `progressbar`).
#'
#' The output includes the trained model object and, optionally, the matrices \eqn{\theta}
#' (document-topic proportions) and \eqn{\phi} (topic-word distributions), both in `data.table` format
#' for convenient downstream analysis. Note that \eqn{\phi} is expressed as a probability
#' distribution over words for each topic. That is, it already represents likelihood estimates.
#'
#' @returns
#' A named list with the following elements:
#'
#' - `lda_object`. The trained [`LDA`][text2vec::LDA] model object, containing methods for inference,
#' scoring, and topic extraction.
#'
#' - `theta`. A `data.table` of document-topic weights (\eqn{\theta}), where rows are documents
#' and columns are topic proportions. Included if `return_theta = TRUE`.
#'
#' - `phi`. A `data.table` of topic-word probabilities (\eqn{\phi}), where rows are topics and columns
#' are terms. Included if `return_phi = TRUE`.
#'
#' @seealso [`LDA`][text2vec::LDA]
#'
#' @references
#' Chen, J., Li, K., Zhu, J., & Chen, W. (2015). Warplda: a cache efficient O(1) algorithm for
#' Latent Dirichlet Allocation. [_arXiv preprint arXiv:1510.08628_](https://arxiv.org/abs/1510.08628).
#'
#' Blei, D. M., Ng, A. Y., & Jordan, M. I. (2003). [Latent Dirichlet Allocation](https://www.jmlr.org/papers/volume3/blei03a/blei03a.pdf).
#' _Journal of Machine Learning Research_, 3(Jan), 993-1022.
#'
#' @importFrom methods is
#' @importFrom data.table as.data.table setnames
#' @importFrom text2vec LDA
#' @importFrom utils modifyList
#' @importFrom cli cli_h2 cli_alert_info cli_alert_success
#' @export

warp_lda <- function(x, k, return_theta = TRUE, return_phi = TRUE,
                     lda_control = list(), fit_control = list()) {

  if (!is(x, "dgCMatrix")) {
    stop("x must be a sparse matrix of class dgCMatrix")
  }
  if (!is.numeric(k) || length(k) != 1L || k < 1L) {
    stop("k must be a single positive numeric")
  }
  if (!is.logical(return_theta) || length(return_theta) != 1L ||
      !is.logical(return_phi)   || length(return_phi)   != 1L) {
    stop("return_theta and return_phi must be single TRUE/FALSE values")
  }
  if (!is.list(lda_control)) stop("lda_control must be a list")
  if (!is.list(fit_control)) stop("fit_control must be a list")

  cli_h2("Fitting WarpLDA topic model")
  cli_alert_info("Initiating LDA estimation with {k} topics...")

  lda_args <- modifyList(
    list(n_topics = k, doc_topic_prior = 0.1, topic_word_prior = 0.001),
    lda_control
  )
  lda_args$n_topics <- k  # n_topics is always driven by k

  fit_args <- modifyList(
    list(x = x, n_iter = 1000, convergence_tol = 0.001,
         n_check_convergence = 25, progressbar = TRUE),
    fit_control
  )
  fit_args$x <- x  # x is always the input matrix

  lda_model <- do.call(LDA$new, lda_args)
  theta      <- do.call(lda_model$fit_transform, fit_args)

  out <- list(lda_object = lda_model)

  if (return_theta) {
    out$theta <- as.data.table(theta, keep.rownames = TRUE)
    set_theta_names(theta_dt = out$theta)
  }
  if (return_phi) {
    out$phi <- as.data.table(lda_model$topic_word_distribution)
  }

  cli_alert_success("WarpLDA model fitted successfully")
  return(out)
}

#' @rdname warp_lda
#' @export
warpLDA <- function(...) {
  if (requireNamespace("lifecycle", quietly = TRUE)) {
    lifecycle::deprecate_warn("0.3.0", "warpLDA()", "warp_lda()")
  } else {
    warning("warpLDA() is deprecated. Use warp_lda() instead.", call. = FALSE)
  }
  warp_lda(...)
}
