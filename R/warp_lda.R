#' Fit a WarpLDA Class Topic Model
#'
#' Fit a Latent Dirichlet Allocation model using [WarpLDA][text2vec::LDA] class from the package **text2vec**.
#' Conveniently returns utility objects such as \eqn{\theta} and \eqn{\phi} matrices for sub-sequent
#' analysis. 
#'
#' @param x A sparse document-term matrix of class [dgCMatrix-class].
#' @param k The number of latent topics to estimate.
#' @param return_theta Should the Document-Topic-Weights matrix ( \eqn{\theta} ) be returned? Default to `TRUE`.
#' @param return_phi Should the Topic-Word-Weights matrix ( \eqn{\phi} ) be returned? Default to `TRUE`.
#' @param ... Additional arguments passed to [`LDA$new`][text2vec::LDA] and [LDA$fit_transform][text2vec::LDA].
#' 
#' @details
#' This function wraps the [text2vec::LDA] class to fit a probabilistic topic model using the 
#' WarpLDA algorithm—a highly efficient method for estimating Latent Dirichlet Allocation (LDA) 
#' models on large and sparse document-term matrices. Internally, the function initializes the 
#' LDA model via `LDA$new()` and fits it using `LDA$fit_transform()`, passing user-supplied 
#' parameters through `...`. 
#' 
#' The output includes the trained model object and, optionally, the matrices \eqn{\theta} 
#' (document-topic proportions) and \eqn{\phi} (topic-word distributions), both in `data.table` format 
#' for convenient downstream analysis. Note that \eqn{\phi} is expressed as a probability
#' distribution over words for each topic. That is, it already represents likelihood estimates.
#' 
##' @returns
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
#' @importFrom cli cli_alert_info
#' @export

warp_lda = function(x, k, return_theta = TRUE, return_phi = TRUE, ...) {
  
  if ( !is(x, "dgCMatrix") ) {
    stop("x must be a sparse matrix of class dgCMatrix")
  }
  if ( !is.numeric(k) ) {
    stop("k must be a numeric")
  }
  if (!is.logical(return_theta) || length(return_theta) != 1 ||
      !is.logical(return_phi) || length(return_phi) != 1) {
    stop("return_theta and return_phi must be single TRUE/FALSE values")
  }  
  
  # Split ellipsis into params for LDA$new() and fit_transform()
  dots = list(...)
  
  # Set defaults but allow overrides
  lda_args = modifyList(
    list(
      n_topics = k,
      doc_topic_prior = 0.1,
      topic_word_prior = 0.001
    ), 
    dots
  )
  
  fit_args = modifyList(
    list(
      x = x,
      n_iter = 1000,
      convergence_tol = 0.001,
      n_check_convergence = 25,
      progressbar = TRUE
    ), 
    dots
  )
  
  cli_alert_info("Initiating LDA estimation with {k} topics...")
  # Create LDA model
  lda_model = do.call(LDA$new, lda_args)
  
  # Fit and transform
  theta = do.call(lda_model$fit_transform, fit_args)
  
  out = list(lda_object = lda_model)
  
  if (return_theta) {
    out$theta = as.data.table(theta, keep.rownames = TRUE)
    set_theta_names(theta_dt = out$theta)
  }
  if (return_phi) {
    out$phi = as.data.table(lda_model$topic_word_distribution)
  }
  return(out)
}

#' @rdname warp_lda
#' @export
warpLDA = function(...) {
  if (requireNamespace("lifecycle", quietly = TRUE)) {
    lifecycle::deprecate_warn("0.3.0", "warpLDA()", "warp_lda()")
  } else {
    warning("warpLDA() is deprecated. Use warp_lda() instead.", call. = FALSE)
  }
  warp_lda(...)
}