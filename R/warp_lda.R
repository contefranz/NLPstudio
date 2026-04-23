#' Backward-Compatible Wrapper for `fit_topic_model()`
#'
#' `warp_lda()` remains available in v0.6.1 as a compatibility wrapper for the
#' legacy WarpLDA workflow. New code should call `fit_topic_model()` with
#' `engine = "text2vec"` and `model = "lda"` instead.
#'
#' @param x A sparse document-feature input of class [dgCMatrix-class][Matrix::dgCMatrix-class],
#'   [dfm][quanteda::dfm], or `DocumentTermMatrix`.
#' @param k Number of topics to estimate.
#' @param return_theta Should cached DTW be returned under the legacy `theta`
#'   name? Defaults to `TRUE`.
#' @param return_phi Should cached TWW be returned under the legacy `phi`
#'   name? Defaults to `TRUE`.
#' @param lda_control A named list of arguments forwarded to
#'   [`LDA$new()`][text2vec::LDA].
#' @param fit_control A named list of arguments forwarded to
#'   `LDA$fit_transform()`. Internally this legacy wrapper maps `lda_control`
#'   and `fit_control` into `control = list(model = lda_control, fit = fit_control)`.
#'
#' @returns A named list with the historical structure:
#'
#' - `lda_object`: the raw [`LDA`][text2vec::LDA] fit.
#' - `theta`: standardized DTW when `return_theta = TRUE`.
#' - `phi`: standardized TWW when `return_phi = TRUE`.
#'
#' @details
#' This wrapper is soft-deprecated in favor of `fit_topic_model()`. It keeps
#' the old return shape for one release cycle so existing code can migrate
#' incrementally.
#'
#' @seealso `fit_topic_model()`, `get_dtw()`, `get_tww()`
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
#' suppressWarnings(
#'   warp_lda(
#'     dtm,
#'     k = 2,
#'     fit_control = list(n_iter = 25, progressbar = FALSE)
#'   )
#' )
#'
#' @export
warp_lda <- function(x, k, return_theta = TRUE, return_phi = TRUE,
                     lda_control = list(), fit_control = list()) {
  if (!is.logical(return_theta) || length(return_theta) != 1L ||
      !is.logical(return_phi) || length(return_phi) != 1L) {
    stop("return_theta and return_phi must be single TRUE/FALSE values.")
  }
  if (!is.list(lda_control)) {
    stop("lda_control must be a list.")
  }
  if (!is.list(fit_control)) {
    stop("fit_control must be a list.")
  }

  if (requireNamespace("lifecycle", quietly = TRUE)) {
    lifecycle::deprecate_warn(
      when = "0.6.1",
      what = "warp_lda()",
      with = "fit_topic_model()"
    )
  } else {
    warning(
      "warp_lda() is deprecated as of v0.6.1. Use fit_topic_model() instead.",
      call. = FALSE
    )
  }

  fit <- fit_topic_model(
    x = x,
    engine = "text2vec",
    model = "lda",
    k = k,
    return_dtw = return_theta,
    return_tww = return_phi,
    control = list(model = lda_control, fit = fit_control)
  )

  out <- list(lda_object = fit$model_object)
  if (return_theta) {
    out$theta <- get_dtw(fit)
  }
  if (return_phi) {
    out$phi <- get_tww(fit)
  }
  out
}
