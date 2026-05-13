if (getRversion() >= "2.15.1") {
  utils::globalVariables(character())
}

#' Prepare Weighted DFM Input for OpTop
#'
#' Convert a [quanteda::dfm] of counts to the document-level word proportions
#' expected by `OpTop::optimal_topic()`.
#'
#' @param x A [quanteda::dfm] object.
#'
#' @returns A [quanteda::dfm] with rows weighted to document-level proportions.
#'
#' @examplesIf requireNamespace("quanteda", quietly = TRUE)
#' dfmat <- quanteda::dfm(quanteda::tokens(c(doc1 = "a a b", doc2 = "b c")))
#' as_optop_weighted_dfm(dfmat)
#'
#' @export
as_optop_weighted_dfm <- function(x) {
  if (!quanteda::is.dfm(x)) {
    stop("'x' must be a quanteda dfm.", call. = FALSE)
  }
  quanteda::dfm_weight(x, scheme = "prop")
}

#' Prepare NLPstudio Topic Models for OpTop
#'
#' Extract an ordered list of raw `topicmodels::LDA(method = "VEM")` fits and a
#' vocabulary-aligned weighted DFM for `OpTop::optimal_topic()`.
#'
#' @param x An `nlp_k_selection` object created with `return_fits = TRUE`, a
#'   list of `nlp_topic_fit` objects, or a list of raw `LDA_VEM` objects.
#' @param weighted_dfm A weighted [quanteda::dfm], usually created with
#'   [as_optop_weighted_dfm()] from the same fitting input used for the LDA
#'   models.
#'
#' @returns A list of class `c("nlp_optop_input", "list")` with:
#'   \describe{
#'     \item{`lda_models`}{Raw `LDA_VEM` objects ordered by topic count.}
#'     \item{`weighted_dfm`}{Weighted DFM aligned to the LDA vocabulary.}
#'     \item{`k`}{Integer topic counts in ascending order.}
#'   }
#'
#' @details
#' OpTop currently expects `LDA_VEM` objects from **topicmodels**. This adapter
#' intentionally rejects Gibbs LDA, CTM, **text2vec**, **seededlda**, ETM, and
#' partial fits so that users do not pass objects outside OpTop's current
#' assumptions.
#'
#' NLPstudio does not import or call **OpTop**. After preparing the input, call
#' `OpTop::optimal_topic(lda_models = input$lda_models, weighted_dfm =
#' input$weighted_dfm, ...)` when **OpTop** is installed.
#'
#' @examplesIf requireNamespace("topicmodels", quietly = TRUE)
#' dtm <- methods::as(
#'   Matrix::Matrix(
#'     matrix(c(2, 1, 0, 0,  1, 1, 1, 0,  0, 1, 2, 1,
#'              0, 0, 1, 2,  1, 0, 1, 1,  1, 2, 0, 1),
#'            nrow = 6, byrow = TRUE),
#'     sparse = TRUE
#'   ),
#'   "dgCMatrix"
#' )
#' rownames(dtm) <- paste0("doc", 1:6)
#' colnames(dtm) <- paste0("term", 1:4)
#' dfmat <- quanteda::as.dfm(dtm)
#'
#' selection <- select_k_topics(
#'   dfmat,
#'   engine = "topicmodels",
#'   model = "lda",
#'   method = "VEM",
#'   k_grid = 2:3,
#'   metrics = c("diversity", "exclusivity"),
#'   holdout = 0,
#'   return_fits = TRUE,
#'   control = list(fit = list(seed = 1, em = list(iter.max = 5), var = list(iter.max = 5)))
#' )
#'
#' optop_input <- as_optop_input(selection, as_optop_weighted_dfm(dfmat))
#' # OpTop::optimal_topic(optop_input$lda_models, optop_input$weighted_dfm)
#'
#' @export
as_optop_input <- function(x, weighted_dfm) {
  if (missing(weighted_dfm)) {
    stop("'weighted_dfm' is required.", call. = FALSE)
  }
  if (!quanteda::is.dfm(weighted_dfm)) {
    stop("'weighted_dfm' must be a quanteda dfm.", call. = FALSE)
  }

  lda_models <- .optop_extract_lda_models(x)
  lda_models <- .optop_order_lda_models(lda_models)
  weighted_dfm <- .optop_align_weighted_dfm(weighted_dfm, lda_models)

  structure(
    list(
      lda_models = lda_models,
      weighted_dfm = weighted_dfm,
      k = unname(vapply(lda_models, .optop_lda_k, integer(1)))
    ),
    class = c("nlp_optop_input", "list")
  )
}

#' Print OpTop input summary
#'
#' @param x An `nlp_optop_input` object.
#' @param ... Unused.
#'
#' @returns Invisibly returns `x`.
#'
#' @export
print.nlp_optop_input <- function(x, ...) {
  cat("<nlp_optop_input>\n")
  cat(sprintf("  topic counts: %s\n", paste(x$k, collapse = ", ")))
  cat(sprintf(
    "  weighted dfm: %d documents x %d features\n",
    quanteda::ndoc(x$weighted_dfm),
    quanteda::nfeat(x$weighted_dfm)
  ))
  invisible(x)
}

.optop_extract_lda_models <- function(x) {
  if (inherits(x, "nlp_k_selection")) {
    fits <- attr(x, "fits", exact = TRUE)
    if (is.null(fits)) {
      stop(
        "'x' is an nlp_k_selection object without stored fits. Re-run select_k_topics(..., return_fits = TRUE).",
        call. = FALSE
      )
    }
    return(.optop_extract_lda_models(fits))
  }

  if (!is.list(x) || length(x) < 2L) {
    stop("'x' must contain at least two fitted LDA models.", call. = FALSE)
  }

  lapply(x, .optop_extract_one_lda_model)
}

.optop_extract_one_lda_model <- function(x) {
  if (inherits(x, "nlp_topic_fit")) {
    if (!identical(x$engine, "topicmodels") ||
        !identical(x$model, "lda") ||
        !identical(x$method, "VEM")) {
      stop(
        "OpTop input requires topicmodels LDA fits estimated with method = 'VEM'.",
        call. = FALSE
      )
    }
    x <- x$model_object
  }

  if (!.optop_is_lda_vem(x)) {
    stop(
      "OpTop input requires raw LDA_VEM objects from topicmodels::LDA(method = 'VEM').",
      call. = FALSE
    )
  }
  x
}

.optop_order_lda_models <- function(lda_models) {
  k <- vapply(lda_models, .optop_lda_k, integer(1))
  if (anyDuplicated(k)) {
    stop("OpTop input requires one model per topic count; duplicate K values were found.", call. = FALSE)
  }
  lda_models <- lda_models[order(k)]
  terms <- lapply(lda_models, .optop_lda_terms)
  ref_terms <- terms[[1L]]
  same_terms <- vapply(terms[-1L], identical, logical(1), ref_terms)
  if (!all(same_terms)) {
    stop("All OpTop LDA models must use the same vocabulary in the same order.", call. = FALSE)
  }
  names(lda_models) <- paste0("k", vapply(lda_models, .optop_lda_k, integer(1)))
  lda_models
}

.optop_align_weighted_dfm <- function(weighted_dfm, lda_models) {
  doc_ids <- as.character(quanteda::docid(weighted_dfm))
  if (!length(doc_ids) || all(is.na(doc_ids) | !nzchar(doc_ids) | doc_ids == "FALSE")) {
    stop("'weighted_dfm' must have meaningful document IDs for OpTop document matching.", call. = FALSE)
  }
  .optop_validate_weighted_proportions(weighted_dfm)
  .optop_validate_document_overlap(weighted_dfm, lda_models)

  model_terms <- .optop_lda_terms(lda_models[[1L]])
  dfm_terms <- quanteda::featnames(weighted_dfm)
  missing_terms <- setdiff(model_terms, dfm_terms)
  if (length(missing_terms)) {
    stop(
      sprintf(
        "'weighted_dfm' is missing %d terms used by the LDA models, including: %s.",
        length(missing_terms),
        paste(utils::head(missing_terms, 5L), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  weighted_dfm[, model_terms]
}

.optop_validate_weighted_proportions <- function(weighted_dfm, tolerance = 1e-8) {
  rs <- as.numeric(Matrix::rowSums(weighted_dfm))
  bad <- !is.finite(rs) | abs(rs - 1) > tolerance
  if (any(bad)) {
    stop(
      "'weighted_dfm' must contain document-level word proportions. Use as_optop_weighted_dfm() on the fitting dfm.",
      call. = FALSE
    )
  }
  invisible(weighted_dfm)
}

.optop_validate_document_overlap <- function(weighted_dfm, lda_models) {
  weighted_doc_ids <- as.character(quanteda::docid(weighted_dfm))
  has_overlap <- vapply(
    lda_models,
    function(model) {
      model_doc_ids <- as.character(methods::slot(model, "documents"))
      length(intersect(weighted_doc_ids, model_doc_ids)) > 0L
    },
    logical(1)
  )
  if (!all(has_overlap)) {
    stop(
      "Each OpTop LDA model must share at least one document ID with 'weighted_dfm'.",
      call. = FALSE
    )
  }
  invisible(weighted_dfm)
}

.optop_is_lda_vem <- function(x) {
  isS4(x) &&
    inherits(x, "LDA_VEM") &&
    all(c("k", "terms", "documents") %in% methods::slotNames(x))
}

.optop_lda_k <- function(x) {
  as.integer(methods::slot(x, "k"))
}

.optop_lda_terms <- function(x) {
  as.character(methods::slot(x, "terms"))
}
