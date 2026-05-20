make_optop_dfm <- function() {
  x <- Matrix::Matrix(
    matrix(
      c(3, 1, 0, 0,
        2, 2, 1, 0,
        0, 1, 3, 1,
        0, 0, 1, 3,
        2, 0, 1, 1,
        1, 3, 0, 1),
      nrow = 6,
      byrow = TRUE
    ),
    sparse = TRUE
  )
  x <- methods::as(x, "dgCMatrix")
  rownames(x) <- paste0("doc", seq_len(nrow(x)))
  colnames(x) <- paste0("term", seq_len(ncol(x)))
  quanteda::as.dfm(x)
}

make_optop_selection <- function(dfm = make_optop_dfm()) {
  skip_if_not_installed("topicmodels")
  select_k_topics(
    dfm,
    engine = "topicmodels",
    model = "lda",
    method = "VEM",
    k_grid = 2:3,
    metrics = c("diversity", "exclusivity"),
    holdout = 0,
    return_fits = TRUE,
    control = list(fit = list(seed = 1L, em = list(iter.max = 5), var = list(iter.max = 5)))
  )
}

test_that("as_optop_weighted_dfm returns document-level proportions", {
  dfm <- make_optop_dfm()
  out <- as_optop_weighted_dfm(dfm)

  expect_true(quanteda::is.dfm(out))
  expect_equal(as.character(quanteda::docid(out)), as.character(quanteda::docid(dfm)))
  expect_equal(quanteda::featnames(out), quanteda::featnames(dfm))
  expect_equal(as.numeric(rowSums(as.matrix(out))), rep(1, quanteda::ndoc(out)))
  expect_error(as_optop_weighted_dfm(as.matrix(dfm)), "quanteda dfm")
})

test_that("as_optop_input prepares selection fits for OpTop", {
  dfm <- make_optop_dfm()
  selection <- make_optop_selection(dfm)
  weighted <- as_optop_weighted_dfm(dfm)

  out <- as_optop_input(selection, weighted)

  expect_s3_class(out, "nlp_optop_input")
  expect_equal(out$k, 2:3)
  expect_named(out$lda_models, c("k2", "k3"))
  expect_true(all(vapply(out$lda_models, inherits, logical(1), "LDA_VEM")))
  expect_equal(quanteda::featnames(out$weighted_dfm), out$lda_models[[1]]@terms)
  expect_equal(as.character(quanteda::docid(out$weighted_dfm)), paste0("doc", 1:6))

  printed <- capture.output(NLPstudio:::print.nlp_optop_input(out))
  expect_true(any(grepl("<nlp_optop_input>", printed, fixed = TRUE)))
  expect_true(any(grepl("topic counts: 2, 3", printed, fixed = TRUE)))
})

test_that("as_optop_input output is accepted by OpTop when installed", {
  skip_if_not_installed("OpTop")
  dfm <- make_optop_dfm()
  selection <- make_optop_selection(dfm)
  optop_input <- as_optop_input(selection, as_optop_weighted_dfm(dfm))
  optimal_topic <- get("optimal_topic", envir = asNamespace("OpTop"))
  result <- NULL

  expect_no_error({
    invisible(utils::capture.output(
      result <- suppressWarnings(optimal_topic(
        lda_models = optop_input$lda_models,
        weighted_dfm = optop_input$weighted_dfm
      ))
    ))
  })
  expect_false(is.null(result))
})

test_that("as_optop_input accepts nlp_topic_fit and raw LDA_VEM lists", {
  dfm <- make_optop_dfm()
  selection <- make_optop_selection(dfm)
  fits <- attr(selection, "fits")
  weighted <- as_optop_weighted_dfm(dfm)

  from_fits <- as_optop_input(rev(fits), weighted)
  from_raw <- as_optop_input(lapply(rev(fits), `[[`, "model_object"), weighted)

  expect_equal(from_fits$k, 2:3)
  expect_equal(from_raw$k, 2:3)
  expect_equal(names(from_fits$lda_models), c("k2", "k3"))
  expect_equal(names(from_raw$lda_models), c("k2", "k3"))
})

test_that("as_optop_input rejects missing fits and unsupported models", {
  dfm <- make_optop_dfm()
  weighted <- as_optop_weighted_dfm(dfm)
  no_fits <- data.table::data.table(k = 2L, metric = "diversity", level = "aggregate",
                                    topic_id = NA_character_, value = 1, supported = TRUE)
  data.table::setattr(no_fits, "class", c("nlp_k_selection", "data.table", "data.frame"))

  expect_error(as_optop_input(no_fits, weighted), "return_fits = TRUE")
  expect_error(as_optop_input(list(list()), weighted), "at least two")
  expect_error(as_optop_input(list(list(), list()), weighted), "LDA_VEM")

  skip_if_not_installed("topicmodels")
  vem_fit <- fit_topic_model(
    dfm,
    engine = "topicmodels",
    model = "lda",
    method = "VEM",
    k = 2,
    control = list(fit = list(seed = 1L, em = list(iter.max = 5), var = list(iter.max = 5)))
  )
  gibbs_fit <- fit_topic_model(
    dfm,
    engine = "topicmodels",
    model = "lda",
    method = "Gibbs",
    k = 3,
    control = list(fit = list(seed = 1L, iter = 50L, burnin = 0L, thin = 1L))
  )

  expect_error(as_optop_input(list(vem_fit, gibbs_fit), weighted), "method = 'VEM'")
  expect_error(as_optop_input(list(vem_fit, vem_fit), weighted), "duplicate K")
})

test_that("as_optop_input validates and aligns weighted DFM vocabulary", {
  dfm <- make_optop_dfm()
  selection <- make_optop_selection(dfm)
  weighted <- as_optop_weighted_dfm(dfm)
  fits <- attr(selection, "fits")

  shuffled <- weighted[, rev(quanteda::featnames(weighted))]
  out <- as_optop_input(fits, shuffled)
  expect_equal(quanteda::featnames(out$weighted_dfm), out$lda_models[[1]]@terms)

  partial_docs <- weighted[1:3, ]
  partial_out <- as_optop_input(fits, partial_docs)
  expect_equal(quanteda::ndoc(partial_out$weighted_dfm), 3L)

  missing_term <- weighted
  colnames(missing_term)[1L] <- "not_in_model"
  expect_error(as_optop_input(fits, missing_term), "missing 1 terms")
  expect_error(as_optop_input(fits, dfm), "document-level word proportions")
  expect_error(as_optop_input(fits), "'weighted_dfm' is required")
  expect_error(as_optop_input(fits, as.matrix(weighted)), "quanteda dfm")

  bad_docids <- weighted
  attr(bad_docids, "docvars")$docid_ <- factor(
    rep("FALSE", quanteda::ndoc(bad_docids)),
    levels = "FALSE"
  )
  expect_error(as_optop_input(fits, bad_docids), "meaningful document IDs")

  raw_models <- lapply(fits, `[[`, "model_object")
  raw_models[[2L]]@terms <- rev(raw_models[[2L]]@terms)
  expect_error(as_optop_input(raw_models, weighted), "same vocabulary")

  no_overlap_models <- lapply(fits, `[[`, "model_object")
  no_overlap_models[[2L]]@documents <- paste0("other", seq_along(no_overlap_models[[2L]]@documents))
  expect_error(
    as_optop_input(no_overlap_models, weighted),
    "Each OpTop LDA model must share at least one document ID"
  )
})
