make_stm_dfm <- function() {
  txt <- c(
    "apple orange banana fruit",
    "apple banana fruit market",
    "orange banana apple growth",
    "bank loan credit risk",
    "loan credit debt bank",
    "credit risk debt market",
    "audit control governance compliance",
    "governance audit reporting controls",
    "compliance reporting audit risk"
  )
  dfm <- quanteda::dfm(quanteda::tokens(txt))
  quanteda::docnames(dfm) <- paste0("doc", seq_along(txt))
  quanteda::docvars(dfm, "sector") <- rep(c("fruit", "credit", "governance"), each = 3)
  dfm
}

fit_stm_fixture <- function(prevalence = FALSE, ...) {
  skip_if_not_installed("stm")
  dfm <- make_stm_dfm()
  fit_control <- list(
    seed = 1L,
    max.em.its = 5L,
    init.type = "Spectral",
    verbose = FALSE
  )
  if (prevalence) {
    fit_control$prevalence <- ~sector
  }
  fit_control <- utils::modifyList(fit_control, list(...))
  suppressWarnings(fit_topic_model(
    dfm,
    engine = "stm",
    model = "stm",
    k = 3,
    control = list(fit = fit_control)
  ))
}

test_that("fit_topic_model validates STM backend arguments", {
  skip_if_not_installed("stm")
  dfm <- make_stm_dfm()

  expect_error(
    fit_topic_model(dfm, engine = "stm", model = "lda", k = 3),
    "Unsupported model 'lda' for engine 'stm'"
  )
  expect_error(
    fit_topic_model(dfm, engine = "stm", model = "stm", k = 3, method = "VEM"),
    "method must be NULL"
  )
  expect_error(
    fit_topic_model(
      dfm,
      engine = "stm",
      model = "stm",
      k = 3,
      control = list(fit = list(content = ~sector))
    ),
    "STM content covariates are not supported in v0.9.4"
  )
  expect_error(
    fit_topic_model(
      dfm,
      engine = "stm",
      model = "stm",
      k = 3,
      control = list(fit = list(documents = list()))
    ),
    "managed by NLPstudio"
  )

  bad_meta <- data.frame(doc_id = paste0("other", seq_len(quanteda::ndoc(dfm))),
                         sector = quanteda::docvars(dfm, "sector"))
  expect_error(
    fit_topic_model(
      dfm,
      engine = "stm",
      model = "stm",
      k = 3,
      control = list(fit = list(prevalence = ~sector, data = bad_meta))
    ),
    "one row for each input document ID"
  )
})

test_that("STM plain fits expose standardized outputs and prediction", {
  fit <- fit_stm_fixture()
  dfm <- make_stm_dfm()

  expect_s3_class(fit, "nlp_topic_fit")
  expect_identical(fit$engine, "stm")
  expect_identical(fit$model, "stm")
  expect_null(fit$method)
  expect_equal(dim(fit$dtw), c(9L, 3L))
  expect_equal(nrow(fit$tww), 3L)
  expect_equal(colnames(fit$dtw), sprintf("Topic%03d", 1:3))
  expect_equal(rownames(fit$dtw), quanteda::docnames(dfm))

  dtw <- get_dtw(fit, docvars = TRUE)
  tww <- get_tww(fit)
  terms <- get_top_terms(fit, n = 3)
  summary <- summarize_topics(fit, training = dfm, top_n = 3, representative_n = 1)

  expect_true(all(c("doc_id", "sector", "Topic001") %in% names(dtw)))
  expect_equal(nrow(tww), 3L)
  expect_equal(nrow(terms), 9L)
  expect_equal(nrow(summary), 3L)
  expect_s3_class(plot_dtw(fit), "ggplot")

  pred <- predict_topic_model(fit, dfm[1:2, ])
  expect_equal(pred$doc_id, c("doc1", "doc2"))
  expect_true(all(c("Topic001", "Topic002", "Topic003") %in% names(pred)))
})

test_that("STM prevalence fits use aligned metadata and limit prediction", {
  skip_if_not_installed("stm")
  dfm <- make_stm_dfm()
  meta <- data.frame(
    doc_id = rev(quanteda::docnames(dfm)),
    sector = rev(quanteda::docvars(dfm, "sector"))
  )
  fit <- suppressWarnings(fit_topic_model(
    dfm,
    engine = "stm",
    model = "stm",
    k = 3,
    control = list(fit = list(
      prevalence = ~sector,
      data = meta,
      seed = 1L,
      max.em.its = 5L,
      init.type = "Spectral",
      verbose = FALSE
    ))
  ))

  expect_s3_class(fit, "nlp_topic_fit")
  expect_false(is.null(fit$model_object$settings$covariates$formula))
  expect_error(
    predict_topic_model(fit, dfm[1:2, ]),
    "STM prediction for prevalence-covariate fits is not supported in v0.9.4"
  )

  eval <- evaluate_topic_model(
    fit,
    training = dfm,
    newdata = dfm[1:2, ],
    metrics = c("diversity", "held_out_perplexity", "train_perplexity")
  )
  expect_true(eval[metric == "diversity", supported])
  expect_false(eval[metric == "held_out_perplexity", supported])
  expect_true(eval[metric == "train_perplexity", supported])
})

test_that("raw STM objects can be adopted and queried", {
  skip_if_not_installed("stm")
  dfm <- make_stm_dfm()
  stm_input <- quanteda::convert(dfm, to = "stm")
  raw <- suppressWarnings(stm::stm(
    documents = stm_input$documents,
    vocab = stm_input$vocab,
    K = 3,
    seed = 1L,
    max.em.its = 5L,
    init.type = "Spectral",
    verbose = FALSE
  ))

  fit <- as_nlp_topic_fit(raw, doc_ids = quanteda::docnames(dfm))
  expect_identical(fit$engine, "stm")
  expect_equal(fit$doc_ids, quanteda::docnames(dfm))
  expect_equal(nrow(get_dtw(raw)), quanteda::ndoc(dfm))
  expect_equal(nrow(get_tww(raw)), 3L)
  expect_equal(nrow(get_top_terms(fit, n = 2)), 6L)
})

test_that("STM participates in K selection and stability diagnostics", {
  skip_if_not_installed("stm")
  dfm <- make_stm_dfm()

  selection <- suppressWarnings(select_k_topics(
    dfm,
    engine = "stm",
    model = "stm",
    k_grid = 2:3,
    metrics = c("diversity", "exclusivity"),
    holdout = 0,
    seed = 1L,
    control = list(fit = list(max.em.its = 3L, init.type = "Spectral", verbose = FALSE))
  ))
  expect_s3_class(selection, "nlp_k_selection")
  expect_equal(sort(unique(selection$k)), 2:3)

  stability <- assess_topic_stability(
    dfm,
    engine = "stm",
    model = "stm",
    k = 3,
    seeds = c(11L, 12L),
    control = list(fit = list(max.em.its = 3L, init.type = "Spectral", verbose = FALSE)),
    return_fits = TRUE
  )
  fits <- attr(stability, "fits")
  expect_equal(vapply(fits, function(x) x$backend_control$fit$seed, integer(1)), c(11L, 12L))
  expect_equal(unique(stability$engine), "stm")
})
