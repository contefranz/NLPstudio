make_legacy_warp <- function(theta = NULL, phi = NULL, model_tww = NULL,
                             n_topics = NULL) {
  if (is.null(theta)) {
    theta <- data.table::data.table(
      doc_id = c("doc1", "doc2", "doc3"),
      Topic1 = c(0.7, 0.2, 0.4),
      Topic2 = c(0.3, 0.8, 0.6)
    )
  }
  if (is.null(phi)) {
    phi <- data.table::data.table(
      revenue = c(0.6, 0.2),
      risk = c(0.3, 0.7),
      audit = c(0.1, 0.1)
    )
  }
  if (is.null(model_tww) && !is.null(phi)) {
    model_tww <- as.matrix(phi)
  }
  if (is.null(n_topics) && !is.null(phi)) {
    n_topics <- nrow(phi)
  }
  structure(
    list(
      lda_object = structure(
        list(topic_word_distribution = model_tww, n_topics = n_topics),
        class = "WarpLDA"
      ),
      theta = theta,
      phi = phi
    ),
    class = "list"
  )
}

test_that("as_nlp_topic_fit converts complete legacy WarpLDA output", {
  old <- make_legacy_warp()
  fit <- as_nlp_topic_fit(old)

  expect_s3_class(fit, "nlp_topic_fit")
  expect_equal(fit$engine, "text2vec")
  expect_equal(fit$model, "lda")
  expect_true(inherits(fit$model_object, "WarpLDA"))
  expect_equal(rownames(fit$dtw), c("doc1", "doc2", "doc3"))
  expect_equal(colnames(fit$dtw), c("Topic001", "Topic002"))
  expect_equal(rownames(fit$tww), c("Topic001", "Topic002"))
  expect_equal(colnames(fit$tww), c("revenue", "risk", "audit"))
  expect_equal(fit$doc_ids, c("doc1", "doc2", "doc3"))
  expect_equal(fit$vocab, c("revenue", "risk", "audit"))
})

test_that("converted legacy WarpLDA output works with extraction and plotting helpers", {
  fit <- as_nlp_topic_fit(make_legacy_warp())

  dtw <- get_dtw(fit)
  tww <- get_tww(fit)
  terms <- get_top_terms(fit, n = 2)

  expect_named(dtw, c(
    "doc_id", "Topic001", "Topic002",
    "topic_max_id", "topic_max_int", "topic_max_value"
  ))
  expect_named(tww, c("topic_id", "revenue", "risk", "audit"))
  expect_equal(terms$topic, c("Topic001", "Topic001", "Topic002", "Topic002"))
  if (requireNamespace("tidytext", quietly = TRUE)) {
    expect_s3_class(plot_top_terms(terms), "ggplot")
  }
  expect_s3_class(plot_dtw(fit), "ggplot")
})

test_that("as_nlp_topic_fit is idempotent for current topic fits", {
  fit <- as_nlp_topic_fit(make_legacy_warp())
  expect_identical(as_nlp_topic_fit(fit), fit)
})

test_that("legacy theta IDs can be supplied or recovered from rn", {
  theta <- data.table::data.table(
    rn = c("a", "b"),
    V1 = c(0.2, 0.9),
    V2 = c(0.8, 0.1)
  )
  phi <- data.table::data.table(t1 = c(0.5, 0.4), t2 = c(0.5, 0.6))

  fit <- as_nlp_topic_fit(make_legacy_warp(theta = theta, phi = phi))
  expect_equal(rownames(fit$dtw), c("a", "b"))

  fit_override <- as_nlp_topic_fit(
    make_legacy_warp(theta = theta[, -"rn"], phi = phi),
    doc_ids = c("doc_a", "doc_b")
  )
  expect_equal(rownames(fit_override$dtw), c("doc_a", "doc_b"))
})

test_that("legacy phi can be recovered from the WarpLDA model object", {
  theta <- data.table::data.table(
    doc_id = c("doc1", "doc2"),
    Topic1 = c(0.6, 0.2),
    Topic2 = c(0.4, 0.8)
  )
  model_tww <- matrix(
    c(0.8, 0.2, 0.3, 0.7),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(NULL, c("term_a", "term_b"))
  )
  old <- make_legacy_warp(theta = theta, model_tww = model_tww, n_topics = 2L)
  old$phi <- NULL

  fit <- as_nlp_topic_fit(old)

  expect_equal(colnames(fit$tww), c("term_a", "term_b"))
  expect_equal(fit$vocab, c("term_a", "term_b"))
})

test_that("partial legacy WarpLDA objects warn but remain convertible", {
  no_theta <- make_legacy_warp(theta = NULL)
  no_theta$theta <- NULL
  expect_warning(
    fit_no_theta <- as_nlp_topic_fit(no_theta),
    "does not contain theta"
  )
  expect_null(fit_no_theta$dtw)
  expect_error(get_dtw(fit_no_theta), "does not contain cached DTW")

  no_phi <- make_legacy_warp(n_topics = 2L)
  no_phi$phi <- NULL
  no_phi$lda_object$topic_word_distribution <- NULL
  expect_warning(
    fit_no_phi <- as_nlp_topic_fit(no_phi),
    "does not contain recoverable phi"
  )
  expect_null(fit_no_phi$tww)
  expect_error(get_tww(fit_no_phi), "do not contain recoverable TWW")
})

test_that("as_nlp_topic_fit validates legacy WarpLDA shapes", {
  expect_error(as_nlp_topic_fit(list()), "legacy warp_lda")
  expect_error(as_nlp_topic_fit("bad"), "cannot be converted")
  expect_error(as_nlp_topic_fit(make_legacy_warp(), warn_partial = NA), "warn_partial")
  expect_error(as_nlp_topic_fit(make_legacy_warp(), k = 0), "single positive integer")

  bad_theta <- make_legacy_warp()
  bad_theta$theta <- data.table::data.table(
    doc_id = c("doc1", "doc2"),
    Topic1 = c(0.5, 0.4),
    Topic2 = c("bad", "worse")
  )
  expect_error(as_nlp_topic_fit(bad_theta), "theta topic columns must be numeric")
  bad_theta_empty <- make_legacy_warp()
  bad_theta_empty$theta <- data.table::data.table(doc_id = c("doc1", "doc2"))
  expect_error(as_nlp_topic_fit(bad_theta_empty), "theta must contain topic columns")

  bad_phi <- make_legacy_warp()
  bad_phi$phi <- data.table::data.table(revenue = c(0.6, 0.2), risk = c("bad", "worse"))
  expect_error(as_nlp_topic_fit(bad_phi), "phi term columns must be numeric")
  bad_phi_empty <- make_legacy_warp()
  bad_phi_empty$phi <- matrix(numeric(), nrow = 2, ncol = 0)
  expect_error(as_nlp_topic_fit(bad_phi_empty), "phi must contain term columns")
  bad_phi_matrix <- make_legacy_warp()
  bad_phi_matrix$phi <- matrix(c("a", "b", "c", "d"), nrow = 2)
  expect_error(as_nlp_topic_fit(bad_phi_matrix), "phi must be numeric")

  expect_error(
    as_nlp_topic_fit(make_legacy_warp(), doc_ids = c("doc1", "doc2")),
    "one value per theta row"
  )
  expect_error(
    as_nlp_topic_fit(make_legacy_warp(), vocab = c("a", "b")),
    "one value per phi column"
  )
  no_theta <- make_legacy_warp()
  no_theta$theta <- NULL
  expect_error(
    as_nlp_topic_fit(no_theta, doc_ids = c("doc1", "doc2", "doc3")),
    "doc_ids.*only when legacy theta is available"
  )
  no_phi <- make_legacy_warp()
  no_phi$phi <- NULL
  no_phi$lda_object$topic_word_distribution <- NULL
  expect_error(
    as_nlp_topic_fit(no_phi, vocab = c("a", "b", "c")),
    "vocab.*only when legacy phi is available"
  )
})

test_that("as_nlp_topic_fit rejects inconsistent topic counts", {
  old <- make_legacy_warp(n_topics = 3L)
  expect_error(
    as_nlp_topic_fit(old),
    "disagree on the number of topics"
  )

  expect_error(
    as_nlp_topic_fit(make_legacy_warp(), k = 3L),
    "disagree on the number of topics"
  )

  old_unknown_k <- make_legacy_warp()
  old_unknown_k$theta <- NULL
  old_unknown_k$phi <- NULL
  old_unknown_k$lda_object$topic_word_distribution <- NULL
  old_unknown_k$lda_object$n_topics <- NULL
  expect_error(
    as_nlp_topic_fit(old_unknown_k),
    "Could not infer the topic count"
  )
})

test_that("docvars and doc_data can be stored on converted legacy fits", {
  docvars <- data.table::data.table(year = c(2024, 2025, 2026))
  doc_data <- data.table::data.table(
    doc_id = c("doc1", "doc2", "doc3"),
    source = c("a", "b", "c"),
    text = c("alpha text", "beta text", "gamma text")
  )
  fit <- as_nlp_topic_fit(
    make_legacy_warp(),
    docvars = docvars,
    doc_data = doc_data
  )

  dtw <- get_dtw(fit, docvars = TRUE, include_text = TRUE)
  expect_equal(dtw$year, c(2024, 2025, 2026))
  expect_equal(dtw$source, c("a", "b", "c"))
  expect_equal(dtw$text, c("alpha text", "beta text", "gamma text"))

  expect_error(
    as_nlp_topic_fit(make_legacy_warp(), docvars = "bad"),
    "docvars"
  )
  expect_error(
    as_nlp_topic_fit(make_legacy_warp(), doc_data = data.table::data.table(source = "x")),
    "doc_data must contain"
  )

  expect_error(
    as_nlp_topic_fit(make_legacy_warp(), docvars = data.table::data.table(x = 1:2)),
    "one row per document"
  )

  no_dtw <- make_legacy_warp()
  no_dtw$theta <- NULL
  fit_from_docvars <- suppressWarnings(as_nlp_topic_fit(
    no_dtw,
    docvars = data.table::data.table(doc_id = c("a", "b", "c"), year = 1:3),
    warn_partial = FALSE
  ))
  expect_equal(fit_from_docvars$doc_ids, c("a", "b", "c"))

  fit_from_doc_data <- suppressWarnings(as_nlp_topic_fit(
    no_dtw,
    doc_data = data.table::data.table(doc_id = c("x", "y", "z"), source = letters[1:3]),
    warn_partial = FALSE
  ))
  expect_equal(fit_from_doc_data$doc_ids, c("x", "y", "z"))
})

test_that("matrix inputs and explicit controls are preserved", {
  theta <- matrix(
    c(0.1, 0.9, 0.8, 0.2),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("row_a", "row_b"), NULL)
  )
  phi <- matrix(
    c(0.3, 0.7, 0.6, 0.4),
    nrow = 2,
    byrow = TRUE
  )
  old <- make_legacy_warp(theta = theta, phi = phi, n_topics = 2L)

  fit <- as_nlp_topic_fit(
    old,
    vocab = c("alpha", "beta"),
    control = list(model = list(doc_topic_prior = 0.2, topic_word_prior = 0.03))
  )

  expect_equal(rownames(fit$dtw), c("row_a", "row_b"))
  expect_equal(fit$vocab, c("alpha", "beta"))
  hp <- get_topic_hyperparameters(fit)
  expect_equal(hp[parameter == "alpha", value][[1]], 0.2)
  expect_equal(hp[parameter == "beta", value][[1]], 0.03)
})

test_that("real text2vec WarpLDA objects can be wrapped without refitting", {
  skip_if_not_installed("text2vec")

  dtm <- methods::as(
    Matrix::Matrix(
      matrix(
        c(2, 1, 0,
          1, 0, 2,
          0, 2, 1,
          1, 1, 1),
        nrow = 4,
        byrow = TRUE
      ),
      sparse = TRUE
    ),
    "dgCMatrix"
  )
  rownames(dtm) <- paste0("doc", 1:4)
  colnames(dtm) <- paste0("term", 1:3)

  lda <- text2vec::LDA$new(n_topics = 2L, doc_topic_prior = 0.1, topic_word_prior = 0.001)
  theta <- lda$fit_transform(dtm, n_iter = 10L, progressbar = FALSE)
  old <- list(
    lda_object = lda,
    theta = data.table::as.data.table(theta, keep.rownames = TRUE),
    phi = data.table::as.data.table(lda$topic_word_distribution)
  )
  data.table::setnames(old$theta, "rn", "doc_id")

  fit <- as_nlp_topic_fit(old)

  expect_s3_class(fit, "nlp_topic_fit")
  expect_equal(nrow(get_dtw(fit)), 4L)
  expect_equal(nrow(get_tww(fit)), 2L)
  pred <- predict_topic_model(fit, dtm, control = list(n_iter = 5L, progressbar = FALSE))
  expect_equal(nrow(pred), 4L)
})
