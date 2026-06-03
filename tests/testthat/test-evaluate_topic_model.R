# Helpers shared across evaluate_topic_model tests
make_eval_dtm <- function() {
  # 6 docs x 4 terms, same fixture as test-topic_model_api.R
  x <- Matrix::Matrix(
    matrix(
      c(2, 1, 0, 0,
        1, 1, 1, 0,
        0, 1, 2, 1,
        0, 0, 1, 2,
        1, 0, 1, 1,
        1, 2, 0, 1),
      nrow = 6, byrow = TRUE
    ),
    sparse = TRUE
  )
  x <- methods::as(x, "dgCMatrix")
  rownames(x) <- paste0("doc", 1:6)
  colnames(x) <- paste0("term", 1:4)
  x
}

make_eval_newdata <- function() {
  x <- Matrix::Matrix(
    matrix(c(1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1), nrow = 3, byrow = TRUE),
    sparse = TRUE
  )
  x <- methods::as(x, "dgCMatrix")
  rownames(x) <- paste0("new", 1:3)
  colnames(x) <- paste0("term", 1:4)
  x
}

make_eval_fit <- function() {
  skip_if_not_installed("text2vec")
  fit_topic_model(
    make_eval_dtm(), engine = "text2vec", model = "lda", k = 2,
    control = list(
      model = list(doc_topic_prior = 0.1, topic_word_prior = 0.01),
      fit   = list(n_iter = 50, progressbar = FALSE, convergence_tol = -1)
    )
  )
}

# ---- Input validation -------------------------------------------------------

test_that("evaluate_topic_model rejects non-nlp_topic_fit input", {
  expect_error(evaluate_topic_model(list()), "nlp_topic_fit", fixed = FALSE)
})

test_that("evaluate_topic_model rejects invalid top_n", {
  skip_if_not_installed("text2vec")
  fit <- make_eval_fit()
  expect_error(evaluate_topic_model(fit, top_n = 0), "top_n")
  expect_error(evaluate_topic_model(fit, top_n = 1.5), "top_n")
  expect_error(evaluate_topic_model(fit, top_n = NA_integer_), "top_n")
  expect_error(evaluate_topic_model(fit, top_n = Inf), "top_n")
  expect_error(evaluate_topic_model(fit, top_n = "two"), "top_n")
})

test_that("evaluate_topic_model rejects invalid epsilon", {
  skip_if_not_installed("text2vec")
  fit <- make_eval_fit()
  expect_error(evaluate_topic_model(fit, epsilon = 0), "epsilon")
  expect_error(evaluate_topic_model(fit, epsilon = -1), "epsilon")
  expect_error(evaluate_topic_model(fit, epsilon = NA_real_), "epsilon")
  expect_error(evaluate_topic_model(fit, epsilon = Inf), "epsilon")
})

test_that("evaluate_topic_model rejects unknown metric names", {
  skip_if_not_installed("text2vec")
  fit <- make_eval_fit()
  expect_error(evaluate_topic_model(fit, metrics = c("diversity", "foo")), "foo")
  expect_error(evaluate_topic_model(fit, metrics = character(0L)), "metrics")
  expect_error(evaluate_topic_model(fit, metrics = NA_character_), "metrics")
})

test_that("evaluate_topic_model rejects invalid level", {
  skip_if_not_installed("text2vec")
  fit <- make_eval_fit()
  expect_error(evaluate_topic_model(fit, metrics = "diversity", level = "document"),
               "should be one of")
})

# ---- Output schema ----------------------------------------------------------

test_that("evaluate_topic_model returns correct column schema", {
  skip_if_not_installed("text2vec")
  fit  <- make_eval_fit()
  dtm  <- make_eval_dtm()
  result <- evaluate_topic_model(fit, training = dtm,
                                  metrics = c("diversity", "exclusivity",
                                              "coherence_npmi"))
  expect_true(data.table::is.data.table(result))
  expect_named(result, c("metric", "level", "topic_id", "value", "supported"),
               ignore.order = FALSE)
  expect_type(result$metric,    "character")
  expect_type(result$level,     "character")
  expect_type(result$topic_id,  "character")
  expect_type(result$value,     "double")
  expect_type(result$supported, "logical")
})

test_that("evaluate_topic_model rows are ordered by metric then level then topic_id", {
  skip_if_not_installed("text2vec")
  fit    <- make_eval_fit()
  dtm    <- make_eval_dtm()
  result <- evaluate_topic_model(fit, training = dtm,
                                  metrics = c("coherence_npmi", "exclusivity"),
                                  level = "all")
  expect_equal(result$metric, sort(result$metric))
})

test_that("evaluate_topic_model defaults to aggregate rows", {
  skip_if_not_installed("text2vec")
  fit    <- make_eval_fit()
  dtm    <- make_eval_dtm()
  result <- evaluate_topic_model(
    fit,
    training = dtm,
    metrics = c("coherence_umass", "diversity", "exclusivity")
  )
  expect_true(all(result$level == "aggregate"))
  expect_setequal(unique(result$metric),
                  c("coherence_umass", "diversity", "exclusivity"))
})

test_that("evaluate_topic_model level controls aggregate and topic rows", {
  skip_if_not_installed("text2vec")
  fit <- make_eval_fit()
  dtm <- make_eval_dtm()

  aggregate <- evaluate_topic_model(
    fit,
    training = dtm,
    metrics = c("coherence_umass", "diversity", "exclusivity"),
    level = "aggregate"
  )
  expect_true(all(aggregate$level == "aggregate"))

  all_rows <- evaluate_topic_model(
    fit,
    training = dtm,
    metrics = c("coherence_umass", "diversity", "exclusivity"),
    level = "all"
  )
  expect_true("aggregate" %in% all_rows$level)
  expect_true("topic" %in% all_rows$level)

  expect_warning(
    topic <- evaluate_topic_model(
      fit,
      training = dtm,
      metrics = c("coherence_umass", "diversity", "exclusivity"),
      level = "topic"
    ),
    "do not have topic-level rows"
  )
  expect_true(all(topic$level == "topic"))
  expect_setequal(unique(topic$metric), c("coherence_umass", "exclusivity"))
})

# ---- Diversity metric -------------------------------------------------------

test_that("diversity is a scalar aggregate in (0, 1]", {
  skip_if_not_installed("text2vec")
  fit    <- make_eval_fit()
  result <- evaluate_topic_model(fit, metrics = "diversity")
  expect_equal(nrow(result), 1L)
  expect_equal(result$metric,   "diversity")
  expect_equal(result$level,    "aggregate")
  expect_true(is.na(result$topic_id))
  expect_true(result$value > 0 && result$value <= 1)
  expect_true(result$supported)
})

test_that("diversity equals 1 when all topics have non-overlapping top terms", {
  skip_if_not_installed("text2vec")
  # 2 topics, 4 terms, top_n = 2 -> each topic owns 2 unique terms
  # Force this by constructing a TWW where topics are perfectly separated
  dtm <- Matrix::sparseMatrix(
    i = c(1, 1, 2, 2, 3, 3, 4, 4),
    j = c(1, 2, 1, 2, 3, 4, 3, 4),
    x = rep(1, 8),
    dims = c(4L, 4L),
    dimnames = list(paste0("d", 1:4), paste0("t", 1:4))
  )
  fit <- fit_topic_model(dtm, engine = "text2vec", model = "lda", k = 2,
                          control = list(fit = list(n_iter = 1, progressbar = FALSE)))
  # Override tww to guarantee perfect separation
  fit$tww[1, ] <- c(0.9, 0.09, 0.005, 0.005)
  fit$tww[2, ] <- c(0.005, 0.005, 0.9, 0.09)

  result <- evaluate_topic_model(fit, metrics = "diversity", top_n = 2L)
  expect_equal(result$value, 1)
})

test_that("diversity denominator uses available top-term slots", {
  tww <- matrix(
    c(0.5, 0.3, 0.2),
    nrow = 1L,
    dimnames = list("Topic001", c("a", "b", "c"))
  )
  fit <- structure(
    list(tww = tww, dtw = NULL, vocab = colnames(tww),
         engine = "test", model = "test", model_object = NULL),
    class = c("nlp_topic_fit", "list")
  )

  result <- evaluate_topic_model(fit, metrics = "diversity", top_n = 10L)
  expect_equal(result$value, 1)
})

# ---- Exclusivity metric -----------------------------------------------------

test_that("exclusivity returns topic and aggregate rows", {
  skip_if_not_installed("text2vec")
  fit    <- make_eval_fit()
  result <- evaluate_topic_model(fit, metrics = "exclusivity", level = "all")
  expect_true("topic" %in% result$level)
  expect_true("aggregate"   %in% result$level)
  topic_rows <- result[result$level == "topic", ]
  expect_equal(nrow(topic_rows), 2L)  # k = 2 topics
  expect_true(all(result$supported))
  # Overall must equal mean of per-topic values
  aggregate <- result[result$level == "aggregate", ]$value
  per_vals <- result[result$level == "topic", ]$value
  expect_equal(aggregate, mean(per_vals), tolerance = 1e-9)
})

test_that("exclusivity values are in (0, 1]", {
  skip_if_not_installed("text2vec")
  fit    <- make_eval_fit()
  result <- evaluate_topic_model(fit, metrics = "exclusivity")
  expect_true(all(result$value > 0 & result$value <= 1))
})

test_that("exclusivity aggregate equals 1 when topics are perfectly exclusive", {
  skip_if_not_installed("text2vec")
  dtm <- Matrix::sparseMatrix(
    i = c(1, 2, 3, 4), j = c(1, 2, 3, 4), x = rep(1, 4),
    dims = c(4L, 4L),
    dimnames = list(paste0("d", 1:4), paste0("t", 1:4))
  )
  fit <- fit_topic_model(dtm, engine = "text2vec", model = "lda", k = 2,
                          control = list(fit = list(n_iter = 1, progressbar = FALSE)))
  # Force perfect separation
  fit$tww[1, ] <- c(0.5, 0.5, 0, 0)
  fit$tww[2, ] <- c(0, 0, 0.5, 0.5)

  result <- evaluate_topic_model(fit, metrics = "exclusivity", top_n = 2L)
  expect_equal(result[result$level == "aggregate", ]$value, 1, tolerance = 1e-9)
})

# ---- Coherence metrics ------------------------------------------------------

test_that("coherence returns topic and aggregate rows with training", {
  skip_if_not_installed("text2vec")
  fit    <- make_eval_fit()
  dtm    <- make_eval_dtm()
  result <- evaluate_topic_model(fit, training = dtm,
                                  metrics = c("coherence_npmi", "coherence_umass"),
                                  top_n   = 3L,
                                  level   = "all")
  for (m in c("coherence_npmi", "coherence_umass")) {
    sub <- result[result$metric == m, ]
    expect_true("topic" %in% sub$level)
    expect_true("aggregate"   %in% sub$level)
    expect_equal(nrow(sub[sub$level == "topic", ]), 2L)
    expect_true(all(sub$supported))
  }
})

test_that("coherence aggregate equals mean of per-topic values", {
  skip_if_not_installed("text2vec")
  fit    <- make_eval_fit()
  dtm    <- make_eval_dtm()
  for (m in c("coherence_npmi", "coherence_umass")) {
    result   <- evaluate_topic_model(fit, training = dtm, metrics = m, top_n = 3L,
                                     level = "all")
    per_vals <- result[result$level == "topic", ]$value
    aggregate  <- result[result$level == "aggregate",   ]$value
    expect_equal(aggregate, mean(per_vals, na.rm = TRUE), tolerance = 1e-9)
  }
})

test_that("coherence UMass matches hand-computed value on symmetric corpus", {
  skip_if_not_installed("text2vec")
  # 4 docs x 3 terms; all pairs equally co-occurring (same fixture as test-coherence.R)
  dtm <- Matrix::sparseMatrix(
    i = c(1, 1, 2, 2, 3, 3, 4, 4, 4),
    j = c(1, 2, 1, 3, 2, 3, 1, 2, 3),
    x = rep(1, 9),
    dims = c(4L, 3L),
    dimnames = list(paste0("doc", 1:4), c("a", "b", "c"))
  )
  fit <- fit_topic_model(dtm, engine = "text2vec", model = "lda", k = 2,
                          control = list(fit = list(n_iter = 1, progressbar = FALSE)))
  # Override TWW so ordering is deterministic: topic1 a>b>c, topic2 b>c>a
  fit$tww[1, ] <- c(0.6, 0.3, 0.1)
  fit$tww[2, ] <- c(0.1, 0.6, 0.3)
  fit$vocab    <- c("a", "b", "c")

  result <- evaluate_topic_model(fit, training = dtm,
                                  metrics = "coherence_umass", top_n = 3L,
                                  epsilon = 1e-12, level = "all")
  expected_umass <- log(2 / 3)
  per_vals <- result[result$level == "topic", ]$value
  expect_equal(per_vals[1], expected_umass, tolerance = 1e-9)
  expect_equal(per_vals[2], expected_umass, tolerance = 1e-9)
})

test_that("coherence is unsupported (with warning) when training is NULL", {
  skip_if_not_installed("text2vec")
  fit <- make_eval_fit()
  expect_warning(
    result <- evaluate_topic_model(fit, metrics = "coherence_umass"),
    "require 'training'"
  )
  expect_true(all(!result$supported))
  expect_true(all(is.na(result$value)))
})

# ---- Likelihood metrics -----------------------------------------------------

test_that("train_perplexity and train_nll return aggregate scalars with training", {
  skip_if_not_installed("text2vec")
  fit <- make_eval_fit()
  dtm <- make_eval_dtm()
  result <- evaluate_topic_model(
    fit,
    training = dtm,
    metrics = c("train_perplexity", "train_nll")
  )
  for (m in c("train_perplexity", "train_nll")) {
    sub <- result[result$metric == m, ]
    expect_equal(nrow(sub), 1L)
    expect_equal(sub$level, "aggregate")
    expect_true(is.na(sub$topic_id))
    expect_true(sub$supported)
    expect_true(is.finite(sub$value) && sub$value > 0)
  }
})

test_that("train_perplexity equals exp(train_nll)", {
  skip_if_not_installed("text2vec")
  fit <- make_eval_fit()
  dtm <- make_eval_dtm()
  result <- evaluate_topic_model(
    fit,
    training = dtm,
    metrics = c("train_perplexity", "train_nll")
  )
  nll  <- result[result$metric == "train_nll", ]$value
  perp <- result[result$metric == "train_perplexity", ]$value
  expect_equal(perp, exp(nll), tolerance = 1e-9)
})

test_that("train_nll is unsupported (with warning) when training is NULL", {
  skip_if_not_installed("text2vec")
  fit <- make_eval_fit()
  expect_warning(
    result <- evaluate_topic_model(fit, metrics = "train_nll"),
    "require 'training'"
  )
  expect_false(result$supported)
  expect_true(is.na(result$value))
})

test_that("held_out_perplexity and held_out_nll return aggregate scalars with newdata", {
  skip_if_not_installed("text2vec")
  fit     <- make_eval_fit()
  newdata <- make_eval_newdata()
  result  <- evaluate_topic_model(fit, newdata = newdata,
                                   metrics = c("held_out_perplexity", "held_out_nll"))
  for (m in c("held_out_perplexity", "held_out_nll")) {
    sub <- result[result$metric == m, ]
    expect_equal(nrow(sub), 1L)
    expect_equal(sub$level, "aggregate")
    expect_true(is.na(sub$topic_id))
    expect_true(sub$supported)
    expect_true(is.finite(sub$value) && sub$value > 0)
  }
})

test_that("held_out_perplexity equals exp(held_out_nll)", {
  skip_if_not_installed("text2vec")
  fit     <- make_eval_fit()
  newdata <- make_eval_newdata()
  result  <- evaluate_topic_model(fit, newdata = newdata,
                                   metrics = c("held_out_perplexity", "held_out_nll"))
  nll  <- result[result$metric == "held_out_nll", ]$value
  perp <- result[result$metric == "held_out_perplexity", ]$value
  expect_equal(perp, exp(nll), tolerance = 1e-9)
})

test_that("held-out metrics work with unrownamed newdata", {
  skip_if_not_installed("text2vec")
  fit     <- make_eval_fit()
  newdata <- make_eval_newdata()
  rownames(newdata) <- NULL

  result <- evaluate_topic_model(
    fit,
    newdata = newdata,
    metrics = c("held_out_perplexity", "held_out_nll")
  )

  expect_true(all(result$supported))
  expect_true(all(is.finite(result$value)))
  expect_equal(
    result[metric == "held_out_perplexity", ]$value,
    exp(result[metric == "held_out_nll", ]$value),
    tolerance = 1e-9
  )
})

test_that("held_out_nll is unsupported (with warning) when newdata is NULL", {
  skip_if_not_installed("text2vec")
  fit <- make_eval_fit()
  expect_warning(
    result <- evaluate_topic_model(fit, metrics = "held_out_nll"),
    "require 'newdata'"
  )
  expect_false(result$supported)
  expect_true(is.na(result$value))
})

test_that("perplexity alias is no longer accepted", {
  skip_if_not_installed("text2vec")
  fit <- make_eval_fit()

  expect_error(
    evaluate_topic_model(fit, metrics = "perplexity"),
    "Unknown metric.*perplexity"
  )
})

# ---- Mixed: all metrics together -------------------------------------------

test_that("all metrics can be computed together and return correct row count", {
  skip_if_not_installed("text2vec")
  fit     <- make_eval_fit()
  dtm     <- make_eval_dtm()
  newdata <- make_eval_newdata()
  result  <- evaluate_topic_model(fit, training = dtm, newdata = newdata,
                                   top_n = 3L)
  expect_equal(nrow(result), 8L)
  expect_true(all(result$level == "aggregate"))
  expect_true(all(result$supported))
  expect_true(all(is.finite(result$value)))
})

test_that("all metrics can return all aggregate and topic rows", {
  skip_if_not_installed("text2vec")
  fit     <- make_eval_fit()
  dtm     <- make_eval_dtm()
  newdata <- make_eval_newdata()
  result  <- evaluate_topic_model(fit, training = dtm, newdata = newdata,
                                   top_n = 3L, level = "all")
  expect_equal(nrow(result), 14L)
  expect_true("aggregate" %in% result$level)
  expect_true("topic" %in% result$level)
  expect_true(all(result$supported))
  expect_true(all(is.finite(result$value)))
})

test_that("evaluate_topic_model works with topicmodels engine", {
  skip_if_not_installed("topicmodels")
  dtm <- make_eval_dtm()
  fit <- fit_topic_model(
    dtm, engine = "topicmodels", model = "lda", k = 2,
    control = list(fit = list(
      seed = 1L, em = list(iter.max = 5L), var = list(iter.max = 5L)
    ))
  )
  newdata <- make_eval_newdata()
  result  <- evaluate_topic_model(fit, training = dtm, newdata = newdata,
                                   metrics = c("diversity", "exclusivity",
                                               "coherence_umass",
                                               "held_out_perplexity",
                                               "held_out_nll", "train_nll"),
                                   top_n = 3L)
  expect_true(all(result$supported))
  expect_true(all(is.finite(result$value)))
})

test_that("evaluate_topic_model works with seededlda engine", {
  skip_if_not_installed("seededlda")
  dtm <- make_eval_dtm()
  fit <- fit_topic_model(
    dtm, engine = "seededlda", model = "lda", k = 2,
    control = list(fit = list(max_iter = 100L, verbose = FALSE))
  )
  result <- evaluate_topic_model(fit,
                                  training = dtm,
                                  metrics  = c("diversity", "exclusivity",
                                               "coherence_npmi"),
                                  top_n = 3L)
  expect_true(all(result$supported))
  expect_true(all(is.finite(result$value)))
})
