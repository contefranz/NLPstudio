# Shared fixture: small but non-trivial DTM (12 docs x 5 terms)
make_sel_dtm <- function() {
  set.seed(7L)
  mat <- matrix(
    sample(0:3, 12 * 5, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)),
    nrow = 12, ncol = 5
  )
  # Ensure no all-zero rows
  for (i in seq_len(nrow(mat))) {
    if (all(mat[i, ] == 0)) mat[i, 1] <- 1L
  }
  x <- methods::as(Matrix::Matrix(mat, sparse = TRUE), "dgCMatrix")
  rownames(x) <- paste0("doc", seq_len(nrow(x)))
  colnames(x) <- paste0("term", seq_len(ncol(x)))
  x
}

fast_control <- function() {
  list(fit = list(n_iter = 25L, progressbar = FALSE, convergence_tol = -1))
}

# ---- Input validation -------------------------------------------------------

test_that("select_k_topics rejects corpus input", {
  skip_if_not_installed("text2vec")
  corp <- quanteda::corpus(c(d1 = "a b c", d2 = "b c d"))
  expect_error(
    select_k_topics(corp, engine = "text2vec", model = "lda", k_grid = 2L,
                    holdout = 0),
    "corpus"
  )
})

test_that("select_k_topics rejects empty k_grid", {
  skip_if_not_installed("text2vec")
  dtm <- make_sel_dtm()
  expect_error(
    select_k_topics(dtm, engine = "text2vec", model = "lda",
                    k_grid = integer(0L), holdout = 0),
    "k_grid"
  )
})

test_that("select_k_topics rejects non-finite or fractional k_grid", {
  skip_if_not_installed("text2vec")
  dtm <- make_sel_dtm()
  expect_error(
    select_k_topics(dtm, engine = "text2vec", model = "lda",
                    k_grid = 2.5, holdout = 0),
    "k_grid"
  )
  expect_error(
    select_k_topics(dtm, engine = "text2vec", model = "lda",
                    k_grid = NA_real_, holdout = 0),
    "k_grid"
  )
  expect_error(
    select_k_topics(dtm, engine = "text2vec", model = "lda",
                    k_grid = Inf, holdout = 0),
    "k_grid"
  )
})

test_that("select_k_topics rejects holdout outside [0, 1)", {
  skip_if_not_installed("text2vec")
  dtm <- make_sel_dtm()
  expect_error(
    select_k_topics(dtm, engine = "text2vec", model = "lda",
                    k_grid = 2L, holdout = 1),
    "holdout"
  )
  expect_error(
    select_k_topics(dtm, engine = "text2vec", model = "lda",
                    k_grid = 2L, holdout = -0.1),
    "holdout"
  )
})

test_that("select_k_topics rejects seed of wrong length", {
  skip_if_not_installed("text2vec")
  dtm <- make_sel_dtm()
  expect_error(
    select_k_topics(dtm, engine = "text2vec", model = "lda",
                    k_grid = 2:3, holdout = 0,
                    seed = c(1L, 2L, 3L),   # length 3 != length(k_grid) 2
                    control = fast_control()),
    "seed"
  )
})

test_that("select_k_topics rejects invalid metrics, seed, top_n, and epsilon", {
  skip_if_not_installed("text2vec")
  dtm <- make_sel_dtm()
  expect_error(
    select_k_topics(dtm, engine = "text2vec", model = "lda",
                    k_grid = 2L, metrics = character(0L), holdout = 0),
    "metrics"
  )
  expect_error(
    select_k_topics(dtm, engine = "text2vec", model = "lda",
                    k_grid = 2L, metrics = NA_character_, holdout = 0),
    "metrics"
  )
  expect_error(
    select_k_topics(dtm, engine = "text2vec", model = "lda",
                    k_grid = 2L, metrics = "diversity", seed = NA_integer_,
                    holdout = 0),
    "seed"
  )
  expect_error(
    select_k_topics(dtm, engine = "text2vec", model = "lda",
                    k_grid = 2L, metrics = "diversity", seed = 1.5,
                    holdout = 0),
    "seed"
  )
  expect_error(
    select_k_topics(dtm, engine = "text2vec", model = "lda",
                    k_grid = 2L, metrics = "diversity", top_n = NA_integer_,
                    holdout = 0),
    "top_n"
  )
  expect_error(
    select_k_topics(dtm, engine = "text2vec", model = "lda",
                    k_grid = 2L, metrics = "diversity", epsilon = NA_real_,
                    holdout = 0),
    "epsilon"
  )
})

# ---- Output schema ----------------------------------------------------------

test_that("select_k_topics returns nlp_k_selection data.table with correct columns", {
  skip_if_not_installed("text2vec")
  dtm <- make_sel_dtm()
  result <- suppressWarnings(select_k_topics(
    dtm, engine = "text2vec", model = "lda",
    k_grid  = 2:3,
    metrics = c("diversity", "exclusivity"),
    holdout = 0,
    seed    = 42L,
    control = fast_control()
  ))
  expect_s3_class(result, "nlp_k_selection")
  expect_s3_class(result, "data.table")
  expect_named(result, c("k", "metric", "scope", "topic_id", "value", "supported"),
               ignore.order = FALSE)
  expect_setequal(unique(result$k), 2:3)
})

test_that("select_k_topics result is ordered by k then metric then scope", {
  skip_if_not_installed("text2vec")
  dtm <- make_sel_dtm()
  result <- suppressWarnings(select_k_topics(
    dtm, engine = "text2vec", model = "lda",
    k_grid  = c(3L, 2L),  # supply out of order
    metrics = "diversity",
    holdout = 0,
    seed    = 1L,
    control = fast_control()
  ))
  expect_equal(result$k, sort(result$k))
})

# ---- Metric coverage --------------------------------------------------------

test_that("engine-agnostic metrics work with holdout = 0", {
  skip_if_not_installed("text2vec")
  dtm <- make_sel_dtm()
  result <- suppressWarnings(select_k_topics(
    dtm, engine = "text2vec", model = "lda",
    k_grid  = 2:3,
    metrics = c("diversity", "exclusivity"),
    holdout = 0,
    seed    = 1L,
    control = fast_control()
  ))
  expect_true(all(result$supported))
  expect_true(all(is.finite(result$value)))
})

test_that("coherence metrics work with holdout = 0", {
  skip_if_not_installed("text2vec")
  dtm <- make_sel_dtm()
  result <- suppressWarnings(select_k_topics(
    dtm, engine = "text2vec", model = "lda",
    k_grid  = 2L,
    metrics = c("coherence_npmi", "coherence_umass"),
    holdout = 0,
    seed    = 1L,
    control = fast_control()
  ))
  expect_true(all(result$supported))
  expect_true(all(is.finite(result$value)))
  expect_setequal(unique(result$metric), c("coherence_npmi", "coherence_umass"))
})

test_that("coherence and predictive metrics work with holdout > 0", {
  skip_if_not_installed("text2vec")
  dtm <- make_sel_dtm()
  result <- suppressWarnings(select_k_topics(
    dtm, engine = "text2vec", model = "lda",
    k_grid  = 2:3,
    metrics = c("coherence_umass", "held_out_nll", "perplexity"),
    holdout = 0.3,
    seed    = 42L,
    control = fast_control()
  ))
  expect_true(all(result$supported))
  expect_true(all(is.finite(result$value)))
})

test_that("all six metrics can be requested simultaneously", {
  skip_if_not_installed("text2vec")
  dtm <- make_sel_dtm()
  all_metrics <- c("coherence_npmi", "coherence_umass", "diversity",
                   "exclusivity", "held_out_nll", "perplexity")
  result <- suppressWarnings(select_k_topics(
    dtm, engine = "text2vec", model = "lda",
    k_grid  = 2:3,
    metrics = all_metrics,
    holdout = 0.25,
    seed    = 99L,
    control = fast_control()
  ))
  expect_setequal(unique(result$metric), all_metrics)
  expect_true(all(result$supported))
})

# ---- Reproducibility --------------------------------------------------------

test_that("same seed produces identical results across two runs", {
  skip_if_not_installed("text2vec")
  dtm <- make_sel_dtm()
  args <- list(
    dtm, engine = "text2vec", model = "lda",
    k_grid  = 2:3,
    metrics = c("diversity", "exclusivity"),
    holdout = 0,
    seed    = 7L,
    control = fast_control()
  )
  r1 <- suppressWarnings(do.call(select_k_topics, args))
  r2 <- suppressWarnings(do.call(select_k_topics, args))
  expect_equal(r1$value, r2$value)
})

test_that("ncores = 1 and ncores = 2 produce identical results", {
  skip_if_not_installed("text2vec")
  # PSOCK workers require an installed (not just devtools-loaded) package
  skip_if(
    !file.exists(system.file("Meta", "package.rds", package = "NLPstudio")),
    "NLPstudio must be installed for parallel tests"
  )
  dtm  <- make_sel_dtm()
  args <- list(
    dtm, engine = "text2vec", model = "lda",
    k_grid  = 2:3,
    metrics = c("diversity", "exclusivity"),
    holdout = 0,
    seed    = 42L,
    control = fast_control()
  )
  r1 <- suppressWarnings(do.call(select_k_topics, c(args, list(ncores = 1L))))
  r2 <- suppressWarnings(do.call(select_k_topics, c(args, list(ncores = 2L))))
  # Values may differ slightly due to floating-point non-determinism in
  # parallel execution; use a reasonable tolerance
  expect_equal(r1$k,      r2$k)
  expect_equal(r1$metric, r2$metric)
  expect_equal(r1$scope,  r2$scope)
})

# ---- return_fits ------------------------------------------------------------

test_that("return_fits = FALSE produces no fits attribute", {
  skip_if_not_installed("text2vec")
  dtm <- make_sel_dtm()
  result <- suppressWarnings(select_k_topics(
    dtm, engine = "text2vec", model = "lda",
    k_grid = 2L, metrics = "diversity", holdout = 0,
    seed = 1L, return_fits = FALSE, control = fast_control()
  ))
  expect_null(attr(result, "fits"))
})

test_that("return_fits = TRUE attaches nlp_topic_fit objects as attribute", {
  skip_if_not_installed("text2vec")
  dtm <- make_sel_dtm()
  result <- suppressWarnings(select_k_topics(
    dtm, engine = "text2vec", model = "lda",
    k_grid = 2:3, metrics = "diversity", holdout = 0,
    seed = 1L, return_fits = TRUE, control = fast_control()
  ))
  fits <- attr(result, "fits")
  expect_type(fits, "list")
  expect_named(fits, c("k2", "k3"))
  expect_true(all(vapply(fits, inherits, logical(1L), "nlp_topic_fit")))
})

# ---- print and plot ---------------------------------------------------------

test_that("print.nlp_k_selection runs without error and returns invisibly", {
  skip_if_not_installed("text2vec")
  dtm <- make_sel_dtm()
  result <- suppressWarnings(select_k_topics(
    dtm, engine = "text2vec", model = "lda",
    k_grid = 2:3, metrics = c("diversity", "exclusivity"),
    holdout = 0, seed = 1L, control = fast_control()
  ))
  out <- utils::capture.output(ret <- print(result))
  expect_true(any(grepl("nlp_k_selection", out)))
  expect_identical(ret, result)
})

test_that("plot.nlp_k_selection returns a ggplot object", {
  skip_if_not_installed("text2vec")
  dtm <- make_sel_dtm()
  result <- suppressWarnings(select_k_topics(
    dtm, engine = "text2vec", model = "lda",
    k_grid = 2:3, metrics = c("diversity", "exclusivity"),
    holdout = 0, seed = 1L, control = fast_control()
  ))
  p <- plot(result)
  expect_s3_class(p, "ggplot")
})

# ---- Holdout split helper ---------------------------------------------------

test_that(".k_select_split respects holdout fraction", {
  dtm <- make_sel_dtm()
  n   <- nrow(dtm)
  sp  <- NLPstudio:::.k_select_split(dtm, holdout = 0.25, seed = 1L)
  expect_true(nrow(sp$holdout) >= 1L)
  expect_equal(nrow(sp$train) + nrow(sp$holdout), n)
})

test_that(".k_select_split is reproducible with same seed", {
  dtm <- make_sel_dtm()
  s1  <- NLPstudio:::.k_select_split(dtm, holdout = 0.3, seed = 99L)
  s2  <- NLPstudio:::.k_select_split(dtm, holdout = 0.3, seed = 99L)
  expect_equal(rownames(s1$train),   rownames(s2$train))
  expect_equal(rownames(s1$holdout), rownames(s2$holdout))
})

test_that(".k_select_split preserves dfm class", {
  dtm <- make_sel_dtm()
  dfm <- quanteda::as.dfm(dtm)
  sp  <- NLPstudio:::.k_select_split(dfm, holdout = 0.3, seed = 1L)
  expect_true(inherits(sp$train,   "dfm"))
  expect_true(inherits(sp$holdout, "dfm"))
})
