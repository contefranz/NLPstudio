make_diag_fit <- function(tww, dtw = NULL, seed = NA_integer_,
                          engine = "synthetic", model = "lda", method = NULL,
                          docvars = NULL, doc_data = NULL) {
  tww <- as.matrix(tww)
  storage.mode(tww) <- "double"
  if (is.null(rownames(tww))) {
    rownames(tww) <- sprintf("Topic%03d", seq_len(nrow(tww)))
  }
  if (is.null(colnames(tww))) {
    colnames(tww) <- paste0("term", seq_len(ncol(tww)))
  }

  if (is.null(dtw)) {
    dtw <- matrix(
      1 / nrow(tww),
      nrow = 4L,
      ncol = nrow(tww),
      dimnames = list(paste0("doc", 1:4), rownames(tww))
    )
  } else {
    dtw <- as.matrix(dtw)
    storage.mode(dtw) <- "double"
  }

  structure(
    list(
      engine = engine,
      model = model,
      method = method,
      model_object = list(),
      dtw = dtw,
      tww = tww,
      doc_ids = rownames(dtw),
      vocab = colnames(tww),
      docvars = docvars,
      doc_data = doc_data,
      hyperparameters = data.table::data.table(parameter = "k", value = nrow(tww)),
      backend_control = list(fit = list(seed = seed)),
      call = quote(fit_topic_model())
    ),
    class = c("nlp_topic_fit", "list")
  )
}

make_diag_tww <- function() {
  matrix(
    c(0.8, 0.1, 0.1,
      0.1, 0.8, 0.1,
      0.1, 0.1, 0.8),
    nrow = 3L,
    byrow = TRUE,
    dimnames = list(sprintf("Topic%03d", 1:3), c("alpha", "beta", "gamma"))
  )
}

test_that("assess_topic_stability validates seeds and return_fits", {
  expect_error(
    assess_topic_stability(list(), seeds = 1L),
    "seeds"
  )
  expect_error(
    assess_topic_stability(list(), seeds = c(1L, 1L)),
    "unique"
  )
  expect_error(
    assess_topic_stability(list(), seeds = c(1L, 2.5)),
    "seeds"
  )
  expect_error(
    assess_topic_stability(list(), seeds = c(1L, 2L), return_fits = NA),
    "return_fits"
  )
  expect_error(
    assess_topic_stability(
      make_diag_fit(make_diag_tww()),
      seeds = c(1L, 2L)
    ),
    "engine"
  )
})

test_that("assess_topic_stability matches permuted topics in list mode", {
  ref <- make_diag_fit(make_diag_tww(), seed = 10L)
  cand_tww <- make_diag_tww()[c(2L, 3L, 1L), ]
  rownames(cand_tww) <- sprintf("Topic%03d", 1:3)
  cand <- make_diag_fit(cand_tww, seed = 11L)

  out <- assess_topic_stability(list(ref, cand), seeds = c(10L, 11L))

  expect_s3_class(out, "nlp_topic_stability")
  expect_named(
    out,
    c(
      "run_id", "seed", "reference_run_id", "reference_seed",
      "topic_id", "matched_topic_id", "similarity",
      "topic_stability", "run_stability", "aggregate_stability",
      "k", "engine", "model", "method"
    )
  )
  expect_equal(out$matched_topic_id, c("Topic003", "Topic001", "Topic002"))
  expect_equal(out$similarity, rep(1, 3), tolerance = 1e-8)
  expect_equal(unique(out$aggregate_stability), 1, tolerance = 1e-8)

  inferred <- assess_topic_stability(list(ref, cand), return_fits = TRUE)
  expect_equal(inferred$seed, rep(11L, 3L))
  expect_equal(inferred$reference_seed, rep(10L, 3L))
  expect_equal(length(attr(inferred, "fits")), 2L)
})

test_that("assess_topic_stability reports inferred missing seeds in list mode", {
  ref <- make_diag_fit(make_diag_tww(), seed = NA_integer_)
  cand <- make_diag_fit(make_diag_tww(), seed = NA_integer_)

  out <- assess_topic_stability(list(ref, cand))

  expect_true(all(is.na(out$seed)))
  expect_true(all(is.na(out$reference_seed)))
  expect_error(
    assess_topic_stability(list(ref, cand), seeds = 1:3),
    "one seed per fit"
  )
})

test_that("assess_topic_stability aligns missing and extra vocabulary", {
  ref <- make_diag_fit(
    matrix(
      c(1, 0,
        0, 1),
      nrow = 2L,
      byrow = TRUE,
      dimnames = list(c("Topic001", "Topic002"), c("alpha", "beta"))
    ),
    seed = 1L
  )
  cand <- make_diag_fit(
    matrix(
      c(1, 0,
        0, 1),
      nrow = 2L,
      byrow = TRUE,
      dimnames = list(c("Topic001", "Topic002"), c("beta", "gamma"))
    ),
    seed = 2L
  )

  out <- assess_topic_stability(list(ref, cand), seeds = c(1L, 2L))

  expect_equal(out[topic_id == "Topic001", similarity], 0, tolerance = 1e-8)
  expect_equal(out[topic_id == "Topic002", matched_topic_id], "Topic001")
  expect_equal(unique(out$aggregate_stability), 0.5, tolerance = 1e-8)
})

test_that("assess_topic_stability automatic mode changes only seed-specific controls", {
  calls <- list()
  tww <- matrix(
    c(0.7, 0.3,
      0.2, 0.8),
    nrow = 2L,
    byrow = TRUE,
    dimnames = list(c("Topic001", "Topic002"), c("alpha", "beta"))
  )

  testthat::local_mocked_bindings(
    fit_topic_model = function(x, engine, model, k, method, control, ...) {
      calls[[length(calls) + 1L]] <<- list(
        x = x,
        engine = engine,
        model = model,
        k = k,
        method = method,
        control = control,
        dots = list(...)
      )
      make_diag_fit(tww, seed = control$fit$seed,
                    engine = engine, model = model, method = method)
    },
    .package = "NLPstudio"
  )

  x <- methods::as(Matrix::Matrix(matrix(1, nrow = 4L, ncol = 2L), sparse = TRUE), "dgCMatrix")
  rownames(x) <- paste0("doc", 1:4)
  colnames(x) <- c("alpha", "beta")

  out <- assess_topic_stability(
    x,
    engine = "topicmodels",
    model = "lda",
    k = 2L,
    method = "Gibbs",
    seeds = c(5L, 6L),
    control = list(fit = list(iter = 25L)),
    custom_arg = "kept"
  )

  expect_equal(length(calls), 2L)
  expect_equal(vapply(calls, function(z) z$control$fit$seed, integer(1L)), c(5L, 6L))
  expect_equal(vapply(calls, function(z) z$control$fit$iter, integer(1L)), c(25L, 25L))
  expect_equal(vapply(calls, function(z) z$dots$custom_arg, character(1L)), c("kept", "kept"))
  expect_equal(vapply(calls, function(z) z$k, integer(1L)), c(2L, 2L))
  expect_s3_class(out, "nlp_topic_stability")
})

test_that("assess_topic_stability supports explicit resampling in automatic mode", {
  calls <- list()
  tww <- matrix(
    c(0.7, 0.3,
      0.2, 0.8),
    nrow = 2L,
    byrow = TRUE,
    dimnames = list(c("Topic001", "Topic002"), c("alpha", "beta"))
  )

  testthat::local_mocked_bindings(
    fit_topic_model = function(x, engine, model, k, method, control, ...) {
      calls[[length(calls) + 1L]] <<- list(n_docs = nrow(x), control = control)
      make_diag_fit(tww, seed = control$fit$seed,
                    engine = engine, model = model, method = method)
    },
    .package = "NLPstudio"
  )

  x <- methods::as(Matrix::Matrix(matrix(1, nrow = 6L, ncol = 2L), sparse = TRUE), "dgCMatrix")
  rownames(x) <- paste0("doc", 1:6)
  colnames(x) <- c("alpha", "beta")

  out <- assess_topic_stability(
    x,
    engine = "topicmodels",
    model = "lda",
    k = 2L,
    seeds = c(1L, 2L),
    resampling = list(fraction = 0.5),
    control = list(fit = list(iter = 10L))
  )

  expect_s3_class(out, "nlp_topic_stability")
  expect_equal(vapply(calls, function(z) z$n_docs, integer(1L)), c(3L, 3L))
  expect_equal(vapply(calls, function(z) z$control$fit$seed, integer(1L)), c(1L, 2L))
})

test_that("stability helpers reject incompatible fits and invalid options", {
  ref <- make_diag_fit(make_diag_tww(), seed = 1L)
  different_k <- make_diag_fit(make_diag_tww()[1:2, ], seed = 2L)
  different_engine <- make_diag_fit(make_diag_tww(), seed = 2L, engine = "other")

  expect_error(
    assess_topic_stability(list(ref, different_k), seeds = c(1L, 2L)),
    "same number of topics"
  )
  expect_error(
    assess_topic_stability(list(ref, different_engine), seeds = c(1L, 2L)),
    "same engine"
  )
  expect_error(
    NLPstudio:::.validate_stability_resampling(list()),
    "fraction"
  )
  expect_error(
    NLPstudio:::.validate_stability_resampling(list(fraction = 0)),
    "fraction"
  )
  expect_error(
    NLPstudio:::.validate_stability_resampling(list(fraction = 0.5, replace = TRUE)),
    "Unknown"
  )
  expect_null(NLPstudio:::.validate_stability_seeds(NULL, required = FALSE))
  expect_error(
    NLPstudio:::.hungarian_min_assignment(matrix(1, nrow = 2L, ncol = 3L)),
    "square"
  )
})

test_that("stability resampling preserves dfm inputs", {
  x <- methods::as(Matrix::Matrix(matrix(1, nrow = 6L, ncol = 2L), sparse = TRUE), "dgCMatrix")
  rownames(x) <- paste0("doc", 1:6)
  colnames(x) <- c("alpha", "beta")
  dfm <- quanteda::as.dfm(x)

  out <- NLPstudio:::.stability_resample_input(dfm, list(fraction = 0.5), seed = 1L)

  expect_true(inherits(out, "dfm"))
  expect_equal(quanteda::ndoc(out), 3L)
})

test_that("summarize_topics returns interpretation schema with representative metadata", {
  tww <- matrix(
    c(0.7, 0.2, 0.1,
      0.1, 0.2, 0.7),
    nrow = 2L,
    byrow = TRUE,
    dimnames = list(c("Topic001", "Topic002"), c("alpha", "beta", "gamma"))
  )
  dtw <- matrix(
    c(0.9, 0.1,
      0.8, 0.2,
      0.2, 0.8,
      0.1, 0.9),
    nrow = 4L,
    byrow = TRUE,
    dimnames = list(paste0("doc", 1:4), c("Topic001", "Topic002"))
  )
  doc_data <- data.table::data.table(
    doc_id = paste0("doc", 1:4),
    group = c("a", "a", "b", "b"),
    text = paste("document", 1:4)
  )
  fit <- make_diag_fit(tww, dtw, doc_data = doc_data)

  out <- summarize_topics(
    fit,
    doc_data = doc_data,
    top_n = 2L,
    representative_n = 2L,
    include_text = TRUE
  )

  expect_s3_class(out, "data.table")
  expect_equal(nrow(out), 2L)
  expect_true(all(c(
    "topic_id", "top_terms", "top_term_probabilities", "prevalence",
    "coherence_npmi", "coherence_umass", "diversity", "exclusivity",
    "representative_doc_ids", "representative_documents", "representative_text"
  ) %in% names(out)))
  expect_equal(out[topic_id == "Topic001", prevalence], 0.5, tolerance = 1e-8)
  expect_equal(out[topic_id == "Topic001", top_terms], "alpha, beta")
  expect_equal(out[topic_id == "Topic001", representative_doc_ids], "doc1, doc2")
  expect_equal(out[topic_id == "Topic002", representative_doc_ids], "doc4, doc3")
  expect_true(is.na(out[topic_id == "Topic001", coherence_npmi]))
  expect_equal(out[topic_id == "Topic001", representative_text], "document 1 || document 2")
  expect_equal(out[topic_id == "Topic001", representative_documents][[1L]]$group, c("a", "a"))
})

test_that("summarize_topics includes coherence when training is supplied", {
  tww <- matrix(
    c(0.7, 0.2, 0.1,
      0.1, 0.2, 0.7),
    nrow = 2L,
    byrow = TRUE,
    dimnames = list(c("Topic001", "Topic002"), c("alpha", "beta", "gamma"))
  )
  fit <- make_diag_fit(tww)
  training <- methods::as(
    Matrix::Matrix(
      matrix(
        c(2, 1, 0,
          1, 1, 0,
          0, 1, 2,
          0, 0, 2),
        nrow = 4L,
        byrow = TRUE
      ),
      sparse = TRUE
    ),
    "dgCMatrix"
  )
  rownames(training) <- paste0("doc", 1:4)
  colnames(training) <- c("alpha", "beta", "gamma")

  out <- summarize_topics(fit, training = training, top_n = 2L)

  expect_true(all(is.finite(out$coherence_umass)))
  expect_true(all(is.finite(out$coherence_npmi)))
})

test_that("summarize_topics validates input and supports no representative documents", {
  fit <- make_diag_fit(
    matrix(
      c(0.6, 0.4,
        0.4, 0.6),
      nrow = 2L,
      byrow = TRUE,
      dimnames = list(c("Topic001", "Topic002"), c("alpha", "beta"))
    )
  )

  expect_error(summarize_topics(list()), "nlp_topic_fit")
  expect_error(summarize_topics(fit, top_n = 0L), "top_n")
  expect_error(summarize_topics(fit, representative_n = -1L), "representative_n")
  expect_error(summarize_topics(fit, include_text = NA), "include_text")
  expect_error(summarize_topics(fit, docvars = NA), "docvars")

  doc_data <- data.table::data.table(
    doc_id = paste0("doc", 1:4),
    text = paste("document", 1:4)
  )
  out <- summarize_topics(
    fit,
    doc_data = doc_data,
    representative_n = 0L,
    include_text = TRUE
  )
  expect_true(all(is.na(out$representative_doc_ids)))
  expect_true(all(is.na(out$representative_text)))
  expect_true(all(vapply(out$representative_documents, data.table::is.data.table, logical(1L))))
  expect_true(all(vapply(out$representative_documents, nrow, integer(1L)) == 0L))
})

test_that("summarize_topics handles empty representative-candidate output", {
  tww <- matrix(
    c(0.6, 0.4,
      0.4, 0.6),
    nrow = 2L,
    byrow = TRUE,
    dimnames = list(c("Topic001", "Topic002"), c("alpha", "beta"))
  )
  fit <- make_diag_fit(tww)
  doc_data <- data.table::data.table(
    doc_id = paste0("doc", 1:4),
    text = paste("document", 1:4)
  )
  empty_reps <- data.table::data.table(
    doc_id = character(),
    Topic001 = numeric(),
    Topic002 = numeric(),
    topic_max_id = character(),
    topic_max_int = integer(),
    topic_max_value = numeric(),
    candidate_band = character(),
    topic_rank = integer(),
    text = character()
  )
  testthat::local_mocked_bindings(
    get_representative_candidates = function(...) empty_reps,
    .package = "NLPstudio"
  )

  out <- summarize_topics(
    fit,
    doc_data = doc_data,
    representative_n = 2L,
    include_text = TRUE
  )

  expect_true(all(is.na(out$representative_doc_ids)))
  expect_true(all(is.na(out$representative_text)))
  expect_true(all(vapply(out$representative_documents, nrow, integer(1L)) == 0L))
})

test_that("print.nlp_topic_stability is compact", {
  ref <- make_diag_fit(make_diag_tww(), seed = 10L)
  cand <- make_diag_fit(make_diag_tww(), seed = 11L)
  out <- assess_topic_stability(list(ref, cand), seeds = c(10L, 11L))

  printed <- utils::capture.output(ret <- print(out))
  expect_true(any(grepl("<nlp_topic_stability>", printed, fixed = TRUE)))
  expect_true(any(grepl("aggregate stability", printed, fixed = TRUE)))
  expect_identical(ret, out)

  empty <- data.table::data.table()
  data.table::setattr(empty, "class", c("nlp_topic_stability", "data.table", "data.frame"))
  empty_printed <- utils::capture.output(empty_ret <- print(empty))
  expect_true(any(grepl("No stability comparisons", empty_printed, fixed = TRUE)))
  expect_identical(empty_ret, empty)
})
