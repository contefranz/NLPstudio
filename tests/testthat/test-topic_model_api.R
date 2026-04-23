make_topic_dtm <- function() {
  x <- Matrix::Matrix(
    matrix(
      c(2, 1, 0, 0,
        1, 1, 1, 0,
        0, 1, 2, 1,
        0, 0, 1, 2,
        1, 0, 1, 1,
        1, 2, 0, 1),
      nrow = 6,
      byrow = TRUE
    ),
    sparse = TRUE
  )
  x <- methods::as(x, "dgCMatrix")
  rownames(x) <- paste0("doc", seq_len(nrow(x)))
  colnames(x) <- paste0("term", seq_len(ncol(x)))
  x
}

make_topic_dfm <- function() {
  x <- quanteda::as.dfm(make_topic_dtm())
  quanteda::docvars(x, "year") <- 2020 + seq_len(quanteda::ndoc(x)) - 1L
  quanteda::docvars(x, "group") <- c("a", "a", "b", "b", "c", "c")
  x
}

make_topic_metadata <- function() {
  data.table::data.table(
    doc_id = paste0("doc", 1:6),
    group = paste0("override_", 1:6),
    text = paste("text", 1:6)
  )
}

make_seed_dictionary <- function() {
  quanteda::dictionary(list(
    topic_a = c("term1", "term2"),
    topic_b = c("term3", "term4")
  ))
}

topic_cols <- function(x) {
  if (is.matrix(x)) {
    return(grep("^Topic\\d+$", colnames(x), value = TRUE))
  }
  grep("^Topic\\d+$", names(x), value = TRUE)
}

test_that("fit_topic_model returns lean standardized text2vec output", {
  fit <- fit_topic_model(
    make_topic_dfm(),
    engine = "text2vec",
    model = "lda",
    k = 2,
    control = list(fit = list(n_iter = 25, progressbar = FALSE))
  )

  expect_s3_class(fit, "nlp_topic_fit")
  expect_equal(class(fit), c("nlp_topic_fit", "list"))
  expect_named(
    fit,
    c("engine", "model", "method", "model_object", "dtw", "tww", "doc_ids", "docvars", "doc_data", "call")
  )
  expect_equal(fit$engine, "text2vec")
  expect_equal(fit$model, "lda")
  expect_null(fit$method)
  expect_true(inherits(fit$model_object, "WarpLDA"))
  expect_true(is.matrix(fit$dtw))
  expect_true(is.matrix(fit$tww))
  expect_equal(topic_cols(fit$dtw), c("Topic001", "Topic002"))
  expect_equal(rownames(fit$tww), c("Topic001", "Topic002"))
  expect_equal(fit$doc_ids, paste0("doc", 1:6))
  expect_true(data.table::is.data.table(fit$docvars))
  expect_equal(fit$docvars$year, 2020:2025)
  expect_equal(fit$docvars$group, c("a", "a", "b", "b", "c", "c"))
  expect_null(fit$doc_data)

  expect_equal(
    unname(rowSums(fit$dtw)),
    rep(1, nrow(fit$dtw)),
    tolerance = 1e-8
  )
  expect_equal(
    unname(rowSums(fit$tww)),
    rep(1, nrow(fit$tww)),
    tolerance = 1e-8
  )
})

test_that("fit_topic_model validates unsupported combinations and control structure", {
  expect_error(
    fit_topic_model(make_topic_dtm(), engine = "topicmodels", model = "ctm", k = 2, method = "Gibbs"),
    "CTM only supports"
  )
  expect_error(
    fit_topic_model(make_topic_dtm(), engine = "text2vec", model = "lda", k = 2, method = "VEM"),
    "method must be NULL"
  )
  expect_error(
    fit_topic_model(make_topic_dtm(), engine = "seededlda", model = "lda", k = 2, method = "VEM"),
    "method must be NULL"
  )
  expect_error(
    fit_topic_model(make_topic_dtm(), engine = "seededlda", model = "seededlda"),
    "dictionary must be supplied"
  )
  expect_error(
    fit_topic_model(make_topic_dtm(), engine = "text2vec", model = "lda", k = 2, seedwords = matrix(1, 1, 1)),
    "seedwords is only valid"
  )
  expect_error(
    fit_topic_model(
      make_topic_dtm(),
      engine = "topicmodels",
      model = "lda",
      k = 2,
      control = list(model = list(alpha = 0.1))
    ),
    "control\\$model must be empty"
  )
  expect_error(
    fit_topic_model(
      make_topic_dtm(),
      engine = "text2vec",
      model = "lda",
      k = 2,
      control = list(extra = list())
    ),
    "Unknown top-level control entries"
  )
})

test_that("topicmodels LDA and CTM fits are supported", {
  skip_if_not_installed("topicmodels")

  vem_fit <- fit_topic_model(
    make_topic_dtm(),
    engine = "topicmodels",
    model = "lda",
    k = 2,
    control = list(fit = list(seed = 1, em = list(iter.max = 5), var = list(iter.max = 5)))
  )
  ctm_fit <- fit_topic_model(
    make_topic_dtm(),
    engine = "topicmodels",
    model = "ctm",
    k = 2,
    control = list(fit = list(seed = 1, em = list(iter.max = 5), var = list(iter.max = 5)))
  )
  gibbs_fit <- fit_topic_model(
    make_topic_dtm(),
    engine = "topicmodels",
    model = "lda",
    k = 2,
    method = "Gibbs",
    control = list(fit = list(seed = 1, iter = 50, burnin = 0, thin = 1))
  )

  expect_equal(vem_fit$method, "VEM")
  expect_equal(ctm_fit$method, "VEM")
  expect_equal(gibbs_fit$method, "Gibbs")
  expect_equal(topic_cols(get_dtw(vem_fit)), c("Topic001", "Topic002"))
  expect_equal(get_tww(ctm_fit)$topic_id, c("Topic001", "Topic002"))

  expect_equal(
    rowSums(as.matrix(get_dtw(gibbs_fit)[, topic_cols(get_dtw(gibbs_fit)), with = FALSE])),
    rep(1, nrow(get_dtw(gibbs_fit))),
    tolerance = 1e-8
  )
})

test_that("seededlda engines are supported", {
  skip_if_not_installed("seededlda")

  lda_fit <- fit_topic_model(
    make_topic_dtm(),
    engine = "seededlda",
    model = "lda",
    k = 2,
    control = list(fit = list(max_iter = 100, verbose = FALSE))
  )
  seq_fit <- suppressWarnings(
    fit_topic_model(
      make_topic_dtm(),
      engine = "seededlda",
      model = "seqlda",
      k = 2,
      control = list(fit = list(max_iter = 100, verbose = FALSE))
    )
  )
  seeded_fit <- fit_topic_model(
    make_topic_dtm(),
    engine = "seededlda",
    model = "seededlda",
    dictionary = make_seed_dictionary(),
    control = list(fit = list(max_iter = 100, verbose = FALSE))
  )

  expect_null(lda_fit$method)
  expect_null(seq_fit$method)
  expect_null(seeded_fit$method)
  expect_equal(topic_cols(get_dtw(lda_fit)), c("Topic001", "Topic002"))
  expect_equal(get_tww(seq_fit)$topic_id, c("Topic001", "Topic002"))
  expect_equal(
    rowSums(as.matrix(get_tww(seeded_fit)[, setdiff(names(get_tww(seeded_fit)), "topic_id"), with = FALSE])),
    rep(1, nrow(get_tww(seeded_fit))),
    tolerance = 1e-8
  )
})

test_that("get_dtw stores docvars, supports doc_data, and warns when text is unavailable", {
  fit <- fit_topic_model(
    make_topic_dfm(),
    engine = "text2vec",
    model = "lda",
    k = 2,
    control = list(fit = list(n_iter = 25, progressbar = FALSE))
  )

  embedded <- get_dtw(fit)
  expect_equal(embedded$year, 2020:2025)
  expect_equal(embedded$group, c("a", "a", "b", "b", "c", "c"))

  expect_warning(
    no_text <- get_dtw(fit, include_text = TRUE),
    "text-bearing doc_data"
  )
  expect_false("text" %in% names(no_text))

  fit_with_doc_data <- fit_topic_model(
    make_topic_dfm(),
    engine = "text2vec",
    model = "lda",
    k = 2,
    doc_data = make_topic_metadata(),
    control = list(fit = list(n_iter = 25, progressbar = FALSE))
  )

  stored <- get_dtw(fit_with_doc_data, include_text = TRUE)
  expect_equal(stored$group, paste0("override_", 1:6))
  expect_equal(stored$text, paste("text", 1:6))

  override_meta <- make_topic_metadata()
  override_meta[, group := paste0("manual_", 1:6)]

  override <- get_dtw(fit_with_doc_data, doc_data = override_meta, include_text = TRUE)
  expect_equal(override$group, paste0("manual_", 1:6))
  expect_equal(override$text, paste("text", 1:6))
})

test_that("get_top_terms and plot_dtw use standardized extractors", {
  fit <- fit_topic_model(
    make_topic_dtm(),
    engine = "text2vec",
    model = "lda",
    k = 2,
    control = list(fit = list(n_iter = 25, progressbar = FALSE))
  )

  top_terms <- get_top_terms(fit, n = 2, format = "long")
  expect_true(all(c("rank", "topic", "term", "probability") %in% names(top_terms)))
  expect_true(all(unique(top_terms$topic) %in% c("Topic001", "Topic002")))

  plot_obj <- plot_dtw(fit, topics = 1:2, bins = 5)
  expect_s3_class(plot_obj, "ggplot")
})

test_that("representative candidates band within topic and fall back on ties", {
  dtw <- data.table::data.table(
    doc_id = paste0("doc", 1:8),
    Topic001 = c(0.95, 0.85, 0.75, 0.65, 0.20, 0.15, 0.10, 0.05),
    Topic002 = c(0.05, 0.15, 0.25, 0.35, 0.80, 0.85, 0.90, 0.95),
    text = paste("candidate", 1:8)
  )

  bands <- get_representative_candidates(
    dtw,
    include_text = TRUE,
    quantile_probs = 0.5,
    labels = c("LOW", "HIGH")
  )

  expect_true(all(c("candidate_band", "topic_rank", "text") %in% names(bands)))
  expect_equal(sort(unique(bands$candidate_band)), c("HIGH", "LOW"))

  tied <- data.table::data.table(
    doc_id = paste0("doc", 1:4),
    Topic001 = c(0.9, 0.9, 0.1, 0.1),
    Topic002 = c(0.1, 0.1, 0.9, 0.9)
  )

  tied_out <- get_representative_candidates(
    tied,
    quantile_probs = 0.5,
    labels = c("LOW", "HIGH")
  )

  expect_false(anyNA(tied_out$candidate_band))
  expect_equal(sort(unique(tied_out$candidate_band)), c("HIGH", "LOW"))
})

test_that("warp_lda remains available as a deprecated compatibility wrapper", {
  expect_warning(
    fit <- warp_lda(
      make_topic_dtm(),
      k = 2,
      fit_control = list(n_iter = 25, progressbar = FALSE)
    ),
    "deprecated|fit_topic_model"
  )

  expect_type(fit, "list")
  expect_named(fit, c("lda_object", "theta", "phi"), ignore.order = TRUE)
  expect_true(inherits(fit$lda_object, "WarpLDA"))
  expect_equal(topic_cols(fit$theta), c("Topic001", "Topic002"))
  expect_false(exists("warpLDA", envir = asNamespace("NLPstudio"), inherits = FALSE))
})

test_that("print.nlp_topic_fit stays compact", {
  fit <- fit_topic_model(
    make_topic_dtm(),
    engine = "text2vec",
    model = "lda",
    k = 2,
    control = list(fit = list(n_iter = 25, progressbar = FALSE))
  )

  out <- capture.output(print(fit))
  expect_true(any(grepl("<nlp_topic_fit>", out, fixed = TRUE)))
  expect_true(any(grepl("engine: text2vec", out, fixed = TRUE)))
  expect_true(any(grepl("cached DTW: TRUE", out, fixed = TRUE)))
  expect_false(any(grepl("term1", out, fixed = TRUE)))
})
