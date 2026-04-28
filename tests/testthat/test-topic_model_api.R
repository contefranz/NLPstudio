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

make_prediction_dfm <- function() {
  x <- Matrix::Matrix(
    matrix(
      c(1, 1, 0, 0, 0,
        0, 0, 0, 1, 0,
        0, 0, 0, 0, 2,
        0, 1, 0, 0, 1),
      nrow = 4,
      byrow = TRUE
    ),
    sparse = TRUE
  )
  x <- methods::as(x, "dgCMatrix")
  rownames(x) <- paste0("pred", seq_len(nrow(x)))
  colnames(x) <- paste0("term", seq_len(ncol(x)))
  x <- quanteda::as.dfm(x)
  quanteda::docvars(x, "year") <- 2030:2033
  quanteda::docvars(x, "group") <- c("p", "p", "q", "q")
  x
}

make_prediction_metadata <- function() {
  data.table::data.table(
    doc_id = paste0("pred", 1:4),
    source = paste0("manual_", 1:4),
    text = paste("prediction", 1:4)
  )
}

make_prediction_clean_dfm <- function() {
  x <- Matrix::Matrix(
    matrix(
      c(1, 1, 0, 0,
        0, 0, 1, 1,
        1, 0, 1, 0),
      nrow = 3,
      byrow = TRUE
    ),
    sparse = TRUE
  )
  x <- methods::as(x, "dgCMatrix")
  rownames(x) <- paste0("pred_clean", seq_len(nrow(x)))
  colnames(x) <- paste0("term", seq_len(ncol(x)))
  x <- quanteda::as.dfm(x)
  quanteda::docvars(x, "year") <- 2040:2042
  quanteda::docvars(x, "group") <- c("x", "y", "z")
  x
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
  skip_if_not_installed("text2vec")
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
    c("engine", "model", "method", "model_object", "dtw", "tww",
      "doc_ids", "vocab", "docvars", "doc_data", "hyperparameters",
      "backend_control", "call")
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
  expect_equal(fit$vocab, paste0("term", 1:4))
  expect_true(data.table::is.data.table(fit$docvars))
  expect_equal(fit$docvars$year, 2020:2025)
  expect_equal(fit$docvars$group, c("a", "a", "b", "b", "c", "c"))
  expect_null(fit$doc_data)
  expect_true(data.table::is.data.table(fit$hyperparameters))
  expect_named(fit$backend_control, c("model", "fit", "optimizer"))

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

test_that("get_topic_hyperparameters returns standardized text2vec rows", {
  skip_if_not_installed("text2vec")
  fit <- fit_topic_model(
    make_topic_dtm(),
    engine = "text2vec",
    model = "lda",
    k = 2,
    control = list(
      model = list(doc_topic_prior = 0.2, topic_word_prior = 0.03),
      fit = list(n_iter = 25, progressbar = FALSE)
    )
  )

  hp <- get_topic_hyperparameters(fit)
  expect_true(data.table::is.data.table(hp))
  expect_named(hp, c("parameter", "value", "source_section", "source_name"))
  expect_equal(hp$parameter, c("k", "alpha", "beta"))
  expect_equal(hp[parameter == "k", value][[1L]], 2)
  expect_equal(hp[parameter == "alpha", value][[1L]], 0.2)
  expect_equal(hp[parameter == "beta", value][[1L]], 0.03)
  expect_equal(hp[parameter == "alpha", source_section], "model")
  expect_equal(hp[parameter == "alpha", source_name], "doc_topic_prior")
  expect_equal(hp[parameter == "beta", source_section], "model")
  expect_equal(hp[parameter == "beta", source_name], "topic_word_prior")

  expect_equal(fit$backend_control$model$doc_topic_prior, 0.2)
  expect_equal(fit$backend_control$model$topic_word_prior, 0.03)
  expect_false("x" %in% names(fit$backend_control$fit))
})

test_that("get_topic_hyperparameters rejects non-topic fits", {
  expect_error(get_topic_hyperparameters(list()), "fit_topic_model")
})

test_that("get_topic_hyperparameters warns for legacy topic fits", {
  skip_if_not_installed("text2vec")
  fit <- fit_topic_model(
    make_topic_dtm(),
    engine = "text2vec",
    model = "lda",
    k = 2,
    control = list(fit = list(n_iter = 25, progressbar = FALSE))
  )
  fit$hyperparameters <- NULL

  expect_warning(
    hp <- get_topic_hyperparameters(fit),
    "does not contain stored hyperparameters"
  )
  expect_equal(hp[parameter == "k", value][[1L]], 2)
  expect_equal(hp[parameter == "k", source_section], "fit_object")
  expect_equal(hp[parameter == "k", source_name], "dtw")
  expect_true(is.na(hp[parameter == "alpha", value][[1L]]))
  expect_true(is.na(hp[parameter == "beta", value][[1L]]))
})

test_that("predict_topic_model aligns new vocabulary and optionally joins docvars/doc_data", {
  skip_if_not_installed("text2vec")
  fit <- fit_topic_model(
    make_topic_dfm(),
    engine = "text2vec",
    model = "lda",
    k = 2,
    control = list(fit = list(n_iter = 25, progressbar = FALSE))
  )

  pred <- NULL
  expect_warning(
    expect_warning(
      pred <- predict_topic_model(
        fit,
        make_prediction_dfm(),
        doc_data = make_prediction_metadata(),
        include_text = TRUE
      ),
      "Dropping 1 terms"
    ),
    "Dropping 1 documents"
  )

  expect_equal(pred$doc_id, c("pred1", "pred2", "pred4"))
  expect_equal(topic_cols(pred), c("Topic001", "Topic002"))
  expect_false("year" %in% names(pred))
  expect_false("group" %in% names(pred))
  expect_equal(pred$source, c("manual_1", "manual_2", "manual_4"))
  expect_equal(pred$text, c("prediction 1", "prediction 2", "prediction 4"))
  expect_equal(
    names(pred),
    c("doc_id", "source", topic_cols(pred), "topic_max_id", "topic_max_int", "topic_max_value", "text")
  )
  expect_equal(
    rowSums(as.matrix(pred[, topic_cols(pred), with = FALSE])),
    rep(1, nrow(pred)),
    tolerance = 1e-8
  )

  pred_docvars <- predict_topic_model(fit, make_prediction_clean_dfm(), docvars = TRUE)
  expect_equal(pred_docvars$year, 2040:2042)
  expect_equal(pred_docvars$group, c("x", "y", "z"))
  expect_equal(
    names(pred_docvars),
    c("doc_id", "year", "group", topic_cols(pred_docvars), "topic_max_id", "topic_max_int", "topic_max_value")
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
  expect_error(
    fit_topic_model(
      make_topic_dtm(),
      engine = "text2vec",
      model = "lda",
      k = 2,
      control = list(optimizer = list(lr = 0.01))
    ),
    "control\\$optimizer must be empty"
  )
  expect_error(
    fit_topic_model(
      make_topic_dtm(),
      engine = "topicmodels.etm",
      model = "etm",
      k = 2,
      method = "VEM"
    ),
    "method must be NULL"
  )
})

test_that("predict_topic_model works for topicmodels LDA and CTM", {
  skip_if_not_installed("topicmodels")

  lda_fit <- fit_topic_model(
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

  lda_pred <- predict_topic_model(lda_fit, make_prediction_clean_dfm())
  ctm_pred <- predict_topic_model(ctm_fit, make_prediction_clean_dfm(), docvars = TRUE)

  expect_equal(topic_cols(lda_pred), c("Topic001", "Topic002"))
  expect_equal(topic_cols(ctm_pred), c("Topic001", "Topic002"))
  expect_equal(lda_pred$doc_id, paste0("pred_clean", 1:3))
  expect_false("year" %in% names(lda_pred))
  expect_equal(ctm_pred$year, 2040:2042)
  expect_equal(
    rowSums(as.matrix(lda_pred[, topic_cols(lda_pred), with = FALSE])),
    rep(1, nrow(lda_pred)),
    tolerance = 1e-8
  )
  expect_equal(
    rowSums(as.matrix(ctm_pred[, topic_cols(ctm_pred), with = FALSE])),
    rep(1, nrow(ctm_pred)),
    tolerance = 1e-8
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

test_that("predict_topic_model works for seededlda fits", {
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

  lda_pred <- predict_topic_model(lda_fit, make_prediction_clean_dfm())
  seq_pred <- predict_topic_model(seq_fit, make_prediction_clean_dfm(), docvars = TRUE)
  seeded_pred <- predict_topic_model(seeded_fit, make_prediction_clean_dfm(), docvars = TRUE)

  expect_equal(lda_pred$doc_id, paste0("pred_clean", 1:3))
  expect_false("group" %in% names(lda_pred))
  expect_equal(seq_pred$group, c("x", "y", "z"))
  expect_equal(seeded_pred$year, 2040:2042)
  expect_equal(
    rowSums(as.matrix(lda_pred[, topic_cols(lda_pred), with = FALSE])),
    rep(1, nrow(lda_pred)),
    tolerance = 1e-8
  )
  expect_equal(
    rowSums(as.matrix(seq_pred[, topic_cols(seq_pred), with = FALSE])),
    rep(1, nrow(seq_pred)),
    tolerance = 1e-8
  )
  expect_equal(
    rowSums(as.matrix(seeded_pred[, topic_cols(seeded_pred), with = FALSE])),
    rep(1, nrow(seeded_pred)),
    tolerance = 1e-8
  )
})

test_that("ETM fits are supported with learned and pretrained embeddings", {
  skip_if_not_installed("topicmodels.etm")
  skip_if_not_installed("torch")
  if (!torch::torch_is_installed()) {
    skip("torch backend is not installed")
  }

  torch::torch_manual_seed(1)
  learned_fit <- fit_topic_model(
    make_topic_dtm(),
    engine = "topicmodels.etm",
    model = "etm",
    k = 2,
    control = list(
      model = list(embeddings = 5),
      fit = list(epoch = 2, batch_size = 2, normalize = TRUE),
      optimizer = list(lr = 0.005, weight_decay = 1.2e-06)
    )
  )

  embeddings <- matrix(
    seq_len(ncol(make_topic_dtm()) * 4),
    nrow = ncol(make_topic_dtm()),
    ncol = 4,
    dimnames = list(colnames(make_topic_dtm()), NULL)
  )
  torch::torch_manual_seed(1)
  pretrained_fit <- fit_topic_model(
    make_topic_dtm(),
    engine = "topicmodels.etm",
    model = "etm",
    k = 2,
    control = list(
      model = list(embeddings = embeddings),
      fit = list(epoch = 2, batch_size = 2)
    )
  )

  expect_equal(learned_fit$engine, "topicmodels.etm")
  expect_equal(pretrained_fit$engine, "topicmodels.etm")
  expect_equal(topic_cols(learned_fit$dtw), c("Topic001", "Topic002"))
  expect_equal(rownames(pretrained_fit$tww), c("Topic001", "Topic002"))
  expect_equal(unname(rowSums(learned_fit$dtw)), rep(1, nrow(learned_fit$dtw)), tolerance = 1e-6)
  expect_equal(unname(rowSums(pretrained_fit$tww)), rep(1, nrow(pretrained_fit$tww)), tolerance = 1e-6)
  expect_equal(get_tww(learned_fit$model_object)$topic_id, c("Topic001", "Topic002"))
  expect_true(all(c("rank", "topic", "term", "probability") %in% names(get_top_terms(pretrained_fit, n = 2))))
  expect_error(get_dtw(learned_fit$model_object), "Raw ETM objects do not retain fitted DTW")
})

test_that("ETM prediction and embedding helpers are supported", {
  skip_if_not_installed("topicmodels.etm")
  skip_if_not_installed("torch")
  skip_if_not_installed("uwot")
  if (!torch::torch_is_installed()) {
    skip("torch backend is not installed")
  }

  torch::torch_manual_seed(1)
  fit <- fit_topic_model(
    make_topic_dtm(),
    engine = "topicmodels.etm",
    model = "etm",
    k = 2,
    control = list(
      model = list(embeddings = 5),
      fit = list(epoch = 2, batch_size = 2, normalize = TRUE),
      optimizer = list(lr = 0.005, weight_decay = 1.2e-06)
    )
  )

  pred <- predict_topic_model(
    fit,
    make_prediction_clean_dfm(),
    doc_data = data.table::data.table(
      doc_id = paste0("pred_clean", 1:3),
      text = paste("clean", 1:3)
    ),
    include_text = TRUE
  )
  topic_emb <- get_topic_embeddings(fit)
  raw_topic_emb <- get_topic_embeddings(fit$model_object)
  term_emb <- get_term_embeddings(fit)
  plot_obj <- plot_topic_embeddings(
    fit,
    top_n = 3,
    metric = "cosine",
    n_neighbors = 2,
    fast_sgd = FALSE,
    verbose = FALSE
  )

  expect_equal(topic_cols(pred), c("Topic001", "Topic002"))
  expect_equal(pred$text, c("clean 1", "clean 2", "clean 3"))
  expect_equal(
    rowSums(as.matrix(pred[, topic_cols(pred), with = FALSE])),
    rep(1, nrow(pred)),
    tolerance = 1e-6
  )
  expect_equal(names(topic_emb), c("topic_id", "dim_001", "dim_002", "dim_003", "dim_004", "dim_005"))
  expect_equal(topic_emb$topic_id, c("Topic001", "Topic002"))
  expect_equal(raw_topic_emb$topic_id, c("Topic001", "Topic002"))
  expect_equal(term_emb$term, fit$vocab)
  expect_s3_class(plot_obj, "ggplot")
})

test_that("ETM validates pretrained embeddings and keeps alignment after pruning", {
  skip_if_not_installed("topicmodels.etm")
  skip_if_not_installed("torch")
  if (!torch::torch_is_installed()) {
    skip("torch backend is not installed")
  }

  expect_error(
    fit_topic_model(
      make_topic_dtm(),
      engine = "topicmodels.etm",
      model = "etm",
      k = 2,
      control = list(
        model = list(embeddings = matrix(1:8, nrow = 4)),
        fit = list(epoch = 2, batch_size = 2)
      )
    ),
    "rownames"
  )

  x <- Matrix::Matrix(
    matrix(
      c(2, 0, 0, 0,
        1, 1, 1, 0,
        0, 1, 2, 1,
        0, 0, 1, 2),
      nrow = 4,
      byrow = TRUE
    ),
    sparse = TRUE
  )
  x <- methods::as(x, "dgCMatrix")
  rownames(x) <- paste0("doc", 1:4)
  colnames(x) <- paste0("term", 1:4)
  x <- quanteda::as.dfm(x)
  quanteda::docvars(x, "year") <- 2020:2023

  embeddings <- matrix(
    seq_len(3 * 4),
    nrow = 3,
    ncol = 4,
    dimnames = list(c("term2", "term3", "term4"), NULL)
  )

  expect_warning(
    fit <- fit_topic_model(
      x,
      engine = "topicmodels.etm",
      model = "etm",
      k = 2,
      control = list(
        model = list(embeddings = embeddings),
        fit = list(epoch = 2, batch_size = 2)
      )
    ),
    "Dropping"
  )

  expect_equal(fit$doc_ids, c("doc2", "doc3", "doc4"))
  expect_equal(fit$docvars$doc_id, c("doc2", "doc3", "doc4"))
  expect_equal(get_dtw(fit)$doc_id, c("doc2", "doc3", "doc4"))
})

test_that("get_dtw stores docvars but omits them by default", {
  skip_if_not_installed("text2vec")
  fit <- fit_topic_model(
    make_topic_dfm(),
    engine = "text2vec",
    model = "lda",
    k = 2,
    control = list(fit = list(n_iter = 25, progressbar = FALSE))
  )

  lean <- get_dtw(fit)
  expect_false("year" %in% names(lean))
  expect_false("group" %in% names(lean))

  embedded <- get_dtw(fit, docvars = TRUE)
  expect_equal(embedded$year, 2020:2025)
  expect_equal(embedded$group, c("a", "a", "b", "b", "c", "c"))
  expect_equal(
    names(embedded),
    c("doc_id", "year", "group", topic_cols(embedded), "topic_max_id", "topic_max_int", "topic_max_value")
  )
  expect_type(embedded$topic_max_int, "integer")
  expect_equal(embedded$topic_max_int, as.integer(sub("^Topic", "", embedded$topic_max_id)))

  lean_from_enriched <- get_dtw(embedded)
  expect_false("year" %in% names(lean_from_enriched))
  expect_false("group" %in% names(lean_from_enriched))

  enriched_from_enriched <- get_dtw(embedded, docvars = TRUE)
  expect_equal(enriched_from_enriched$year, 2020:2025)
  expect_equal(enriched_from_enriched$group, c("a", "a", "b", "b", "c", "c"))
  expect_equal(names(embedded), c("doc_id", "year", "group", topic_cols(embedded), "topic_max_id", "topic_max_int", "topic_max_value"))

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
  expect_false("year" %in% names(stored))
  expect_equal(stored$group, paste0("override_", 1:6))
  expect_equal(stored$text, paste("text", 1:6))
  expect_equal(
    names(stored),
    c("doc_id", "group", topic_cols(stored), "topic_max_id", "topic_max_int", "topic_max_value", "text")
  )

  override_meta <- make_topic_metadata()
  override_meta[, group := paste0("manual_", 1:6)]

  override <- get_dtw(fit_with_doc_data, doc_data = override_meta, include_text = TRUE)
  expect_equal(override$group, paste0("manual_", 1:6))
  expect_equal(override$text, paste("text", 1:6))

  enriched <- get_dtw(fit_with_doc_data, docvars = TRUE, include_text = TRUE)
  expect_equal(enriched$year, 2020:2025)
  expect_equal(enriched$group, paste0("override_", 1:6))
  expect_equal(
    names(enriched),
    c("doc_id", "year", "group", topic_cols(enriched), "topic_max_id", "topic_max_int", "topic_max_value", "text")
  )
})

test_that("get_top_terms and plot_dtw use standardized extractors", {
  skip_if_not_installed("text2vec")
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
  expect_equal(tail(names(bands), 1), "text")
  expect_equal(sort(unique(bands$candidate_band)), c("HIGH", "LOW"))

  lean_bands <- get_representative_candidates(
    dtw,
    quantile_probs = 0.5,
    labels = c("LOW", "HIGH")
  )
  expect_false("text" %in% names(lean_bands))

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

test_that("representative candidates optionally include stored docvars", {
  skip_if_not_installed("text2vec")
  fit <- fit_topic_model(
    make_topic_dfm(),
    engine = "text2vec",
    model = "lda",
    k = 2,
    control = list(fit = list(n_iter = 25, progressbar = FALSE))
  )

  candidates <- get_representative_candidates(fit)
  expect_false("year" %in% names(candidates))
  expect_false("group" %in% names(candidates))

  enriched <- get_representative_candidates(fit, docvars = TRUE)
  expect_equal(enriched$year[order(enriched$doc_id)], 2020:2025)
  expect_equal(enriched$group[order(enriched$doc_id)], c("a", "a", "b", "b", "c", "c"))
  expect_equal(
    names(enriched),
    c(
      "doc_id", "year", "group", topic_cols(enriched), "topic_max_id",
      "topic_max_int", "topic_max_value", "candidate_band", "topic_rank"
    )
  )
  expect_type(enriched$topic_max_int, "integer")
  expect_equal(enriched$topic_max_int, as.integer(sub("^Topic", "", enriched$topic_max_id)))

  enriched_dtw <- get_dtw(fit, docvars = TRUE)
  enriched_dtw_names <- names(enriched_dtw)
  candidates_from_enriched_dtw <- get_representative_candidates(enriched_dtw)
  expect_false("year" %in% names(candidates_from_enriched_dtw))
  expect_false("group" %in% names(candidates_from_enriched_dtw))
  expect_equal(names(enriched_dtw), enriched_dtw_names)
  expect_false("candidate_band" %in% names(enriched_dtw))
  expect_false("topic_rank" %in% names(enriched_dtw))

  enriched_from_enriched_dtw <- get_representative_candidates(enriched_dtw, docvars = TRUE)
  expect_equal(enriched_from_enriched_dtw$year[order(enriched_from_enriched_dtw$doc_id)], 2020:2025)
  expect_equal(enriched_from_enriched_dtw$group[order(enriched_from_enriched_dtw$doc_id)], c("a", "a", "b", "b", "c", "c"))

  doc_data <- data.table::data.table(
    doc_id = paste0("doc", 1:6),
    year = 2030:2035,
    group = paste0("metadata_group_", 1:6),
    source = paste0("source_", 1:6),
    text = paste("metadata text", 1:6)
  )
  fit_with_doc_data <- fit_topic_model(
    make_topic_dfm(),
    engine = "text2vec",
    model = "lda",
    k = 2,
    doc_data = doc_data,
    control = list(fit = list(n_iter = 25, progressbar = FALSE))
  )

  candidates_with_doc_data <- get_representative_candidates(
    fit_with_doc_data,
    include_text = TRUE
  )
  expect_false("year" %in% names(candidates_with_doc_data))
  expect_false("group" %in% names(candidates_with_doc_data))
  expect_equal(
    candidates_with_doc_data$source[order(candidates_with_doc_data$doc_id)],
    paste0("source_", 1:6)
  )
  expect_equal(tail(names(candidates_with_doc_data), 1), "text")
})

test_that("warp_lda and warpLDA are no longer exported", {
  expect_false(exists("warp_lda", envir = asNamespace("NLPstudio"), inherits = FALSE))
  expect_false(exists("warpLDA", envir = asNamespace("NLPstudio"), inherits = FALSE))
})

test_that("ETM-specific helpers reject non-ETM inputs", {
  skip_if_not_installed("text2vec")
  fit <- fit_topic_model(
    make_topic_dtm(),
    engine = "text2vec",
    model = "lda",
    k = 2,
    control = list(fit = list(n_iter = 25, progressbar = FALSE))
  )

  expect_error(get_topic_embeddings(fit), "ETM fit or a raw ETM object")
  expect_error(get_term_embeddings(fit), "ETM fit or a raw ETM object")
  expect_error(plot_topic_embeddings(fit), "ETM fit or a raw ETM object")
})

test_that("print.nlp_topic_fit stays compact", {
  skip_if_not_installed("text2vec")
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
