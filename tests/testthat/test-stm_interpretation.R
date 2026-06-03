make_stm_interpretation_dfm <- function() {
  txt <- c(
    doc1 = "profit revenue growth",
    doc2 = "profit margin growth",
    doc3 = "risk litigation loss",
    doc4 = "debt risk loss",
    doc5 = "revenue market profit",
    doc6 = "litigation cost risk",
    doc7 = "growth profit market",
    doc8 = "risk debt loss"
  )
  corp <- quanteda::corpus(txt)
  quanteda::docvars(corp, "group") <- rep(c("a", "b"), 4)
  quanteda::dfm(quanteda::tokens(corp))
}

fit_stm_interpretation_fixture <- function(prevalence = FALSE,
                                           docvars = FALSE) {
  skip_if_not_installed("stm")
  dfm <- make_stm_interpretation_dfm()
  fit_control <- list(
    seed = 1L,
    max.em.its = 5L,
    init.type = "Spectral",
    verbose = FALSE
  )
  if (prevalence) {
    fit_control$prevalence <- ~group
  }
  suppressWarnings(fit_topic_model(
    dfm,
    engine = "stm",
    model = "stm",
    k = 2,
    docvars = docvars,
    control = list(fit = fit_control)
  ))
}

test_that("get_stm_topic_labels returns STM-native labels", {
  fit <- fit_stm_interpretation_fixture()

  labels <- get_stm_topic_labels(fit, n = 3, topics = "Topic002")
  expect_s3_class(labels, "data.table")
  expect_equal(unique(labels$topic_id), "Topic002")
  expect_equal(sort(unique(labels$label_type)), c("frex", "lift", "prob", "score"))
  expect_equal(unique(labels$source), "labelTopics")
  expect_equal(max(labels$rank), 3L)

  sage <- get_stm_topic_labels(fit, n = 2, label_types = c("prob", "frex"),
                               include_sage = TRUE)
  expect_true(all(c("labelTopics", "sageLabels") %in% unique(sage$source)))
  expect_equal(sort(unique(sage$label_type)), c("frex", "prob"))
})

test_that("get_stm_topic_labels validates inputs and STM shape", {
  fit <- fit_stm_interpretation_fixture()

  expect_error(get_stm_topic_labels(list()), "STM nlp_topic_fit")
  expect_error(get_stm_topic_labels(fit, n = 0), "'n'")
  expect_error(get_stm_topic_labels(fit, label_types = "bad"), "label_types")
  expect_error(get_stm_topic_labels(fit, frexweight = 2), "frexweight")
  expect_error(get_stm_topic_labels(fit, include_sage = NA), "include_sage")

  raw <- fit$model_object
  raw$beta$logbeta <- list(raw$beta$logbeta[[1L]], raw$beta$logbeta[[1L]])
  expect_error(
    get_stm_topic_labels(raw),
    "STM content covariates are not supported"
  )
})

test_that("summarize_stm_topics adds collapsed STM label columns", {
  fit <- fit_stm_interpretation_fixture()
  dfm <- make_stm_interpretation_dfm()

  out <- summarize_stm_topics(
    fit,
    training = dfm,
    top_n = 3,
    representative_n = 1,
    label_n = 3,
    include_sage = TRUE
  )

  expect_s3_class(out, "data.table")
  expect_equal(nrow(out), 2L)
  expect_true(all(c(
    "topic_id",
    "topic_int",
    "top_terms",
    "top_term_probabilities",
    "prevalence",
    "coherence_npmi",
    "coherence_umass",
    "diversity",
    "exclusivity",
    "representative_doc_ids",
    "representative_documents",
    "stm_prob_terms",
    "stm_frex_terms",
    "stm_lift_terms",
    "stm_score_terms",
    "stm_sage_prob_terms"
  ) %in% names(out)))
  expect_false(anyNA(out$stm_prob_terms))

  raw_out <- summarize_stm_topics(fit$model_object, top_n = 2,
                                  representative_n = 0)
  expect_equal(nrow(raw_out), 2L)
  expect_true("stm_prob_terms" %in% names(raw_out))
})

test_that("estimate_stm_topic_effects uses stored docvars and returns tidy rows", {
  fit <- fit_stm_interpretation_fixture(prevalence = TRUE, docvars = TRUE)

  out <- estimate_stm_topic_effects(fit, topics = "Topic001", nsims = 5)
  expect_s3_class(out, "nlp_stm_topic_effects")
  expect_s3_class(attr(out, "estimate_effect"), "estimateEffect")
  expect_named(
    out,
    c(
      "topic_id", "topic_int", "term", "estimate", "std_error",
      "statistic", "p_value", "conf_low", "conf_high",
      "uncertainty", "nsims"
    ),
    ignore.order = FALSE
  )
  expect_equal(unique(out$topic_id), "Topic001")
  expect_true(all(c(
    "term", "estimate", "std_error", "statistic", "p_value",
    "conf_low", "conf_high", "uncertainty", "nsims"
  ) %in% names(out)))
  expect_equal(unique(out$nsims), 5L)
  expect_true(all(out$conf_low <= out$estimate))
  expect_true(all(out$conf_high >= out$estimate))
})

test_that("estimate_stm_topic_effects accepts explicit metadata and formula", {
  fit <- fit_stm_interpretation_fixture(prevalence = TRUE)
  meta <- data.frame(
    doc_id = rev(fit$doc_ids),
    group = rev(quanteda::docvars(make_stm_interpretation_dfm(), "group"))
  )

  out <- estimate_stm_topic_effects(
    fit,
    formula = ~group,
    metadata = meta,
    topics = c(1, 2),
    uncertainty = "None",
    nsims = 5
  )

  expect_equal(sort(unique(out$topic_id)), c("Topic001", "Topic002"))
  expect_equal(unique(out$uncertainty), "None")

  raw_out <- estimate_stm_topic_effects(
    fit$model_object,
    formula = ~group,
    metadata = data.frame(group = quanteda::docvars(make_stm_interpretation_dfm(), "group")),
    topics = 1,
    nsims = 5
  )
  expect_equal(unique(raw_out$topic_id), "Topic001")

  expect_warning(
    full_formula <- estimate_stm_topic_effects(
      fit,
      formula = 1 ~ group,
      metadata = meta,
      topics = 2,
      nsims = 5
    ),
    "topics.*ignored"
  )
  expect_equal(unique(full_formula$topic_id), "Topic001")
})

test_that("estimate_stm_topic_effects reports clear metadata and formula errors", {
  fit <- fit_stm_interpretation_fixture(prevalence = TRUE)
  no_prevalence <- fit_stm_interpretation_fixture()

  expect_error(
    estimate_stm_topic_effects(fit, nsims = 5),
    "metadata"
  )
  expect_error(
    estimate_stm_topic_effects(
      fit,
      formula = ~group,
      metadata = data.frame(group = "a"),
      nsims = 5
    ),
    "one row per fitted document"
  )
  expect_error(
    estimate_stm_topic_effects(
      fit,
      formula = ~group,
      metadata = data.frame(doc_id = rep(fit$doc_ids[1], length(fit$doc_ids)),
                            group = "a"),
      nsims = 5
    ),
    "duplicate"
  )
  expect_error(
    estimate_stm_topic_effects(
      fit,
      formula = ~group,
      metadata = data.frame(doc_id = paste0("other", seq_along(fit$doc_ids)),
                            group = "a"),
      nsims = 5
    ),
    "one row for each fitted document"
  )
  expect_error(
    estimate_stm_topic_effects(
      no_prevalence,
      metadata = data.frame(
        doc_id = no_prevalence$doc_ids,
        group = quanteda::docvars(make_stm_interpretation_dfm(), "group")
      ),
      nsims = 5
    ),
    "supply 'formula'"
  )
  expect_error(
    estimate_stm_topic_effects(
      fit,
      formula = "group",
      metadata = data.frame(
        doc_id = fit$doc_ids,
        group = quanteda::docvars(make_stm_interpretation_dfm(), "group")
      ),
      nsims = 5
    ),
    "formula"
  )
  expect_error(
    estimate_stm_topic_effects(fit, metadata = fit$docvars, conf_level = 1,
                               nsims = 5),
    "conf_level"
  )
})

test_that("STM interpretation internals validate edge cases", {
  fit <- fit_stm_interpretation_fixture(prevalence = TRUE, docvars = TRUE)
  not_stm <- structure(
    list(engine = "topicmodels", model_object = list()),
    class = c("nlp_topic_fit", "list")
  )

  expect_error(get_stm_topic_labels(not_stm), "STM fit")
  expect_error(summarize_stm_topics(list()), "STM nlp_topic_fit")
  expect_error(get_stm_topic_labels(fit, label_types = character()), "label_types")

  one_type <- NLPstudio:::.stm_label_tables_to_long(
    labels = list(prob = matrix("term", nrow = 1, ncol = 1)),
    topic_int = 1L,
    label_types = c("prob", "frex"),
    source = "unit"
  )
  expect_equal(unique(one_type$label_type), "prob")

  expect_error(NLPstudio:::.stm_topic_formula_lhs(integer()), "At least one")
  expect_equal(NLPstudio:::.stm_topic_formula_lhs(c(1L, 3L)), "c(1, 3) ~")

  bad_formula_fit <- fit
  bad_formula_fit$model_object$settings$covariates$formula <- "not-a-formula"
  expect_error(
    estimate_stm_topic_effects(bad_formula_fit, metadata = fit$docvars,
                               nsims = 5),
    "not a formula"
  )
})
