if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      "conf_high", "conf_low", "estimate", "label_type", "p_value",
      "rank", "source", "statistic", "std_error", "term", "terms", "topic_id",
      "topic_int"
    )
  )
}

#' Extract STM Topic Labels
#'
#' Return STM-native topic labels as a standardized long table. The helper wraps
#' [stm::labelTopics()] and, optionally, [stm::sageLabels()] while keeping
#' NLPstudio's canonical `Topic###` identifiers.
#'
#' @param x An STM `nlp_topic_fit` returned by [fit_topic_model()] or a raw
#'   **stm** `STM` object without content covariates.
#' @param n Integer. Number of terms per label type. Defaults to `7L`.
#' @param topics Optional topic filter supplied as numeric topic indices or
#'   `Topic###` identifiers.
#' @param label_types Character vector of STM label families to return. Valid
#'   values are `"prob"`, `"frex"`, `"lift"`, and `"score"`.
#' @param frexweight Numeric value in `[0, 1]` forwarded to
#'   [stm::labelTopics()] for FREX labels. Defaults to `0.5`.
#' @param include_sage Logical. Should [stm::sageLabels()] marginal labels also
#'   be included? Defaults to `FALSE`.
#'
#' @returns A [data.table][data.table::data.table] with columns `topic_id`,
#'   `topic_int`, `source`, `label_type`, `rank`, and `term`.
#'
#' @details
#' This function is STM-specific. It is meant to complement the engine-agnostic
#' [get_top_terms()] accessor when users want labels based on STM's own
#' probability, FREX, lift, score, and optional SAGE calculations.
#'
#' STM content-covariate models are not supported because they imply
#' covariate-specific topic-word distributions, while NLPstudio currently
#' standardizes one TWW matrix per fit.
#'
#' @examplesIf requireNamespace("stm", quietly = TRUE)
#' dtm <- methods::as(
#'   Matrix::Matrix(
#'     matrix(c(2, 1, 0, 0,  1, 2, 0, 0,  0, 0, 2, 1,
#'              0, 0, 1, 2,  2, 1, 0, 0,  0, 0, 1, 2),
#'            nrow = 6, byrow = TRUE),
#'     sparse = TRUE
#'   ),
#'   "dgCMatrix"
#' )
#' rownames(dtm) <- paste0("doc", 1:6)
#' colnames(dtm) <- c("growth", "profit", "risk", "loss")
#' fit <- fit_topic_model(
#'   dtm,
#'   engine = "stm",
#'   model = "stm",
#'   k = 2,
#'   control = list(fit = list(seed = 1, max.em.its = 5, verbose = FALSE))
#' )
#' get_stm_topic_labels(fit, n = 3)
#'
#' @export
get_stm_topic_labels <- function(x, n = 7L, topics = NULL,
                                 label_types = c("prob", "frex", "lift", "score"),
                                 frexweight = 0.5,
                                 include_sage = FALSE) {
  if (!requireNamespace("stm", quietly = TRUE)) {
    stop("Package 'stm' must be installed to extract STM topic labels.",
         call. = FALSE)
  }
  n <- .validate_positive_integer(n, "n")
  frexweight <- .validate_stm_frexweight(frexweight)
  include_sage <- .validate_stm_logical(include_sage, "include_sage")
  label_types <- .validate_stm_label_types(label_types)

  model <- .stm_model_from_supported(x)
  topic_info <- .resolve_stm_topics(model, topics)

  labels <- stm::labelTopics(
    model,
    topics = topic_info$topic_int,
    n = n,
    frexweight = frexweight
  )
  out <- .stm_label_tables_to_long(
    labels = labels,
    topic_int = topic_info$topic_int,
    label_types = label_types,
    source = "labelTopics"
  )

  if (include_sage) {
    sage <- stm::sageLabels(model, n = n)
    sage_out <- .stm_label_tables_to_long(
      labels = sage$marginal,
      topic_int = topic_info$topic_int,
      label_types = label_types,
      source = "sageLabels"
    )
    out <- data.table::rbindlist(list(out, sage_out), use.names = TRUE)
  }

  data.table::setorder(out, topic_int, source, label_type, rank)
  out[]
}

#' Summarize STM Topics
#'
#' Build a one-row-per-topic interpretation table for an STM fit, combining
#' NLPstudio's engine-agnostic topic summary with STM-native label columns.
#'
#' @param fit An STM `nlp_topic_fit` returned by [fit_topic_model()] or a raw
#'   **stm** `STM` object without content covariates.
#' @param training Optional training document-feature matrix forwarded to
#'   [summarize_topics()].
#' @param doc_data Optional document metadata or text source forwarded to
#'   [summarize_topics()].
#' @param top_n Integer. Number of probability top terms used by the generic
#'   topic summary. Defaults to `10L`.
#' @param representative_n Integer. Number of representative documents retained
#'   per topic. Defaults to `3L`.
#' @param include_text Logical. Should representative text be included when
#'   available? Defaults to `FALSE`.
#' @param docvars Logical. Should stored document variables be available for
#'   representative selection output? Defaults to `FALSE`.
#' @param label_n Integer. Number of STM-native label terms per label type.
#'   Defaults to `7L`.
#' @param label_types Character vector of STM label families. Valid values are
#'   `"prob"`, `"frex"`, `"lift"`, and `"score"`.
#' @param frexweight Numeric value in `[0, 1]` forwarded to
#'   [stm::labelTopics()]. Defaults to `0.5`.
#' @param include_sage Logical. Should SAGE marginal label columns be included?
#'   Defaults to `FALSE`.
#' @param doc_id_col Document-ID column name when `doc_data` is tabular.
#'   Defaults to `"doc_id"`.
#' @param text_col Text column name when `doc_data` is tabular. Defaults to
#'   `"text"`.
#'
#' @returns A [data.table][data.table::data.table] with one row per STM topic.
#'
#' @details
#' `summarize_stm_topics()` keeps [summarize_topics()] as the generic summary
#' engine and adds collapsed STM-native label columns such as
#' `stm_prob_terms`, `stm_frex_terms`, `stm_lift_terms`, and
#' `stm_score_terms`. When `include_sage = TRUE`, corresponding
#' `stm_sage_*_terms` columns are added.
#'
#' @examplesIf requireNamespace("stm", quietly = TRUE)
#' dtm <- methods::as(
#'   Matrix::Matrix(
#'     matrix(c(2, 1, 0, 0,  1, 2, 0, 0,  0, 0, 2, 1,
#'              0, 0, 1, 2,  2, 1, 0, 0,  0, 0, 1, 2),
#'            nrow = 6, byrow = TRUE),
#'     sparse = TRUE
#'   ),
#'   "dgCMatrix"
#' )
#' rownames(dtm) <- paste0("doc", 1:6)
#' colnames(dtm) <- c("growth", "profit", "risk", "loss")
#' fit <- fit_topic_model(
#'   dtm,
#'   engine = "stm",
#'   model = "stm",
#'   k = 2,
#'   control = list(fit = list(seed = 1, max.em.its = 5, verbose = FALSE))
#' )
#' summarize_stm_topics(fit, training = dtm, top_n = 3, label_n = 3)
#'
#' @export
summarize_stm_topics <- function(fit, training = NULL, doc_data = NULL,
                                 top_n = 10L, representative_n = 3L,
                                 include_text = FALSE, docvars = FALSE,
                                 label_n = 7L,
                                 label_types = c("prob", "frex", "lift", "score"),
                                 frexweight = 0.5,
                                 include_sage = FALSE,
                                 doc_id_col = "doc_id",
                                 text_col = "text") {
  stm_fit <- .stm_fit_from_supported(fit)
  out <- summarize_topics(
    stm_fit,
    training = training,
    doc_data = doc_data,
    top_n = top_n,
    representative_n = representative_n,
    include_text = include_text,
    docvars = docvars,
    doc_id_col = doc_id_col,
    text_col = text_col
  )

  labels <- get_stm_topic_labels(
    stm_fit,
    n = label_n,
    label_types = label_types,
    frexweight = frexweight,
    include_sage = include_sage
  )
  .add_stm_label_columns(out, labels)
}

#' Estimate STM Topic Effects
#'
#' Estimate and tidy STM prevalence effects for fitted topics.
#'
#' @param fit An STM `nlp_topic_fit` returned by [fit_topic_model()] or a raw
#'   **stm** `STM` object without content covariates.
#' @param formula Optional prevalence formula. If `NULL`, NLPstudio uses the
#'   prevalence formula stored in the STM fit. A right-hand-side-only formula
#'   such as `~ group` is combined with the selected topics. A full formula is
#'   forwarded as-is.
#' @param metadata Optional metadata for [stm::estimateEffect()]. If omitted,
#'   stored `fit$docvars` are used when available.
#' @param topics Optional topic filter supplied as numeric topic indices or
#'   `Topic###` identifiers. Ignored when `formula` is a full formula with its
#'   own left-hand side.
#' @param uncertainty Uncertainty mode forwarded to [stm::estimateEffect()].
#'   One of `"Global"`, `"Local"`, or `"None"`.
#' @param nsims Integer number of simulations forwarded to
#'   [stm::estimateEffect()]. Defaults to `25L`.
#' @param conf_level Confidence level used for Wald intervals in the returned
#'   table. Defaults to `0.95`.
#'
#' @returns An object of class
#'   `c("nlp_stm_topic_effects", "data.table", "data.frame")` with tidy
#'   coefficient rows. The raw `estimateEffect` object is attached as
#'   `attr(result, "estimate_effect")`.
#'
#' @details
#' This helper reports prevalence effects for STM fits. It does not add new STM
#' prediction behavior, and it does not support content-covariate STM models.
#'
#' @examplesIf requireNamespace("stm", quietly = TRUE)
#' texts <- c(
#'   doc1 = "profit revenue growth",
#'   doc2 = "profit margin growth",
#'   doc3 = "risk litigation loss",
#'   doc4 = "debt risk loss",
#'   doc5 = "revenue market profit",
#'   doc6 = "litigation cost risk"
#' )
#' corp <- quanteda::corpus(texts)
#' quanteda::docvars(corp, "group") <- rep(c("a", "b"), 3)
#' dfm <- quanteda::dfm(quanteda::tokens(corp))
#' fit <- fit_topic_model(
#'   dfm,
#'   engine = "stm",
#'   model = "stm",
#'   k = 2,
#'   docvars = TRUE,
#'   control = list(
#'     fit = list(
#'       prevalence = ~ group,
#'       seed = 1,
#'       max.em.its = 5,
#'       verbose = FALSE
#'     )
#'   )
#' )
#' estimate_stm_topic_effects(fit, nsims = 5)
#'
#' @export
estimate_stm_topic_effects <- function(fit, formula = NULL, metadata = NULL,
                                       topics = NULL,
                                       uncertainty = c("Global", "Local", "None"),
                                       nsims = 25L,
                                       conf_level = 0.95) {
  if (!requireNamespace("stm", quietly = TRUE)) {
    stop("Package 'stm' must be installed to estimate STM topic effects.",
         call. = FALSE)
  }

  uncertainty <- match.arg(uncertainty)
  nsims <- .validate_positive_integer(nsims, "nsims")
  conf_level <- .validate_stm_conf_level(conf_level)

  model <- .stm_model_from_supported(fit)
  meta <- .stm_effect_metadata(fit, metadata, model)
  effect_formula <- .stm_effect_formula(model, formula, topics)
  effect <- stm::estimateEffect(
    formula = effect_formula,
    stmobj = model,
    metadata = meta,
    uncertainty = uncertainty,
    nsims = nsims
  )

  out <- .tidy_stm_effect(effect, model, uncertainty, nsims, conf_level)
  data.table::setattr(out, "class", c("nlp_stm_topic_effects", "data.table", "data.frame"))
  data.table::setattr(out, "estimate_effect", effect)
  out[]
}

.stm_model_from_supported <- function(x) {
  if (inherits(x, "nlp_topic_fit")) {
    if (!identical(x$engine, "stm") || !.is_stm_object(x$model_object)) {
      stop("'x' must be an STM fit returned by fit_topic_model().",
           call. = FALSE)
    }
    model <- x$model_object
  } else if (.is_stm_object(x)) {
    model <- x
  } else {
    stop("'x' must be an STM nlp_topic_fit or a raw stm::stm() object.",
         call. = FALSE)
  }

  .stm_tww_matrix(model)
  model
}

.stm_fit_from_supported <- function(x) {
  if (inherits(x, "nlp_topic_fit")) {
    .stm_model_from_supported(x)
    return(x)
  }
  if (.is_stm_object(x)) {
    return(as_nlp_topic_fit(x))
  }
  stop("'fit' must be an STM nlp_topic_fit or a raw stm::stm() object.",
       call. = FALSE)
}

.resolve_stm_topics <- function(model, topics) {
  topic_ids <- .topic_ids(.stm_topic_count(model))
  selected_ids <- .resolve_topic_selector(topic_ids, topics)
  data.table::data.table(
    topic_id = selected_ids,
    topic_int = as.integer(sub("^Topic", "", selected_ids))
  )
}

.validate_stm_label_types <- function(label_types) {
  allowed <- c("prob", "frex", "lift", "score")
  if (!is.character(label_types) || !length(label_types) || anyNA(label_types)) {
    stop("'label_types' must contain one or more STM label types.",
         call. = FALSE)
  }
  bad <- setdiff(label_types, allowed)
  if (length(bad)) {
    stop(
      sprintf(
        "'label_types' must be a subset of: %s.",
        paste(allowed, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  unique(label_types)
}

.validate_stm_frexweight <- function(frexweight) {
  if (!is.numeric(frexweight) || length(frexweight) != 1L ||
      is.na(frexweight) || !is.finite(frexweight) ||
      frexweight < 0 || frexweight > 1) {
    stop("'frexweight' must be a single number in [0, 1].", call. = FALSE)
  }
  as.numeric(frexweight)
}

.validate_stm_logical <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("'%s' must be a single TRUE/FALSE value.", name),
         call. = FALSE)
  }
  x
}

.validate_stm_conf_level <- function(conf_level) {
  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      is.na(conf_level) || !is.finite(conf_level) ||
      conf_level <= 0 || conf_level >= 1) {
    stop("'conf_level' must be a single number in (0, 1).", call. = FALSE)
  }
  as.numeric(conf_level)
}

.stm_label_tables_to_long <- function(labels, topic_int, label_types, source) {
  pieces <- lapply(label_types, function(type) {
    if (!type %in% names(labels)) {
      return(data.table::data.table())
    }
    mat <- as.matrix(labels[[type]])
    mat <- mat[topic_int, , drop = FALSE]
    data.table::rbindlist(lapply(seq_along(topic_int), function(i) {
      terms <- as.character(mat[i, ])
      data.table::data.table(
        topic_id = .topic_ids(max(topic_int))[topic_int[i]],
        topic_int = topic_int[i],
        source = source,
        label_type = type,
        rank = seq_along(terms),
        term = terms
      )
    }))
  })

  out <- data.table::rbindlist(pieces, use.names = TRUE)
  out[!is.na(term) & nzchar(term)]
}

.add_stm_label_columns <- function(out, labels) {
  collapsed <- labels[
    ,
    .(terms = paste(term, collapse = ", ")),
    by = .(topic_id, source, label_type)
  ]

  for (src in unique(collapsed$source)) {
    prefix <- if (identical(src, "sageLabels")) "stm_sage" else "stm"
    src_dt <- collapsed[source == src]
    for (type in unique(src_dt$label_type)) {
      col <- paste0(prefix, "_", type, "_terms")
      values <- src_dt[label_type == type, .(topic_id, terms)]
      data.table::setnames(values, "terms", col)
      out <- merge(out, values, by = "topic_id", all.x = TRUE, sort = FALSE)
    }
  }
  out[]
}

.stm_effect_metadata <- function(fit, metadata, model) {
  if (!is.null(metadata)) {
    meta <- data.table::as.data.table(metadata)
  } else if (inherits(fit, "nlp_topic_fit") && !is.null(fit$docvars)) {
    meta <- data.table::as.data.table(fit$docvars)
  } else {
    stop(
      "STM topic effects require 'metadata' or an STM fit created with docvars = TRUE.",
      call. = FALSE
    )
  }

  doc_ids <- .stm_effect_doc_ids(fit, model)
  if ("doc_id" %in% names(meta)) {
    if (anyDuplicated(meta$doc_id)) {
      stop("STM topic-effect metadata must not contain duplicate 'doc_id' values.",
           call. = FALSE)
    }
    idx <- match(doc_ids, as.character(meta$doc_id))
    if (anyNA(idx)) {
      stop("STM topic-effect metadata must contain one row for each fitted document ID.",
           call. = FALSE)
    }
    meta <- meta[idx]
  } else if (nrow(meta) != length(doc_ids)) {
    stop(
      "STM topic-effect metadata must have one row per fitted document, or contain a 'doc_id' column.",
      call. = FALSE
    )
  }
  as.data.frame(meta)
}

.stm_effect_doc_ids <- function(fit, model) {
  if (inherits(fit, "nlp_topic_fit") && !is.null(fit$doc_ids)) {
    return(as.character(fit$doc_ids))
  }
  .stm_doc_ids(model)
}

.stm_effect_formula <- function(model, formula, topics) {
  if (!is.null(formula) && !inherits(formula, "formula")) {
    stop("'formula' must be NULL or a formula.", call. = FALSE)
  }

  if (!is.null(formula) && length(formula) == 3L) {
    if (!is.null(topics)) {
      warning(
        "'topics' is ignored because 'formula' already contains a left-hand side.",
        call. = FALSE
      )
    }
    return(formula)
  }

  topic_info <- .resolve_stm_topics(model, topics)
  lhs <- .stm_topic_formula_lhs(topic_info$topic_int)
  rhs_formula <- formula
  if (is.null(rhs_formula)) {
    rhs_formula <- model$settings$covariates$formula
  }
  if (is.null(rhs_formula)) {
    stop(
      "No STM prevalence formula is stored in this fit; supply 'formula' and 'metadata'.",
      call. = FALSE
    )
  }
  if (!inherits(rhs_formula, "formula")) {
    stop("The stored STM prevalence formula is not a formula.", call. = FALSE)
  }

  rhs <- if (length(rhs_formula) == 2L) {
    rhs_formula[[2L]]
  } else {
    rhs_formula[[3L]]
  }
  stats::as.formula(
    paste(lhs, paste(deparse(rhs, width.cutoff = 500L), collapse = " ")),
    env = environment(rhs_formula)
  )
}

.stm_topic_formula_lhs <- function(topic_int) {
  topic_int <- as.integer(topic_int)
  if (!length(topic_int)) {
    stop("At least one STM topic is required.", call. = FALSE)
  }
  if (length(topic_int) == 1L) {
    return(sprintf("%d ~", topic_int))
  }
  if (all(diff(topic_int) == 1L)) {
    return(sprintf("%d:%d ~", min(topic_int), max(topic_int)))
  }
  sprintf("c(%s) ~", paste(topic_int, collapse = ", "))
}

.tidy_stm_effect <- function(effect, model, uncertainty, nsims, conf_level) {
  topics <- as.integer(effect$topics)
  if (!length(topics)) {
    topics <- seq_len(.stm_topic_count(model))
  }
  z <- stats::qnorm(1 - (1 - conf_level) / 2)

  out <- data.table::rbindlist(lapply(topics, function(topic) {
    summary <- summary(effect, topics = topic)
    table <- as.matrix(summary$tables[[1L]])
    data.table::data.table(
      topic_id = .topic_ids(.stm_topic_count(model))[topic],
      topic_int = topic,
      term = rownames(table),
      estimate = as.numeric(table[, "Estimate"]),
      std_error = as.numeric(table[, "Std. Error"]),
      statistic = as.numeric(table[, "t value"]),
      p_value = as.numeric(table[, "Pr(>|t|)"])
    )
  }), use.names = TRUE)

  out[, conf_low := estimate - z * std_error]
  out[, conf_high := estimate + z * std_error]
  out[, uncertainty := uncertainty]
  out[, nsims := nsims]
  data.table::setcolorder(
    out,
    c(
      "topic_id", "topic_int", "term", "estimate", "std_error",
      "statistic", "p_value", "conf_low", "conf_high",
      "uncertainty", "nsims"
    )
  )
  out[]
}
