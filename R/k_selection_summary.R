if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "k", "level", "metric", "N", "optop_pval", "supported", "topic_id",
    "value"
  ))
}

#' Summarize Topic-Count Selection Results
#'
#' Convert an `nlp_k_selection` object into a wide, export-ready table with one
#' row per candidate topic count.
#'
#' @param selection An `nlp_k_selection` object returned by
#'   [select_k_topics()].
#' @param optop Optional output from `OpTop::optimal_topic()`. The common
#'   **OpTop** result shape with columns `topic`, `OpTop`, and `pval` is
#'   supported, as are cleaned tables with `k`, `optop`, and p-value aliases
#'   such as `p_value` or `p.value`.
#' @param include_unsupported Logical. Should unsupported aggregate metrics be
#'   retained as `NA_real_` columns? Defaults to `FALSE`.
#'
#' @returns An object of class
#'   `c("nlp_k_selection_summary", "data.table", "data.frame")` with one row
#'   per `k`. Aggregate metrics are widened into columns. Topic-level rows are
#'   preserved in `attr(result, "topic_metrics")` when present. Unsupported
#'   aggregate rows are preserved in `attr(result, "unsupported")` when
#'   `include_unsupported = TRUE`.
#'
#' @details
#' This helper reports already-computed evidence; it does not fit models, call
#' **OpTop**, compute a composite score, or choose the best value of \eqn{K}.
#' Use it after [select_k_topics()] and, optionally, after an external
#' `OpTop::optimal_topic()` call.
#'
#' @examples
#' selection <- data.table::data.table(
#'   k = c(2L, 3L),
#'   metric = "diversity",
#'   level = "aggregate",
#'   topic_id = NA_character_,
#'   value = c(0.75, 0.82),
#'   supported = TRUE
#' )
#' class(selection) <- c("nlp_k_selection", class(selection))
#'
#' summarize_k_selection(selection)
#'
#' @seealso [select_k_topics()], [as_optop_input()]
#' @export
summarize_k_selection <- function(selection, optop = NULL,
                                  include_unsupported = FALSE) {
  .validate_k_selection_summary_args(selection, include_unsupported)

  x <- data.table::as.data.table(data.table::copy(selection))
  aggregate <- x[level == "aggregate" & is.na(topic_id)]
  topic_rows <- x[!(level == "aggregate" & is.na(topic_id))]
  unsupported <- aggregate[supported == FALSE]

  metric_rows <- if (include_unsupported) aggregate else aggregate[supported == TRUE]
  if (nrow(metric_rows)) {
    dup <- metric_rows[, .N, by = .(k, metric)][N > 1L]
    if (nrow(dup)) {
      stop(
        "selection contains duplicate aggregate metric rows for the same 'k' and 'metric'.",
        call. = FALSE
      )
    }
  }

  k_values <- sort(unique(as.integer(x$k)))
  out <- data.table::data.table(k = k_values)

  if (nrow(metric_rows)) {
    metric_rows <- data.table::copy(metric_rows)
    metric_rows[supported == FALSE, value := NA_real_]
    metric_order <- unique(metric_rows$metric)
    wide <- data.table::dcast(
      metric_rows,
      k ~ metric,
      value.var = "value"
    )
    out <- merge(out, wide, by = "k", all.x = TRUE, sort = FALSE)
    data.table::setcolorder(out, c("k", metric_order))
  }

  optop_table <- .normalize_optop_selection_output(optop)
  if (!is.null(optop_table)) {
    extra_k <- setdiff(optop_table$k, out$k)
    if (length(extra_k)) {
      warning(
        "OpTop output contains K values not present in selection; extra rows were ignored.",
        call. = FALSE
      )
    }
    out <- merge(out, optop_table, by = "k", all.x = TRUE, sort = FALSE)
  }

  data.table::setorder(out, k)
  data.table::setattr(out, "class", c("nlp_k_selection_summary", "data.table", "data.frame"))

  if (nrow(topic_rows)) {
    data.table::setattr(out, "topic_metrics", topic_rows)
  }
  if (include_unsupported && nrow(unsupported)) {
    data.table::setattr(out, "unsupported", unsupported)
  }
  out[]
}

#' Print K-selection summary
#'
#' @param x An `nlp_k_selection_summary` object.
#' @param ... Unused.
#' @returns Invisibly returns `x`.
#' @export
print.nlp_k_selection_summary <- function(x, ...) {
  metric_cols <- setdiff(names(x), "k")
  optop_cols <- grep("^optop", metric_cols, value = TRUE)

  cat("<nlp_k_selection_summary>\n")
  cat(sprintf("  candidate K values: %s\n", paste(x$k, collapse = ", ")))
  cat(sprintf("  columns: %s\n", paste(metric_cols, collapse = ", ")))
  cat(sprintf("  OpTop: %s\n", if (length(optop_cols)) "included" else "not included"))
  if (!is.null(attr(x, "topic_metrics", exact = TRUE))) {
    cat("  topic-level metrics: attached\n")
  }
  if (!is.null(attr(x, "unsupported", exact = TRUE))) {
    cat("  unsupported aggregate metrics: attached\n")
  }
  invisible(x)
}

.validate_k_selection_summary_args <- function(selection, include_unsupported) {
  if (!inherits(selection, "nlp_k_selection")) {
    stop("'selection' must be an nlp_k_selection object returned by select_k_topics().",
         call. = FALSE)
  }
  if (!is.logical(include_unsupported) || length(include_unsupported) != 1L ||
      is.na(include_unsupported)) {
    stop("'include_unsupported' must be a single TRUE/FALSE value.", call. = FALSE)
  }

  required <- c("k", "metric", "level", "topic_id", "value", "supported")
  missing <- setdiff(required, names(selection))
  if (length(missing)) {
    stop(
      sprintf(
        "'selection' is missing required columns: %s.",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(selection)
}

.normalize_optop_selection_output <- function(optop) {
  if (is.null(optop)) {
    return(NULL)
  }
  if (inherits(optop, "nlp_optop_input")) {
    stop(
      "'optop' must be the result returned by OpTop::optimal_topic(), not an nlp_optop_input object.",
      call. = FALSE
    )
  }

  dt <- .as_optop_result_table(optop)
  if (!nrow(dt)) {
    stop("'optop' must contain at least one row.", call. = FALSE)
  }

  name_map <- stats::setNames(names(dt), tolower(names(dt)))
  k_name <- .first_named_match(name_map, c("k", "topic", "topics"))
  if (is.null(k_name)) {
    stop("'optop' must contain a K column named 'k', 'topic', or 'topics'.",
         call. = FALSE)
  }

  out <- data.table::data.table(k = .coerce_optop_k(dt[[k_name]]))

  stat_names <- character()
  optop_name <- .first_named_match(name_map, c("optop"))
  if (!is.null(optop_name)) {
    out[, optop := as.numeric(dt[[optop_name]])]
    stat_names <- c(stat_names, "optop")
  }

  pval_name <- .first_named_match(name_map, c("pval", "p.value", "p_value", "pvalue", "p"))
  if (!is.null(pval_name)) {
    out[, optop_pval := as.numeric(dt[[pval_name]])]
    stat_names <- c(stat_names, "optop_pval")
  }

  if (!length(stat_names)) {
    stop(
      "'optop' does not contain recognizable statistic columns such as 'OpTop' or 'pval'.",
      call. = FALSE
    )
  }
  if (anyDuplicated(out$k)) {
    stop("'optop' must contain at most one row per K value.", call. = FALSE)
  }

  out[]
}

.as_optop_result_table <- function(optop) {
  if (data.table::is.data.table(optop)) {
    return(data.table::copy(optop))
  }
  if (is.data.frame(optop)) {
    return(data.table::as.data.table(optop))
  }
  out <- tryCatch(
    data.table::as.data.table(as.data.frame(optop, stringsAsFactors = FALSE)),
    error = function(e) NULL
  )
  if (is.null(out)) {
    stop("'optop' must be a data.frame, data.table, or table-like list.",
         call. = FALSE)
  }
  out
}

.first_named_match <- function(name_map, candidates) {
  hit <- intersect(candidates, names(name_map))
  if (!length(hit)) {
    return(NULL)
  }
  unname(name_map[[hit[1L]]])
}

.coerce_optop_k <- function(x) {
  out <- suppressWarnings(as.integer(x))
  if (length(out) == 0L || anyNA(out) || any(out < 1L) ||
      any(as.character(out) != as.character(x))) {
    stop("'optop' K values must be positive integers.", call. = FALSE)
  }
  out
}
