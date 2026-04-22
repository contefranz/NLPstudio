if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("probability"))
}

#' Extract Top Terms from Standardized TWW
#'
#' Extract the top `n` highest-probability terms from each topic. The function
#' works across all topic-model backends supported by `get_tww()`.
#'
#' @param x A supported topic-model object accepted by `get_tww()`.
#' @param n Integer. Number of top terms to extract per topic. Defaults to `10`.
#' @param topics Optional topic filter supplied either as numeric indices or as
#'   `Topic###` identifiers. If `NULL`, all topics are included.
#' @param format Output format. Either `"long"` (default) or `"wide"`.
#'
#' @returns A `data.table`.
#'
#' - `"long"` format contains one row per `(topic, term)` pair with columns
#'   `rank`, `topic`, `term`, and `probability`.
#' - `"wide"` format contains one row per rank with two columns per topic:
#'   `<Topic###>_term` and `<Topic###>_prob`.
#'
#' @details
#' `get_top_terms()` first extracts standardized TWW via `get_tww()`, then
#' ranks terms within each topic by descending probability.
#'
#' @seealso `fit_topic_model()`, `get_tww()`, [plot_top_terms()], [plot_dtw()]
#'
#' @examples
#' dtm <- methods::as(
#'   Matrix::Matrix(
#'     matrix(
#'       c(1, 0, 1,
#'         1, 1, 0,
#'         0, 1, 1,
#'         1, 1, 1),
#'       nrow = 4,
#'       byrow = TRUE
#'     ),
#'     sparse = TRUE
#'   ),
#'   "dgCMatrix"
#' )
#' rownames(dtm) <- paste0("doc", 1:4)
#' colnames(dtm) <- paste0("term", 1:3)
#'
#' fit <- fit_topic_model(
#'   dtm,
#'   engine = "text2vec",
#'   model = "lda",
#'   k = 2,
#'   fit_control = list(n_iter = 25, progressbar = FALSE)
#' )
#'
#' get_top_terms(fit, n = 2, format = "long")
#'
#' @export
get_top_terms <- function(x, n = 10, topics = NULL, format = c("long", "wide")) {
  format <- match.arg(format)

  if (!is.numeric(n) || length(n) != 1L || n < 1 || n != as.integer(n)) {
    stop("n must be a single positive integer.")
  }

  tww <- get_tww(x)
  topic_ids <- .resolve_topic_selector(tww$topic_id, topics)
  tww <- tww[match(topic_ids, tww$topic_id), ]
  term_cols <- setdiff(names(tww), "topic_id")

  long_out <- data.table::rbindlist(
    lapply(seq_len(nrow(tww)), function(i) {
      probs <- as.numeric(tww[i, term_cols, with = FALSE])
      nn <- min(as.integer(n), length(probs))
      idx <- utils::head(order(probs, decreasing = TRUE), nn)
      data.table::data.table(
        rank = seq_len(nn),
        topic = tww$topic_id[i],
        term = term_cols[idx],
        probability = probs[idx]
      )
    })
  )

  if (identical(format, "long")) {
    return(long_out[])
  }

  wide_list <- lapply(topic_ids, function(topic_id) {
    topic_dt <- long_out[topic == topic_id, .(rank, term, probability)]
    data.table::setnames(
      topic_dt,
      old = c("term", "probability"),
      new = c(paste0(topic_id, "_term"), paste0(topic_id, "_prob"))
    )
    topic_dt
  })

  wide_out <- Reduce(function(a, b) merge(a, b, by = "rank", all = TRUE), wide_list)
  data.table::setkey(wide_out, NULL)
  wide_out[]
}
