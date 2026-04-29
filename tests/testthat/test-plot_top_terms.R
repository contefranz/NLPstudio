make_top_terms_table <- function() {
  data.table::data.table(
    rank = c(1L, 2L, 1L, 2L),
    topic = c("Topic001", "Topic001", "Topic002", "Topic002"),
    term = c("apple", "banana", "carrot", "banana"),
    probability = c(0.6, 0.3, 0.7, 0.2)
  )
}

test_that("plot_top_terms reports missing tidytext dependency", {
  testthat::local_mocked_bindings(
    .has_namespace = function(pkg) FALSE,
    .package = "NLPstudio"
  )

  expect_error(
    plot_top_terms(make_top_terms_table()),
    "Package 'tidytext' is required"
  )
})

test_that("plot_top_terms rejects non-long top-term tables", {
  testthat::local_mocked_bindings(
    .has_namespace = function(pkg) TRUE,
    .package = "NLPstudio"
  )

  expect_error(
    plot_top_terms(data.table::data.table(rank = 1L, Topic001_term = "apple")),
    "Use get_top_terms"
  )
})

test_that("plot_top_terms returns a ggplot object", {
  skip_if_not_installed("tidytext")

  top_terms <- make_top_terms_table()
  plot_obj <- plot_top_terms(
    data.table::copy(top_terms),
    facet_args = list(scales = "free_y", ncol = 1),
    fill = "steelblue"
  )

  expect_s3_class(plot_obj, "ggplot")
  expect_equal(plot_obj$labels$title, "Top Terms per Topic")
  expect_equal(plot_obj$labels$x, "Topic-Word Probability")
})
