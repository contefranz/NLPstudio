test_that("set_ff_industries has been removed from the public API", {
  expect_error(
    getExportedValue("NLPstudio", "set_ff_industries"),
    "not an exported object"
  )
})
