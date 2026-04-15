test_that(".validate_parallel_args rejects invalid ncores", {
  expect_error(NLPstudio:::.validate_parallel_args(0, 1), "ncores must be a single positive integer")
  expect_error(NLPstudio:::.validate_parallel_args(-1, 1), "ncores must be a single positive integer")
  expect_error(NLPstudio:::.validate_parallel_args(1.5, 1), "ncores must be a single positive integer")
  expect_error(NLPstudio:::.validate_parallel_args("a", 1), "ncores must be a single positive integer")
  expect_error(NLPstudio:::.validate_parallel_args(c(1, 2), 1), "ncores must be a single positive integer")
})

test_that(".validate_parallel_args rejects invalid nchunks", {
  expect_error(NLPstudio:::.validate_parallel_args(1, 0), "nchunks must be a single positive integer")
  expect_error(NLPstudio:::.validate_parallel_args(1, -1), "nchunks must be a single positive integer")
  expect_error(NLPstudio:::.validate_parallel_args(1, 1.5), "nchunks must be a single positive integer")
})

test_that(".validate_parallel_args accepts valid inputs", {
  expect_no_error(NLPstudio:::.validate_parallel_args(1, 1))
  expect_no_error(NLPstudio:::.validate_parallel_args(4, 8))
  # Accept numeric that equals integer
  expect_no_error(NLPstudio:::.validate_parallel_args(2.0, 2.0))
})

test_that(".run_parallel works sequentially via PSOCK with 1 core", {
  chunks <- list(1:3, 4:6, 7:9)
  result <- NLPstudio:::.run_parallel(chunks, sum, ncores = 1L, socket = "PSOCK")
  expect_equal(result, list(6L, 15L, 24L))
})

test_that(".run_parallel passes extra arguments to FUN", {
  chunks <- list(c(1, NA, 3), c(4, NA, 6))
  result <- NLPstudio:::.run_parallel(chunks, sum, ncores = 1L, socket = "PSOCK", na.rm = TRUE)
  expect_equal(result, list(4, 10))
})

test_that(".run_parallel FORK errors on Windows", {
  skip_on_os("windows")
  # On non-Windows, FORK should work but emit a warning
  chunks <- list(1:3)
  expect_warning(
    NLPstudio:::.run_parallel(chunks, sum, ncores = 1L, socket = "FORK"),
    "FORK sockets"
  )
})
