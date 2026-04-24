test_that(".coherence_prepare_training binarizes and aligns to vocab", {
  # 4 docs x 3 terms
  dtm <- Matrix::sparseMatrix(
    i = c(1, 1, 2, 2, 3, 3, 4, 4, 4),
    j = c(1, 2, 1, 3, 2, 3, 1, 2, 3),
    x = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
    dims = c(4L, 3L),
    dimnames = list(paste0("doc", 1:4), c("a", "b", "c"))
  )

  result <- NLPstudio:::.coherence_prepare_training(dtm, c("a", "b", "c"))

  expect_true(methods::is(result, "dgCMatrix"))
  expect_equal(dim(result), c(4L, 3L))
  expect_equal(colnames(result), c("a", "b", "c"))
  # All non-zero values must be exactly 1 (binarized)
  expect_true(all(result@x == 1))
  # Term doc-frequencies
  expect_equal(as.numeric(Matrix::colSums(result)), c(3, 3, 3))
})

test_that(".coherence_prepare_training reorders columns to match vocab", {
  dtm <- Matrix::sparseMatrix(
    i = c(1, 1, 2, 3),
    j = c(1, 2, 2, 3),
    x = c(5, 2, 3, 1),  # raw counts, should be binarized
    dims = c(3L, 3L),
    dimnames = list(paste0("d", 1:3), c("b", "a", "c"))
  )
  # vocab wants a, b, c order
  result <- NLPstudio:::.coherence_prepare_training(dtm, c("a", "b", "c"))
  expect_equal(colnames(result), c("a", "b", "c"))
  # All non-zero values must be 1
  expect_true(all(result@x == 1))
})

test_that(".coherence_prepare_training adds zero columns for missing vocab terms", {
  dtm <- Matrix::sparseMatrix(
    i = c(1, 2),
    j = c(1, 1),
    x = c(1, 1),
    dims = c(2L, 1L),
    dimnames = list(c("d1", "d2"), "a")
  )
  expect_warning(
    NLPstudio:::.coherence_prepare_training(dtm, c("a", "b", "c")),
    "vocabulary terms"
  )
  result <- suppressWarnings(
    NLPstudio:::.coherence_prepare_training(dtm, c("a", "b", "c"))
  )
  expect_equal(dim(result), c(2L, 3L))
  expect_equal(colnames(result), c("a", "b", "c"))
  # b and c columns should be all zero
  expect_equal(as.numeric(Matrix::colSums(result)), c(2, 0, 0))
})

test_that(".compute_coherence returns correct UMass on symmetric corpus", {
  # 4 docs x 3 terms, all pairs equally co-occurring
  # doc1: a b  | doc2: a c  | doc3: b c  | doc4: a b c
  dtm <- Matrix::sparseMatrix(
    i = c(1, 1, 2, 2, 3, 3, 4, 4, 4),
    j = c(1, 2, 1, 3, 2, 3, 1, 2, 3),
    x = rep(1, 9),
    dims = c(4L, 3L),
    dimnames = list(paste0("doc", 1:4), c("a", "b", "c"))
  )

  # D(a)=3, D(b)=3, D(c)=3, D(a,b)=2, D(a,c)=2, D(b,c)=2, D_total=4

  # TWW matrix: topic 1 ranks a > b > c; topic 2 ranks b > c > a
  tww <- matrix(
    c(0.6, 0.3, 0.1,   # topic 1: a most probable
      0.1, 0.6, 0.3),  # topic 2: b most probable
    nrow = 2,
    dimnames = list(c("Topic001", "Topic002"), c("a", "b", "c"))
  )

  eps <- 1e-12
  result <- NLPstudio:::.compute_coherence(tww, dtm, top_n = 3L, epsilon = eps)

  # All pairs in the symmetric corpus: log((2 + eps)/(3 + eps)) = log(2/3)
  expected_umass <- log(2 / 3)

  expect_equal(result$umass[1], expected_umass, tolerance = 1e-9)
  expect_equal(result$umass[2], expected_umass, tolerance = 1e-9)
})

test_that(".compute_coherence returns correct NPMI on symmetric corpus", {
  dtm <- Matrix::sparseMatrix(
    i = c(1, 1, 2, 2, 3, 3, 4, 4, 4),
    j = c(1, 2, 1, 3, 2, 3, 1, 2, 3),
    x = rep(1, 9),
    dims = c(4L, 3L),
    dimnames = list(paste0("doc", 1:4), c("a", "b", "c"))
  )

  tww <- matrix(
    c(0.6, 0.3, 0.1,
      0.1, 0.6, 0.3),
    nrow = 2,
    dimnames = list(c("Topic001", "Topic002"), c("a", "b", "c"))
  )

  # Hand-computed:
  # P(a)=P(b)=P(c)=3/4, P(any pair)=2/4=0.5
  # log_p_pair = log(0.5), log_p_term = log(0.75)
  # npmi_num = log(0.5) - 2*log(0.75) = log(0.5/0.75^2) = log(8/9)
  # npmi_denom = -log(0.5) = log(2)
  # npmi = log(8/9) / log(2)
  eps <- 1e-12
  expected_npmi <- (log(0.5) - 2 * log(0.75)) / (-log(0.5))

  result <- NLPstudio:::.compute_coherence(tww, dtm, top_n = 3L, epsilon = eps)

  expect_equal(result$npmi[1], expected_npmi, tolerance = 1e-9)
  expect_equal(result$npmi[2], expected_npmi, tolerance = 1e-9)
})

test_that(".compute_coherence is correct on asymmetric corpus with top_n = 2", {
  # 5 docs x 3 terms
  # doc1: x y | doc2: x | doc3: y z | doc4: x y z | doc5: z
  dtm <- Matrix::sparseMatrix(
    i = c(1, 1, 2, 3, 3, 4, 4, 4, 5),
    j = c(1, 2, 1, 2, 3, 1, 2, 3, 3),
    x = rep(1, 9),
    dims = c(5L, 3L),
    dimnames = list(paste0("d", 1:5), c("x", "y", "z"))
  )

  # D(x)=3, D(y)=3, D(z)=3
  # D(x,y)=2, D(x,z)=1, D(y,z)=2, D_total=5

  # Topic: x most probable, y second (top_n=2 -> single pair (x, y))
  tww <- matrix(
    c(0.7, 0.2, 0.1),
    nrow = 1,
    dimnames = list("Topic001", c("x", "y", "z"))
  )

  eps <- 1e-12

  # UMass: log((D(x,y) + eps) / (D(x) + eps)) = log(2/3)
  expected_umass <- log(2 / 3)

  # NPMI: P(x)=3/5, P(y)=3/5, P(x,y)=2/5
  # log_p_xy = log(2/5), log_p_x = log(3/5), log_p_y = log(3/5)
  # num = log(2/5) - 2*log(3/5) = log((2/5) / (3/5)^2) = log(2*5 / 9) = log(10/9)
  # denom = -log(2/5)
  expected_npmi <- (log(2 / 5) - 2 * log(3 / 5)) / (-log(2 / 5))

  result <- NLPstudio:::.compute_coherence(tww, dtm, top_n = 2L, epsilon = eps)

  expect_equal(result$umass, expected_umass, tolerance = 1e-9)
  expect_equal(result$npmi,  expected_npmi,  tolerance = 1e-9)
})

test_that(".compute_coherence handles fewer than 2 top terms gracefully", {
  dtm <- Matrix::sparseMatrix(
    i = 1L, j = 1L, x = 1,
    dims = c(1L, 1L),
    dimnames = list("d1", "w1")
  )
  tww <- matrix(1, nrow = 1, dimnames = list("Topic001", "w1"))

  result <- NLPstudio:::.compute_coherence(tww, dtm, top_n = 1L, epsilon = 1e-12)
  expect_true(is.na(result$umass))
  expect_true(is.na(result$npmi))
})

test_that(".compute_coherence respects top_n when top_n < V", {
  # V = 4 terms; top_n = 2 -> only first 2 most probable terms used
  dtm <- Matrix::sparseMatrix(
    i = c(1, 1, 1, 1, 2, 2, 2),
    j = c(1, 2, 3, 4, 1, 2, 3),
    x = rep(1, 7),
    dims = c(2L, 4L),
    dimnames = list(c("d1", "d2"), c("a", "b", "c", "d"))
  )
  # phi ranks: a > b >> c >> d
  tww <- matrix(c(0.5, 0.3, 0.15, 0.05), nrow = 1,
                dimnames = list("Topic001", c("a", "b", "c", "d")))

  eps <- 1e-12
  result_2 <- NLPstudio:::.compute_coherence(tww, dtm, top_n = 2L, epsilon = eps)
  result_4 <- NLPstudio:::.compute_coherence(tww, dtm, top_n = 4L, epsilon = eps)

  # top_n=2 uses only (a,b); top_n=4 uses all 6 pairs -> different means
  expect_false(isTRUE(all.equal(result_2$umass, result_4$umass)))
})

test_that(".compute_coherence NPMI is clamped to [-1, 1]", {
  # Perfectly co-occurring terms (always appear together): NPMI -> 1
  dtm <- Matrix::sparseMatrix(
    i = c(1, 1, 2, 2),
    j = c(1, 2, 1, 2),
    x = rep(1, 4),
    dims = c(2L, 2L),
    dimnames = list(c("d1", "d2"), c("a", "b"))
  )
  # D(a)=D(b)=D(a,b)=2, D_total=2 -> P(a,b)=1, P(a)=P(b)=1
  # NPMI undefined (log(1)/(-log(1))) -> handled as 1 per clamp rule
  tww <- matrix(c(0.7, 0.3), nrow = 1, dimnames = list("Topic001", c("a", "b")))
  result <- NLPstudio:::.compute_coherence(tww, dtm, top_n = 2L, epsilon = 1e-12)
  expect_true(result$npmi >= -1 && result$npmi <= 1)
})
