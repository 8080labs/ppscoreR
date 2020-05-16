context('test calculation.R')

test_that("normalized_f1_score works", {
  expect_equal(normalized_f1_score(0.4, 0.5), 0)
  expect_equal(normalized_f1_score(0.75, 0.5), 0.5)
})

test_that("normalized_mae_score works", {
  expect_equal(normalized_mae_score(10, 5), 0)
  expect_equal(normalized_mae_score(5, 10), 0.5)
})

