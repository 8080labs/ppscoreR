context('test calculation.R')

test_that("test normalized_f1_score", {
  expect_equal(normalized_f1_score(0.4, 0.5), 0)
  expect_equal(normalized_f1_score(0.75, 0.5), 0.5)
})

test_that("test normalized_mae_score", {
  expect_equal(normalized_mae_score(10, 5), 0)
  expect_equal(normalized_mae_score(5, 10), 0.5)
})

test_that("test maybe_sample", {
  df =
  expect_equal(nrow(maybe_sample(df, 10)), 10)
})

test_that("test infer_task", {

  expect_match(infer_task(df, "Age", "Age"), "predict_itself")
  df['constant'] <- 1
  expect_match(infer_task(df, "Age", "constant"), "predict_constant")

  expect_match(infer_task(df, "Age", "Survived"), "classification")

  #TODO:df reset_index
  #TODO:change index to string type
  expect_match(infer_task(df, "Age", "id"), "predict_id")

  # classification because numeric but few categories
  expect_match(infer_task(df, "Age", "SibSp"), "classification")

  df["Pclass_category"] = as.factor(df['Pclass'])
  expect_match(infer_task(df, "Age", "Pclass_category"), "classification")

  df["Pclass_datetime"] = as.Date(df['Pclass'])
  #TODO: raise exception

  expect_match(infer_task(df, "Survived", "Age"), "regression")

})

test_that("test score", {
  x <- runif(1000, -2, 2)
  error <- runif(1000, -0.5, 0.5)
  y <- x * x + error
  df <- data.frame(x, error, y)

  df['constant'] <- 1
  rownames(df) <- NULL
  df['id'] <- as.character(rownames(df))

  df['x_greater_0'] <- df['x'] > 0
  df['x_greater_0'] <- as.character(df['x_greater_9'])

  df['nan'] <- NA
  #TODO: raise exception

  expect_match(score(df, "x", "y", "regression")$task, "regression")

  expect_match(score(df, "x", "constant")$task, "predict_constant")
  expect_match(score(df, "x", "x")$task, "predict_itself")
  expect_match(score(df, "x", "id")$task, "predict_id")

  #feature is id
  expect_equal(score(df, "id", "y")$ppscore, 0)

  #numeric feature and target
  expect_more_than(score(df, "x", "y")$ppscore, 0.5)
  expect_less_than(score(df, "y", "x")$ppscore, 0.05)

  #object feature or target
  expect_more_than(score(df, "x", "x_greater_0")$ppscore, 0.6)
  expect_less_than(score(df, "x_greater_0", "x")$ppscore, 0.6)
})

test_that("test matrix", {

  df = df[c("Age", "Survived")]

  expect_match(class(matrix(df)), "data.frame")
  expect_match(class(matrix(df, output = "list")), "list")

  #TODO
})

