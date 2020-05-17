CV_ITERATIONS <- 4

NUMERIC_AS_CATEGORIC_BREAKPOINT <- 15

calculate_model_cv_score <- function(df, target, feature, metric, model) {

}

normalized_mae_score <- function(model_mae, naive_mae) {
  ### Normalize the model MAE score, given the baseline score
  ## Value range of MAE is [0, infinity), 0 is best
  # 10, 5 >> 0 because worse than naive
  # 10, 20 >> 0.5
  # 5, 20 >> 0.75 = 1 - (mae/base_mae)
  if (model_mae > naive_mae) {
    return(0)
  }
  else {
    return(1 - (model_mae / naive_mae))
  }
}

 mae_normalizer <- function(df, y, model_score) {
   ## In case of MAE, calculates the baseline score for y and derives the PPS.
   df["naive"] = median(df[[y]], na.rm = TRUE)
   baseline_score = hydroGOF::mae(df[y], df['naive'], na.rm = TRUE)[[1]]

   ppscore = normalized_mae_score(abs(model_score), baseline_score)
   mae_normalizer_returnlist = list("ppscore" = ppscore, "baseline_score" = baseline_score)
   return(mae_normalizer_returnlist)
}

normalized_f1_score <- function(model_f1, baseline_f1) {
  ### Normalizes the model F1 score, given the baseline score
  ## F1 ranges from 0 to 1
  ## 1 is best
  # 0.5, 0.7 >> 0 because worse than naive
  # 0.75, 0.5 >> 0.5
  if (model_f1 < baseline_f1) {
    return(0)
  }
  else {
    scale_range = 1.0 - baseline_f1 # eg 0.3
    f1_diff = model_f1 - baseline_f1 # eg 0.1
    return(f1_diff / scale_range) # 0.1/0.3 = 0.33
  }
}

f1_normalizer <- function(df, y, model_score) {
  ### In case of F1, calculates the baseline score for y and derive the PPS.
  df["naive"] = #TODO
  baseline_score = #TODO

  ppscore = normalized_f1_score(model_score, baseline_score)
  f1_normalizer_returnlist = list("ppscore" = ppscore, "baseline_score" = baseline_score)
  return(f1_normalizer_returnlist)
}

infer_task <- function(df, x, y) {
  ## Returns str with the name of the inferred task based on the columns x and y
  if (x == y) {
    return("predict_itself")
  }

  category_count = length(unique(na.omit(df[x])))
  if (category_count == 1) {
    return("predict_constant")
  }
  if (category_count == 2) {
    return("classification")
  }
  if (category_count == nrow(df[y]) & ) {
    #TODO:is_string_dtype or is_categorical_dtype)
    return("predict_id")
  }
  if (category_count <= NUMERIC_AS_CATEGORIC_BREAKPOINT & ) {
    #TODO:is_numeric_dtype
    return("classification")
  }

}


}

feature_is_id <- function(df, x) {
  ## Returns Boolean if t he feature column x is an ID
  if (#TODO) {
    return(FALSE)
  }
  category_count = length(unique(na.omit(df[x])))
  return(category_count == nrow(df[x]))
}

maybe_sample <- function(df, sample) {

}

#' Calculate the Predictive Power Score (PPS) for "x predicts y"
#'
#' The Predictive Power Score (PPS) always ranges form 0 to 1 and is data-type agnostic.
#' \itemize{
#'   \item A score of 0 means that the column x cannot predict the column y better than a naive baseline model.
#'   \item A score of 1 means that the column x can perfectly predict the column y given the model.
#'   \item A score between 0 and 1 states the ratio of how much potential predictive power
#'    the model achieved compared to the baseline model.
#' }
#' @param df `data.frame`. Dataframe that contains the columns x and y
#' @param x `str`. Name of the column x which acts as the feature
#' @param y `str`. Name of the column y which acts as the target
#' @param task `str`, default `NULL`. Name of the prediction task, e.g. `classification` or `regression`. \cr
#' If the task is not specified, it is infered based on the y column. \cr
#' The task determines which model and evaluate score is used for the PPS.
#' @param sample `int` or `NULL`. Number of rows for sampling. The sampling decreases the calculation time of the PPS. \cr
#' If `NULL`, there will be no sampling.
#' @return A list that contains multiple fields about the resulting PPS. \cr
#' The list enables introspection into the calculations that have been performed under the hood.
#' @export
score <- function(df, x, y, task = NULL, sample = 5000) {
  if (x == y) {
    task_name = "predict_itself"
  }
  else {
    df = na.omit(df[c(x,y)])
    if (nrow(df) == 0) {
      #TODO: raise exception
    }
    df = maybe_sample(df, sample)

    if (is.null(task)) {
      task_name = infer_task(df, x, y)
    }
    else {
      task_name = task
    }
  }

  #TODO: task = TASKS[task_name]

  if (task_name %in% c("predict_constant", "predict_itself")) {
    model_score = 1
    ppscore = 1
    baseline_score = 1
  }
  else if (task_name == "predict_id") {
    model_score = 0
    ppscore = 0
    baseline_score = 0
  }
  else if (feature_is_id(df, x)) {
    model_score = 0
    ppscore = 0
    baseline_score = 0
  }
  else {
    model_score = calculate_model_cv_score(
      #TODO: modify to R syntax later
      #df, target = y, feature = x, metric = task['mtric_key'], model = task['model']
    )
    #TODO
    #ppscore, baseline_score = task["score_normalizer"](df, y, model_score)
  }

  return(
    list(
      "x" = x,
      "y" = y,
      "task" = task_name,
      "ppscore" = ppscore,
      "metric" = task$metric_name,
      "baseline_score" = baseline_score,
      "model_score" = abs(model_score),
      "model" = task$model
    )
  )
}

#' Calculate the Predictive Power Score (PPS) natrix for all columns in the data frame
#'
#' @param df `data.frame`. The data frame that contains the data
#' @param output `str`. Pottential values: "df", "list". \cr
#' Control the type of the output. Either return a df or a dict with all the PPS dicts arranged by the target column
#' @export
matrix <- function(df, output = "df") {
  data = list()
  columns = colnames(df)

  for (target in columns) {
    scores =
    for (feature in columns) {
      #TODO: try, except single_score
      scores <- c(scores, single_score)
    }
    data[target] <- scores
  }

  if (output = "df") {
    #TODO
  }
  else {
    return(data)
  }
}



