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

#' @export
score <- function(df, x, y, task = NULL, sample = 5000) {

}

#' @export
matrix <- function(df, output = "df") {

}



