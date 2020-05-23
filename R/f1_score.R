##TODO: raise errors for functions when values of arguments of functions are wrongly set

#' @title
#' Confusion Matrix
#'
#' @description
#' Compute confusion matrix to evaluate the accuracy of a classification.
#'
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @param y_true Ground truth (correct) 0-1 labels vector
#' @return a table of Confusion Matrix

confusion_matrix <- function(y_pred, y_true) {
  confusion_matrix <- table(y_true, y_pred)
  return(confusion_matrix)
}

#' @title
#' Confusion DF
#'
#' @description
#' Compute data frame format confusion matrix for internal usage.
#'
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @param y_true Ground truth (correct) 0-1 labels vector
#' @return a data.frame of Confusion Matrix

confusion_df <- function(y_pred, y_true) {
  confusion_df <- transform(as.data.frame(confusion_matrix(y_pred, y_true)),
                            y_true = as.character(y_true),
                            y_pred = as.character(y_pred),
                            Freq = as.integer(Freq))
  return(confusion_df)
}

utils::globalVariables("Freq")

#' @title
#' precision
#'
#' @description
#' Compute the precision score (of multi-class problems) using different types of averaging. \cr
#' For more information, please visit the following site:
#' \url{https://sebastianraschka.com/faq/docs/multiclass-metric.html}
#'
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @param y_true Ground truth (correct) labels vector (0-1 in the case when `average = "binary"``)
#' @param positive An optional character string for the factor level that
#'   corresponds to a "positive" result (only applies in the case when `average = "binary"`)
#' @param labels An optional vector containing the list of the existent (unique) labels.
#' @param average `string`, ["binary" (default), "micro", "macro", "weighted]
#' This parameter is required for multiclass/multilabel targets, and determines
#' the type of averaging performed on the data.
#' @return The precision score

precision <- function(y_true, y_pred, positive = NULL, labels = NULL, average = "binary") {
  confusion_df <- ConfusionDF(y_pred, y_true)
  if (average == "binary") {
    if (is.null(positive) == TRUE) {
      positive <- as.character(confusion_df[1,1])
    }
    true_positive <- as.integer(confusion_df[which(confusion_df$y_true==positive & confusion_df$y_pred==positive),"Freq"])
    false_positive <- as.integer(sum(confusion_df[which(confusion_df$y_true!=positive & confusion_df$y_pred==positive), "Freq"]))
    precision <- true_positive/(true_positive+false_positive)
  }
  else {
    if (is.null(labels) == TRUE) {
      labels <- unique(c(y_true, y_pred))
    }

    true_positive <- c()
    false_positive <- c()
    prec <- c()
    for (i in c(1:length(labels))) {
      positive <- labels[i]

      tmp_true_positive <- confusion_df[which(confusion_df$y_true==positive & confusion_df$y_pred==positive), "Freq"]
      true_positive[i] <- if (length(tmp_true_positive)==0) 0 else as.integer(tmp_true_positive)

      tmp_false_positive <- confusion_df[which(confusion_df$y_true!=positive & confusion_df$y_pred==positive), "Freq"]
      false_positive[i] <- if (length(tmp_false_positive)==0) 0 else as.integer(sum(tmp_false_positive))

      prec[i] <- true_positive[i] / (true_positive[i] + false_positive[i])
    }

    if (average == "micro") {
      precision <- sum(true_positive) / (sum(true_positive) + sum(false_positive))
    }
    else if (average == "macro") {
      precision <- mean(prec)
    }
    else if (average == "weighted") {
      precision <- weighted.mean(prec, as.vector(table(y_true)[labels]))
    }
  }
  return(precision)
}

#' @title recall
#'
#' @description
#' Compute the recall score (of multi-class problems) using different types of averaging. \cr
#' For more information, please visit the following site:
#' \url{https://sebastianraschka.com/faq/docs/multiclass-metric.html}
#'
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @param y_true Ground truth (correct) labels vector (0-1 in the case when `average = "binary"``)
#' @param positive `string`. An optional character string for the factor level that
#'   corresponds to a "positive" result (only applies in the case when `average = "binary"`)
#' @param labels An optional vector containing the list of the existent (unique) labels.
#' @param average `string`, ["binary" (default), "micro", "macro", "weighted].
#' This parameter is required for multiclass/multilabel targets, and determines
#' the type of averaging performed on the data.
#' @return The recall score

recall <- function(y_true, y_pred, positive = NULL, labels = NULL, average = "binary") {
  confusion_df <- ConfusionDF(y_pred, y_true)
  if (average == "binary") {
    if (is.null(positive) == TRUE) {
      positive <- as.character(confusion_df[1,1])
    }
    true_positive <- as.integer(confusion_df[which(confusion_df$y_true==positive & confusion_df$y_pred==positive),"Freq"])
    false_negative <- as.integer(sum(confusion_df[which(confusion_df$y_true==positive & confusion_df$y_pred!=positive), "Freq"]))
    recall <- true_positive/(true_positive+false_negative)
  }
  else {
    if (is.null(labels) == TRUE) {
      labels <- unique(c(y_true, y_pred))
    }

    true_positive <- c()
    false_negative <- c()
    rec <- c()
    for (i in c(1:length(labels))) {
      positive <- labels[i]

      tmp_true_positive <- confusion_df[which(confusion_df$y_true==positive & confusion_df$y_pred==positive), "Freq"]
      true_positive[i] <- if (length(tmp_true_positive)==0) 0 else as.integer(tmp_true_positive)

      tmp_false_negative <- confusion_df[which(confusion_df$y_true==positive & confusion_df$y_pred!=positive), "Freq"]
      false_negative[i] <- if (length(tmp_false_negative)==0) 0 else as.integer(sum(tmp_false_negative))

      rec[i] <- true_positive[i] / (true_positive[i] + false_positive[i])
    }

    if (average == "micro") {
      recall <- sum(true_positive) / (sum(true_positive) + sum(false_negative))
    }
    else if (average == "macro") {
      recall <- mean(rec)
    }
    else if (average == "weighted") {
      recall <- weighted.mean(rec, as.vector(table(y_true)[labels]))
    }
  }
  return(recall)
}

#' @title
#' F1 Score
#'
#' @description
#' Compute the F1 Score, also known as balanced F-score or F-measure.
#'
#' @details
#' The F1 score can be interpreted as a weighted average of the precision and recall,
#' where an F1 score reaches its best value at 1 and worst score at 0.
#' The relative contribution of precision and recall to the F1 score are equal.
#' The formula for the F1 score is: \cr
#' `f1_score = 2 * (precision * recall) / (precision + recall)` \cr
#' In the multi-class and multi-label case, this is the average of the F1 score of each class
#' with weighting depending on the `average` parameter.
#'
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @param y_true Ground truth (correct) labels vector (0-1 in the case when `average = "binary"``)
#' @param positive An optional character string for the factor level that
#'   corresponds to a "positive" result (only applies in the case when `average = "binary"`)
#' @param labels An optional vector containing the list of the existent (unique) labels.
#' @param average `string`, ["binary" (default), "micro", "macro", "weighted]
#' This parameter is required for multiclass/multilabel targets, and determines
#' the type of averaging performed on the data:
#' \itemize{
#'   \item `binary`: This is applicable only if targets `y_pred` and `y_true` are binary.
#'   \item `micro`: Calculate metrics globally by counting the total true positives, false negatives and false positives.
#'   \item `macro`: Calculate metrics for each label, and find their unweighted mean. This does not take label imbalance into account.
#'   \item `weighted`: Calculate metrics for each label, and find their average weighted by support (the number of true instances for each label).
#'   This alters ‘macro’ to account for label imbalance; it can result in an F-score that is not between precision and recall.
#' }
#' @return F1 score of the positive class in binary classification or weighted average of the F1 scores
#' of each class for the multiclass task.
#' @export

f1_score <- function(y_true, y_pred, positive = NULL, labels = NULL, average = "binary") {
  if (average == "binary") {
    confusion_df <- ConfusionDF(y_pred, y_true)
    if (is.null(positive) == TRUE) positive <- as.character(confusion_df[1,1])
  }
  else {
    if (is.null(labels) == TRUE) labels <- unique(c(y_true, y_pred)) # possible problems if labels are missing from y_*
  }
  precision <- precision(y_true, y_pred, positive = positive, labels = labels, average = average)
  recall <- recall(y_true, y_pred, positive = positive, labels = labels, average = average)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  return(f1_score)
}
