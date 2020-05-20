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

ConfusionMatrix <- function(y_pred, y_true) {
  Confusion_Mat <- table(y_true, y_pred)
  return(Confusion_Mat)
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

ConfusionDF <- function(y_pred, y_true) {
  Confusion_DF <- transform(as.data.frame(ConfusionMatrix(y_pred, y_true)),
                            y_true = as.character(y_true),
                            y_pred = as.character(y_pred),
                            Freq = as.integer(Freq))
  return(Confusion_DF)
}

utils::globalVariables("Freq")

#' @title
#' Precision
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

Precision <- function(y_true, y_pred, positive = NULL, labels = NULL, average = "binary") {
  Confusion_DF <- ConfusionDF(y_pred, y_true)
  if (average == "binary") {
    if (is.null(positive) == TRUE) {
      positive <- as.character(Confusion_DF[1,1])
    }
    TP <- as.integer(Confusion_DF[which(Confusion_DF$y_true==positive & Confusion_DF$y_pred==positive),"Freq"])
    FP <- as.integer(sum(Confusion_DF[which(Confusion_DF$y_true!=positive & Confusion_DF$y_pred==positive), "Freq"]))
    Precision <- TP/(TP+FP)
  }
  else {
    if (is.null(labels) == TRUE) {
      labels <- unique(c(y_true, y_pred))
    }

    TP <- c()
    FP <- c()
    Prec <- c()
    for (i in c(1:length(labels))) {
      positive <- labels[i]

      tmp_TP <- Confusion_DF[which(Confusion_DF$y_true==positive & Confusion_DF$y_pred==positive), "Freq"]
      TP[i] <- if (length(tmp_TP)==0) 0 else as.integer(tmp_TP)

      tmp_FP <- Confusion_DF[which(Confusion_DF$y_true!=positive & Confusion_DF$y_pred==positive), "Freq"]
      FP[i] <- if (length(tmp_FP)==0) 0 else as.integer(sum(tmp_FP))

      Prec[i] <- TP[i] / (TP[i] + FP[i])
    }

    if (average == "micro") {
      Precision <- sum(TP) / (sum(TP) + sum(FP))
    }
    else if (average == "macro") {
      Precision <- mean(Prec)
    }
    else if (average == "weighted") {
      Precision <- weighted.mean(Prec, as.vector(table(y_true)[labels]))
    }
  }
  return(Precision)
}

#' @title Recall
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

Recall <- function(y_true, y_pred, positive = NULL, labels = NULL, average = "binary") {
  Confusion_DF <- ConfusionDF(y_pred, y_true)
  if (average == "binary") {
    if (is.null(positive) == TRUE) {
      positive <- as.character(Confusion_DF[1,1])
    }
    TP <- as.integer(Confusion_DF[which(Confusion_DF$y_true==positive & Confusion_DF$y_pred==positive),"Freq"])
    FN <- as.integer(sum(Confusion_DF[which(Confusion_DF$y_true==positive & Confusion_DF$y_pred!=positive), "Freq"]))
    Recall <- TP/(TP+FN)
  }
  else {
    if (is.null(labels) == TRUE) {
      labels <- unique(c(y_true, y_pred))
    }

    TP <- c()
    FN <- c()
    Rec <- c()
    for (i in c(1:length(labels))) {
      positive <- labels[i]

      tmp_TP <- Confusion_DF[which(Confusion_DF$y_true==positive & Confusion_DF$y_pred==positive), "Freq"]
      TP[i] <- if (length(tmp_TP)==0) 0 else as.integer(tmp_TP)

      tmp_FN <- Confusion_DF[which(Confusion_DF$y_true==positive & Confusion_DF$y_pred!=positive), "Freq"]
      FN[i] <- if (length(tmp_FN)==0) 0 else as.integer(sum(tmp_FN))

      Rec[i] <- TP[i] / (TP[i] + FP[i])
    }

    if (average == "micro") {
      Recall <- sum(TP) / (sum(TP) + sum(FN))
    }
    else if (average == "macro") {
      Recall <- mean(Rec)
    }
    else if (average == "weighted") {
      Recall <- weighted.mean(Rec, as.vector(table(y_true)[labels]))
    }
  }
  return(Recall)
}

#' @title F1 Score
#'
#' @description
#' Compute the F1 Score, also known as balanced F-score or F-measure.
#'
#' @details
#' The F1 score can be interpreted as a weighted average of the precision and recall,
#' where an F1 score reaches its best value at 1 and worst score at 0.
#' The relative contribution of precision and recall to the F1 score are equal.
#' The formula for the F1 score is: \cr
#' `F1_score = 2 * (precision * recall) / (precision + recall)` \cr
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

F1_Score <- function(y_true, y_pred, positive = NULL, labels = NULL, average = "binary") {
  if (average == "binary") {
    Confusion_DF <- ConfusionDF(y_pred, y_true)
    if (is.null(positive) == TRUE) positive <- as.character(Confusion_DF[1,1])
  }
  else {
    if (is.null(labels) == TRUE) labels <- unique(c(y_true, y_pred)) # possible problems if labels are missing from y_*
  }
  Precision <- Precision(y_true, y_pred, positive = positive, labels = labels, average = average)
  Recall <- Recall(y_true, y_pred, positive = positive, labels = labels, average = average)
  F1_score <- 2 * (Precision * Recall) / (Precision + Recall)
  return(F1_score)
}
