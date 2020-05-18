#' @title titanic_train
#' @description This data set provides information on the fate of passengers on
#' the fatal maiden voyage of the ocean liner `Titanic`, summarized according
#' to economic status (class), sex, age and survival. Whereas the base R
#' Titanic data found by calling data(`Titanic`) is an array resulting
#' from cross-tabulating 2201 observations, this dataset is the individual
#' non-aggregated observations and formatted in a machine learning context
#' as a training sample. This dataset is downloaded from the Kaggle competition and thus
#' lowers the barrier to entry for users new to `R`` or machine learing.
#' @format A data frame with 891 rows and 12 variables:
#' \describe{
#'   \item{\code{PassengerId}}{integer Passenger ID}
#'   \item{\code{Survived}}{integer Passenger survival indicator}
#'   \item{\code{Pclass}}{integer Passenger Class}
#'   \item{\code{Name}}{character Name}
#'   \item{\code{Sex}}{integer Sex}
#'   \item{\code{Age}}{double Age}
#'   \item{\code{SibSp}}{integer Number of Siblings/Spouses aboard}
#'   \item{\code{Parch}}{integer Number of Parents/Children aboard}
#'   \item{\code{Ticket}}{character Ticket number}
#'   \item{\code{Fare}}{double Passenger Fare}
#'   \item{\code{Cabin}}{Factor Cabin}
#'   \item{\code{Embarked}}{Factor Port of Embarkation}
#'}
#' @source Package `titanic`: Titanic Passenger Survival Data Set. \cr
#' \url{http://cran.r-project.org/web/packages/titanic/}
"titanic_train"

#' @rdname titanic_train
