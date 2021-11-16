#' Performs logistic model training and cross validation
#'
#' A function that trains the logistic regression model and performs repeated
#' K-fold cross validation.
#'
#' @param data A data frame to be trained. The data frame should only contain
#' one dependent variable and one or more independent variables. All independent
#' variables to be used in training the model should be of type numeric.
#' @param col_index A positive integer indicating the index of the dependent
#' variable column in the data set.
#' @param K A positive integer indicating the number of groups that a given
#' data set is to be split into.
#' @param repetition A positive integer indicating the number of repetitions for
#' the K-fold cross validation. Must be at least 2.
#'
#' @return Returns an S3 object of class trainCV with the following elements:
#' \itemize{
#'   \item models - Fitted logistic regression models.
#'   \item predictions - The probabilities predicted by each model.
#'   \item test_sets - Test data sets used in each fold of the cross validation.
#' }
#'
#' @examples
#' # Changing the class of hasCanonicalRBDs (an independent variable) to
#' # numeric to be used for the trainCV function
#' rbps$hasCanonicalRBDs <- as.numeric(rbps$hasCanonicalRBDs)
#'
#' # Remove the Human Gene and pLI columns
#' rbps <- subset(rbps, select = -c(1, 8))
#'
#' rbps_results <- trainCV(data = rbps, col_index = 10, K = 5, repetition = 3)
#'
#' # Show the first fitted model
#' rbps_results$models[[1]]
#'
#' @references
#' Alice, M. (2020, July 5). \emph{How to perform a logistic regression in R:
#' R-bloggers.} R. \href{https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/}{Link}.
#'
#' Selva, P. (n.d.). \emph{Logistic Regression with R.} r-statistics.co.
#' \href{http://r-statistics.co/Logistic-Regression-With-R.html}{Link}.
#'
#' @export
#' @import ROCR
plotAUC <- function(pred, truth, ...) {
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  plot(perf, ...)
  area <- auc(truth, pred)
  area <- format(round(area, 4), nsmall = 4)
  text(x = 0.8, y = 0.1, labels = paste("AUC =", area))

  # Plot the reference x=y line
  segments(x0 = 0, y0 = 0, x1 = 1, y1 = 1, col = "gray", lty = 2)
}
