#' Plots the Receiver Operating Characteristics Curve (ROC)
#'
#' A function that computes the area under the ROC curve (AUC) from model
#' predictions and plots the ROC.
#'
#' @param pred A vector, list, matrix, or data frame containing the predictions
#' from logistic regression.
#' @param truth A vector, list, matrix, or data frame containing the true class
#' labels. The dimensions must be the same as predictions.
#' @param ... Arguments to be passed to methods, such as graphical parameters.
#'
#' @return Returns a plot of an ROC curve with its AUC value.
#'
#' @examples
#' # Using rbps data set available with package
#' rbps_results <- trainCV(rbps, col_index = 10)
#' plotROC(rbps_results$predictions[[1]], rbps_results$test_sets[[1]]$autism_genes)
#'
#' @references
#' Zach. (2021, September 9). \emph{How to calculate AUC (area under curve) in
#' R.} Statology. \href{https://www.statology.org/auc-in-r/}{Link}.
#'
#' Bradley, A. P. (1997). The use of the area under the ROC curve in the
#' evaluation of machine learning algorithms. \emph{Pattern Recognition}, 30(7),
#' 1145–1159. \href{https://doi.org/10.1016/s0031-3203(96)00142-2}{Link}.
#'
#' @export
#' @import ROCR
#' @import graphics
#' @importFrom pROC auc
plotROC <- function(pred, truth, ...) {
  predobj <- prediction(pred, truth)
  perf <- performance(predobj, "tpr", "fpr")
  plot(perf, xlim = c(0.0, 1.0), ylim = c(0.0, 1.0), ...)
  area <- auc(truth, pred)
  area <- format(round(area, 4), nsmall = 4)
  text(x = 0.8, y = 0.3, labels = paste("AUC =", area))

  # Plot the reference x=y line
  segments(x0 = 0, y0 = 0, x1 = 1, y1 = 1, col = "gray", lty = 2)
}
