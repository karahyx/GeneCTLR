#' Plots the Precision-Recall Curve (PRC)
#'
#' A function that computes the area under the precision-recall curve (PRC) from
#' model predictions and plots the precision-recall curve.
#'
#' @param pred A vector, list, matrix, or data frame containing the predictions
#' from logistic regression.
#' @param truth A vector, list, matrix, or data frame containing the true class
#' labels. The dimensions must be the same as predictions.
#' @param ... Arguments to be passed to methods, such as graphical parameters.
#'
#' @return Returns a plot of a precision-recall curve with its AUC value.
#'
#' @examples
#' # Using rbps data set available with package
#' data(rbps)
#' imputedRBPs <- impute(rbps, "mean")
#' imputedRBPs$hasCanonicalRBDs <- as.numeric(imputedRBPs$hasCanonicalRBDs)
#'
#' # Remove the Human Gene and pLI columns
#' rbps <- subset(imputedRBPs, select = -c(1, 8))
#'
#' results <- trainCV(data = rbps, colIndex = 10)
#' plotPR(results$predictions[[3]], results$testSets[[3]]$autism_genes)
#'
#' @references
#' Fu, G. H., Yi, L. Z., & Pan, J. (2018). Tuning model parameters in
#' class‐imbalanced learning with precision‐recall curve. \emph{Biometrical
#' Journal}, 61(3), 652–664. \href{https://doi.org/10.1002/bimj.201800148}{Link}
#'
#' Williams, C. K. (2021). The effect of class imbalance on precision-recall
#' curves. \emph{Neural Computation}, 33(4), 853–857.
#' \href{https://doi.org/10.1162/neco_a_01362}{Link}.
#'
#' @export
#' @importFrom ROCR prediction
#' @importFrom ROCR performance
#' @importFrom graphics plot
#' @importFrom graphics text
#' @importFrom graphics segments
plotPR <- function(pred, truth, ...) {
  if (is.vector(pred) == FALSE & is.list(pred) == FALSE &
      is.matrix(pred) == FALSE & is.data.frame(pred) == FALSE) {
    stop("pred should be a vector, list, matrix or data frame.")
  }

  if (is.vector(truth) == FALSE & is.list(truth) == FALSE &
      is.matrix(truth) == FALSE & is.data.frame(truth) == FALSE) {
    stop("truth should be a vector, list, matrix or data frame.")
  }

  predobj <- ROCR::prediction(pred, truth)
  perf <- ROCR::performance(predobj, "prec", "rec")
  # https://stackoverflow.com/questions/11467855/roc-curve-in-r-using-rocr-package
  graphics::plot(perf, xlim = c(0.0, 1.0), ylim = c(0.0, 1.0), ...)
  aucpr <- ROCR::performance(predobj, "aucpr")
  area <- aucpr@y.values[[1]]
  area <- format(round(area, 4), nsmall = 4)
  graphics::text(x = 0.8, y = 0.3, labels = paste("AUPRC =", area))

  # the reference x=y line
  graphics::segments(x0 = 0, y0 = 0, x1 = 1, y1 = 1, col = "gray", lty = 2)
}
