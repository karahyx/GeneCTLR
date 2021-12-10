#' Plots the Receiver Operating Characteristics (ROC) Curve
#'
#' A function that computes the area under the ROC curve (AUC) from model
#' predictions and plots the ROC curve.
#'
#' @param results an S3 object of class trainCV with the following elements:
#' \itemize{
#'   \item models - Fitted logistic regression models.
#'   \item predictions - The probabilities predicted by each model.
#'   \item testSets - Test data sets used in each fold of the cross validation.}
#' @param dependentVarIndex A positive integer indicating the index of the
#' dependent variable column in the data set.
#' @param title A string of characters denoting the title of the plot. An
#' optional parameter. The default value, "ROC Curve", is used if there isn't
#' a user-specified value for title.
#'
#' @return Returns an ROC curve with AUC values showing the possibility that
#' the model ranks a random positive event more highly than a random negative
#' event.
#'
#' @examples
#' # Using rbps data set available with package
#' data(rbps)
#' imputedRBPs <- impute(data = rbps, replace = "mean")
#'
#' newDat <- preProcessData(data = imputedRBPs,
#'                          dependentVarIndex = 12,
#'                          deleteColumns = c(1, 8))
#'
#' # Using default K = 5
#' results <- trainCV(data = newDat, dependentVarIndex = 10)
#'
#' plotROC(results = results,
#'         dependentVarIndex = 10,
#'         title = "ROC Curves for Autism Gene Prediction")
#'
#' @references
#' Bradley, A. P. (1997). The use of the area under the ROC curve in the
#' evaluation of machine learning algorithms. \emph{Pattern Recognition}, 30(7),
#' 1145–1159. \href{https://doi.org/10.1016/s0031-3203(96)00142-2}{Link}.
#'
#' Google. (n.d.). \emph{Classification: Roc curve and AUC; machine learning crash
#' course; google developers.} Google.
#' \href{https://developers.google.com/machine-learning/crash-course/classification/roc-and-auc.}{Link}.
#'
#' Zach. (2021, September 9). \emph{How to calculate AUC (area under curve) in
#' R.} Statology. \href{https://www.statology.org/auc-in-r/}{Link}.
#'
#'
#' @export
#' @importFrom ROCR prediction performance
#' @importFrom pROC auc
#' @importFrom cowplot theme_cowplot
#' @importFrom ggsci scale_color_lancet
#' @import ggplot2
plotROC <- function(results, dependentVarIndex, title) {

  # Performing checks of user input
  if (class(results) != "trainCV") {
    stop("results should be an S3 object of class trainCV.")
  }

  if (is.numeric(dependentVarIndex) == FALSE) {
    stop("dependentVarIndex should be a positive interger.")
  }

  if (is.numeric(dependentVarIndex) == TRUE && dependentVarIndex < 1) {
    stop("dependentVarIndex should be a positive interger.")
  }

  if (missing(title) == FALSE) {
    if (is.character(title) == FALSE) {
      stop("title should be a string of characters.")
    }
  }

  # Initializing variables
  K <- length(results$models)
  allpf <- data.frame(fpr = numeric(),
                      tpr = numeric(),
                      fold = numeric())
  aucs <- numeric()

  # Adding predictions and auc values from each model to allpf
  for (i in 1:K) {
    pred <- ROCR::prediction(results$predictions[[i]],
                             results$testSets[[i]][[dependentVarIndex]])
    perf <- ROCR::performance(pred, "tpr", "fpr")

    area <- pROC::auc(results$testSets[[i]][[dependentVarIndex]],
                      results$predictions[[i]])
    aucs <- append(aucs, area)

    pf <- data.frame(fpr = perf@x.values,
                     tpr = perf@y.values,
                     fold = i)
    names(pf) <- c("fpr", "tpr", "fold")

    allpf <- rbind(allpf, pf)
  }

  # Formatting the legend labels
  legendLabels <- character()
  for (i in 1:K) {
    content <- paste("Model ", as.character(i), " (AUC = ",
                     as.character(aucs[i]), ")", sep = "")
    legendLabels <- append(legendLabels, content)
  }

  # Generating the ROC curves
  rocCurve <-
    ggplot2::ggplot(data = allpf, aes(x = fpr, y = tpr, colour = as.factor(fold))) +
    ggplot2::geom_line() +
    cowplot::theme_cowplot() +
    ggplot2::theme(axis.title = element_text(size = 16, face = "bold"),
                   axis.text.x = element_text(size = 12, face = "bold"),
                   axis.text.y = element_text(size = 12, face = "bold"),
                   axis.line = element_line(size = 1),
                   legend.text = element_text(size = 11, face = "bold"),
                   legend.title = element_blank(),
                   legend.position = c(0.6, 0.4),
                   legend.box.background = element_blank(),
                   legend.key.size = unit(1.0, "cm"),
                   plot.title = element_text(size = 30, hjust = 0.5)) +
    ggsci::scale_color_lancet(labels = legendLabels) +
    ggplot2::labs(x = "False positive rate", y = "True positive rate") +
    ggplot2::ggtitle(label = "ROC Curves")

  return(rocCurve)
}


# [END]



