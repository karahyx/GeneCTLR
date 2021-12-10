#' Plots the Precision-Recall (PR) Curve
#'
#' A function that computes the area under the precision-recall (PR) curve from
#' model predictions and plots the precision-recall curve. PR curves are
#' particularly useful for imbalanced data sets where we observe one class more
#' frequently than the other class.
#'
#' @param results an S3 object of class trainCV with the following elements:
#' \itemize{
#'   \item models - Fitted logistic regression models.
#'   \item predictions - The probabilities predicted by each model.
#'   \item testSets - Test data sets used in each fold of the cross validation.}
#' @param dependentVarIndex A positive integer indicating the index of the
#' dependent variable column in the data set.
#'
#' @return Returns a precision-recall curve with AUCPR values demonstrating
#' the performance of the algorithm
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
#' plotPR(results = results,
#'        dependentVarIndex = 10)
#'
#' @references
#' Boyd, K., Eng, K. H., &amp; Page, C. D. (2013). Area under the
#' precision-recall curve: Point estimates and confidence intervals.
#' \emph{Advanced Information Systems Engineering}, 451–466.
#' \href{https://doi.org/10.1007/978-3-642-40994-3_29}{Link}
#'
#' Fu, G. H., Yi, L. Z., & Pan, J. (2018). Tuning model parameters in
#' class‐imbalanced learning with precision‐recall curve. \emph{Biometrical
#' Journal}, 61(3), 652–664. \href{https://doi.org/10.1002/bimj.201800148}{Link}
#'
#' Williams, C. K. (2021). The effect of class imbalance on precision-recall
#' curves. \emph{Neural Computation}, 33(4), 853–857.
#' \href{https://doi.org/10.1162/neco_a_01362}{Link}.
#'
#' @export
#' @importFrom ROCR prediction performance
#' @importFrom cowplot theme_cowplot
#' @importFrom ggsci scale_color_lancet
#' @import ggplot2
plotPR <- function(results, dependentVarIndex) {
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


  # Initializing variables
  K <- length(results$models)
  allpf <- data.frame(fpr = numeric(),
                      tpr = numeric(),
                      fold = numeric())
  aucprs <- numeric()

  # Adding predictions and aucpr values from each model to allpf
  for (i in 1:K) {
    pred <- ROCR::prediction(results$predictions[[i]],
                             results$testSets[[i]][[dependentVarIndex]])
    perf <- ROCR::performance(pred, "prec", "rec")

    aucprObj <- ROCR::performance(pred, "aucpr")
    aucpr <- aucprObj@y.values[[1]]
    aucprs <- append(aucprs, aucpr)

    pf <- data.frame(rec = perf@x.values,
                     prec = perf@y.values,
                     fold = i)
    data.frame(rec = perf@x.values, prec = perf@y.values)
    names(pf) <- c("rec", "prec", "fold")

    allpf <- rbind(allpf, pf)
  }

  # Formatting the legend labels
  legendLabels <- character()
  for (i in 1:K) {
    content <- paste("Model ", as.character(i), " (AUCPR = ",
                     as.character(aucprs[i]), ")", sep = "")
    legendLabels <- append(legendLabels, content)
  }

  # Generating the precision-recall curves
  prCurve <-
    ggplot2::ggplot(data = allpf, aes(x = rec, y = prec, colour = as.factor(fold))) +
    ggplot2::geom_line() +
    cowplot::theme_cowplot() +
    ggplot2::theme(axis.title = element_text(size = 16, face = "bold"),
                   axis.text.x = element_text(size = 12, face = "bold"),
                   axis.text.y = element_text(size = 12, face = "bold"),
                   axis.line = element_line(size = 1),
                   legend.text = element_text(size = 11, face = "bold"),
                   legend.title = element_blank(),
                   legend.position = c(0.5, 0.5),
                   legend.box.background = element_blank(),
                   legend.key.size = unit(1.0, "cm"),
                   plot.title = element_text(size = 30, hjust = 0.5)) +
    ggsci::scale_color_lancet(labels = legendLabels) +
    ggplot2::labs(x = "Recall", y = "Precision") +
    ggplot2::ggtitle(label = "Precision-Recall Curve")

  return(prCurve)
}

# [END]
