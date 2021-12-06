#' Add model predictions to the original data set
#'
#' A function that adds the predictions obtained from the trainCV function to
#' data.
#'
#' @param results An object of class trainCV that contains the following
#' elements: models, predictions, and testSets.
#' @param data A data frame.
#'
#' @return Returns a data frame with predictions added as the last column.
#'
#' @examples
#' # Using rbps data set available with package
#' imputedRBPs <- impute(rbps, "mean")
#'
#' # Changing the class of hasCanonicalRBDs (an independent variable) to
#' # numeric to be used for the trainCV function
#' imputedRBPs$hasCanonicalRBDs <- as.numeric(imputedRBPs$hasCanonicalRBDs)
#'
#' # Remove the Human Gene and pLI columns
#' imputedRBPs <- subset(imputedRBPs, select = -c(1, 8))
#'
#' results <- trainCV(data = imputedRBPs, colIndex = 10)
#' newRBPs <- addPredictions(results, rbps)
#' head(newRBPs)
#'
#' @export
addPredictions <- function(results, data) {
  # Performing checks of user input
  if (class(results) != "trainCV") {
    stop("results should be an object of class trainCV.")
  }

  if (is.data.frame(data) == FALSE) {
    stop("data should be a data frame.")
  }

  prValue <- sort(unlist(results$predictions))
  predictions <- prValue[match(rownames(data), names(prValue))]
  newData <- cbind(data, predictions)

  return(newData)
}
