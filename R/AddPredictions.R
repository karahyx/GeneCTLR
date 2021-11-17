#' Add model predictions to the original data set
#'
#' A function that adds the predictions obtained from the trainCV function to
#' data.
#'
#' @param results An object of class trainCV that contains the following
#' elements: models, predictions, and test_sets.
#' @param data A data frame.
#'
#' @return Returns a data frame with predictions added as the last column.
#'
#' @examples
#' # Using rbps data set available with package
#' imputated_rbps <- impute(rbps, "mean")
#'
#' # Changing the class of hasCanonicalRBDs (an independent variable) to
#' # numeric to be used for the trainCV function
#' imputated_rbps$hasCanonicalRBDs <- as.numeric(imputated_rbps$hasCanonicalRBDs)
#'
#' # Remove the Human Gene and pLI columns
#' imputated_rbps <- subset(imputated_rbps, select = -c(1, 8))
#'
#' rbps_results <- trainCV(data = imputated_rbps, col_index = 10)
#' new_rbps <- AddPredictions(rbps_results, rbps)
#' head(new_rbps)
#'
#' @export
AddPredictions <- function(results, data) {
  # Performing checks of user input
  if (class(results) != "trainCV") {
    stop("results should be an object of class trainCV.")
  }

  if (is.data.frame(data) == FALSE) {
    stop("data should be a data frame.")
  }
  pr_val <- sort(unlist(results$predictions))
  predictions <- pr_val[match(rownames(data), names(pr_val))]
  new_data <- cbind(data, predictions)
  return(new_data)
}
