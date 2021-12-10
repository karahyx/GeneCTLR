#' Pre-process the data set to be used for logistic regression modelling
#'
#' A function that pre-processes the data for use in building the logistic
#' regression model.
#'
#' @param data A data frame to be pre-processed.
#' @param dependentVarIndex A positive integer indicating the index of the
#' dependent variable column in the data set.
#' @param deleteColumns A numeric vector containing the columns to be deleted.
#' These are user-specified indices of the columns that will not be used for
#' building the logistic regression model with the tranCV function. An
#' optional parameter.
#'
#' @return Returns the pre-processed data frame.
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
#' head(newDat)
#'
#' @export
preProcessData <- function(data, dependentVarIndex, deleteColumns = NULL) {
  # Performing checks of user input
  if (is.data.frame(data) == FALSE) {
    stop("data should be a data frame.")
  }

  if (is.numeric(dependentVarIndex) == FALSE) {
    stop("dependentVarIndex should be a positive interger.")
  }

  if (is.numeric(dependentVarIndex) == TRUE && dependentVarIndex < 1) {
    stop("dependentVarIndex should be a positive interger.")
  }


  if (missing(deleteColumns) == FALSE) {
    if (is.vector(deleteColumns) == FALSE) {
      stop("deleteColumns should be a numeric vector.")
    }
  }

  # Changing the class of the dependent variable column to factor to be used
  # in the glm function
  data[[dependentVarIndex]] <- as.factor(data[[dependentVarIndex]])

  data[ , -dependentVarIndex] <- apply(data[ , -dependentVarIndex],
                                       2,
                                       function(x) as.numeric(x))

  # Deleting user-specified columns
  if (missing(deleteColumns) == FALSE) {
    newData <- data[-deleteColumns]
  }
  else {
    newData <- data
  }

  return(newData)
}
