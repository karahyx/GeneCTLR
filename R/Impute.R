#' Imputation of missing data
#'
#' A function that replaces the missing values in a column with values imputed
#' from the observed data (i.e. the mean, median, or mode for that variable).
#'
#' @param data A data frame to be imputed.
#' @param replace A value used to replace the missing values in a column.
#' Should be either the mean or the median.
#'
#' @return Returns the modified data frame data.
#'
#' @details Only variables of class numeric that contain NA values will be
#' imputed.
#'
#' @examples
#' # Using rbps data set available with package
#' data(rbps)
#' # Replace the missing values in rbps with the mean
#' imputedRBPs <- impute(rbps, "mean")
#' head(imputedRBPs)
#'
#' @references
#' Little, JA. R. & Rubin B. D. (1987). \emph{Statistical analysis of missing
#' data}. John Wiley & Sons.
#'
#' @export
#' @importFrom stats median
impute <- function(data, replace) {

  # Performing checks of user input
  if (is.data.frame(data) == FALSE) {
    stop("data should be a data frame.")
  }

  if (replace != "mean" & replace != "median") {
    stop("replace should be either mean or median.")
  }

  newData <- data
  # Replacing missing values with the mean
  if (replace == "mean") {
    for (i in 1:ncol(newData)) {
      if (is.numeric(newData[[i]])) {
        newData[[i]][is.na(newData[[i]])] <- mean(newData[[i]], na.rm = TRUE)
      }
    }
  }
  # Replacing missing values with the median
  else if (replace == "median") {
    for (i in 1:ncol(newData)) {
      if (is.numeric(newData[[i]])) {
        newData[[i]][is.na(newData[[i]])] <- stats::median(newData[[i]], na.rm = TRUE)
      }
    }
  }
  return(newData)
}


# [END]
