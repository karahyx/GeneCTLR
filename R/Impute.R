#' Imputation of missing data
#'
#' A function that replaces the missing values in a column with values imputed
#' from the observed data (i.e. the mean, median, or mode for that variable).
#'
#' @param data A data frame to be imputated.
#' @param replace A value used to replace the missing values in a column.
#' Should be one of the mean, median, or mode.
#'
#' @return Returns the modified data frame data.
#'
#' @examples
#' # Using rbps data set available with package
#' # Replace the missing values in rbps with the mean
#' imputated_rbps <- impute(rbps, "mean")
#' head(imputated_rbps)
#'
#' @references
#' Little, JA. R. & Rubin B. D. (1987). \emph{Statistical analysis of missing
#' data}. John Wiley & Sons.
#'
#' @export
impute <- function(data, replace) {
  # Performing checks of user input
  if (is.data.frame(data) == FALSE) {
    stop("data should be a data frame.")
  }

  if (replace != "mean" & replace != "median" & replace != "mode") {
    stop("replace should be one of mean, median, or mode.")
  }

  new_data <- data
  # Replacing missing values with the mean
  if (replace == "mean") {
    for (i in 1:ncol(new_data)) {
      if (is.numeric(new_data[[i]])) {
        new_data[[i]][is.na(new_data[[i]])] <- mean(new_data[[i]], na.rm = TRUE)
      }
    }
  }
  # Replacing missing values with the median
  else if (replace == "median") {
    for (i in 1:ncol(new_data)) {
      if (is.numeric(new_data[[i]])) {
        new_data[[i]][is.na(new_data[[i]])] <- mean(new_data[[i]], na.rm = TRUE)
      }
    }
  }
  # Replacing missing values with the mode
  else if (replace == "mode") {
    for (i in 1:ncol(new_data)) {
      if (is.numeric(new_data[[i]])) {
        new_data[[i]][is.na(new_data[[i]])] <- mean(new_data[[i]], na.rm = TRUE)
      }
    }
  }
  return(new_data)
}
