#' Checking for missing values in the data set
#'
#' A function that checks the number of NA values in each column and produces
#' a visual output for the missing values in the data set. The most missing
#' variable is farthest to the left in the visual output.
#'
#' @param data A data frame to be analyzed.
#'
#' @return Returns an object of class MissingValues with results.
#' \itemize{
#'   \item NA_results - The number of NA values in each column.
#'   \item unique_results - The number of unique values in each column.
#' }
#'
#' @examples
#' # Using rbps data set available with package
#'
#' # Check for missing values in the rbps data set
#' MissingValuesResults <- MissingValues(data = rbps)
#' MissingValuesResults$NA_results
#'
#' @references
#' Honaker, J., King, G., & Blackwell, M. (2011). Amelia II: A Program for
#' Missing Data. \emph{Journal of Statistical Software}, 45(7), 1–47.
#' \href{https://doi.org/10.18637/jss.v045.i07}{Link}.
#' @export
#' @importFrom Amelia missmap
MissingValues <- function(data) {
  # Performing checks of user input
  if (is.data.frame(data) == FALSE) {
    stop("data should be a data frame.")
  }

  # Checking for NA values in each column
  NA_values <- sapply(data, function(x) sum(is.na(x)))

  # Checking for unique values in each column
  unique_values <- sapply(data, function(x) length(unique(x)))

  # A visual that highlights the missing values
  Amelia::missmap(data, main = "Missing values vs observed", margins = c(10, 4),
          x.cex = 0.3)

  results <- list(NA_results = NA_values,
                  unique_results = unique_values)
  class(results) <- "MissingValues"
  return(results)
}
