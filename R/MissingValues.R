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
#'   \item naResults - The number of NA values in each column.
#'   \item uniqueResults - The number of unique values in each column.
#' }
#'
#' @examples
#' # Using rbps data set available with package
#' data(rbps)
#' # Check for missing values in the rbps data set
#' missingValuesResults <- missingValues(data = rbps)
#' missingValuesResults$naResults
#'
#' @references
#' Honaker, J., King, G., & Blackwell, M. (2011). Amelia II: A Program for
#' Missing Data. \emph{Journal of Statistical Software}, 45(7), 1–47.
#' \href{https://doi.org/10.18637/jss.v045.i07}{Link}.
#' @export
#' @importFrom Amelia missmap
missingValues <- function(data) {
  # Performing checks of user input
  if (is.data.frame(data) == FALSE) {
    stop("data should be a data frame.")
  }

  # Checking for NA values in each column
  naValues <- sapply(data, function(x) sum(is.na(x)))

  # Checking for unique values in each column
  uniqueValues <- sapply(data, function(x) length(unique(x)))

  # A visual that highlights the missing values
  Amelia::missmap(data, main = "Missing values vs observed", margins = c(10, 4))

  results <- list(naResults = naValues, uniqueResults = uniqueValues)
  class(results) <- "MissingValues"

  return(results)
}


# [END]
