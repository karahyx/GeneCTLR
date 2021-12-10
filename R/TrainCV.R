#' Performs logistic model training and cross validation
#'
#' A function that trains the logistic regression model and performs a K-fold
#' cross validation.
#'
#' @param data A data frame to be trained. The data frame should only contain
#' one dependent variable and one or more independent variables. All independent
#' variables to be used in training the model should be of type numeric.
#' @param dependentVarIndex A positive integer indicating the index of the
#' dependent variable column in the data set.
#' @param K A positive integer indicating the number of groups that a given
#' data set is to be split into. Should be great than or equal to 2. Default
#' value is K = 5.
#'
#' @return Returns an S3 object of class trainCV with the following elements:
#' \itemize{
#'   \item models - Fitted logistic regression models.
#'   \item predictions - The probabilities predicted by each model.
#'   \item testSets - Test data sets used in each fold of the cross validation.
#' }
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
#' @references
#' Alice, M. (2020, July 5). \emph{How to perform a logistic regression in R:
#' R-bloggers.} R. \href{https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/}{Link}.
#'
#' Selva, P. (n.d.). \emph{Logistic Regression with R.} r-statistics.co.
#' \href{http://r-statistics.co/Logistic-Regression-With-R.html}{Link}.
#'
#' @export
#' @importFrom stats predict glm as.formula binomial
#' @importFrom caret createFolds
#' @importFrom dplyr summarise_if mutate_if
#' @importFrom magrittr %>%

trainCV <- function(data, dependentVarIndex, K = 5) {
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

  if (K < 2) {
    stop("K should be a positive integer greater than or equal to 2,
    indicating the number of groups that a given data set is to be split into.")
  }

  modelList <- list()
  prList <- list()
  testList <- list()

  data[[dependentVarIndex]] <- as.factor(data[[dependentVarIndex]])
  folds <- caret::createFolds(as.factor(data[[dependentVarIndex]]),
                              k = K,
                              list = FALSE)

  for (i in 1:K) {
    # Creating training and test data sets
    trainingData <- data[which(folds != i), ]
    testData <- data[which(folds == i), ]

    # Calculating means and sds of numeric variables
    means <- trainingData %>% dplyr::summarise_if(is.numeric, mean)
    sds <- trainingData %>% dplyr::summarise_if(is.numeric, sd)

    # Standardize the training set
    standardizedTrain <- trainingData %>%
      dplyr::mutate_if(is.numeric, ~(scale(.) %>% as.vector))

    # Standardize the test set
    standardizedTest <- testData
    for (j in 1:ncol(means)) {
      colName <- colnames(means)[j]
      standardizedTest[colName] <-
        (testData[colName] - as.numeric(means[j])) / as.numeric(sds[j])
    }

    # Adding the test set to testList
    testList[[i]] <- standardizedTest

    # Training the logit regression model
    model <- stats::glm(
                 stats::as.formula(paste(colnames(data)[dependentVarIndex], "~",
                                         paste('`', colnames(data)[-dependentVarIndex],
                                               '`', collapse = "+", sep = ""),
                                         sep = "")),
                 family = stats::binomial(link = 'logit'),
                 data = standardizedTrain
                 )

    # Adding the model to modelList
    modelList[[i]] <- model

    # Obtaining the probabilities predicted from the model
    pr <- stats::predict(model, standardizedTest, type = "response")
    prList[[i]] <- pr
  }
  results <- list(models = modelList,
                  predictions = prList,
                  testSets = testList)
  class(results) <- "trainCV"

  return(results)
}


# [END]
