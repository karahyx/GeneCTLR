#' Performs logistic model training and cross validation
#'
#' A function that trains the logistic regression model and performs a K-fold
#' cross validation.
#'
#' @param data A data frame to be trained. The data frame should only contain
#' one dependent variable and one or more independent variables. All independent
#' variables to be used in training the model should be of type numeric.
#' @param col_index A positive integer indicating the index of the dependent
#' variable column in the data set.
#' @param K A positive integer indicating the number of groups that a given
#' data set is to be split into.
#'
#' @return Returns an S3 object of class trainCV with the following elements:
#' \itemize{
#'   \item models - Fitted logistic regression models.
#'   \item predictions - The probabilities predicted by each model.
#'   \item test_sets - Test data sets used in each fold of the cross validation.
#' }
#'
#' @examples
#' # Changing the class of hasCanonicalRBDs (an independent variable) to
#' # numeric to be used for the trainCV function
#' imputedRBPs <- impute(rbps, "mean")
#' imputedRBPs$hasCanonicalRBDs <- as.numeric(imputedRBPs$hasCanonicalRBDs)
#'
#' # Remove the Human Gene and pLI columns
#' rbps <- subset(imputedRBPs, select = -c(1, 8))
#'
#' results <- trainCV(data = rbps, col_index = 10)
#'
#' @references
#' Alice, M. (2020, July 5). \emph{How to perform a logistic regression in R:
#' R-bloggers.} R. \href{https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/}{Link}.
#'
#' Selva, P. (n.d.). \emph{Logistic Regression with R.} r-statistics.co.
#' \href{http://r-statistics.co/Logistic-Regression-With-R.html}{Link}.
#'
#' @export
#' @importFrom stats predict
#' @importFrom stats glm
#' @importFrom stats as.formula
#' @importFrom stats binomial
#' @importFrom caret createFolds
#' @importFrom dplyr summarise_if
#' @importFrom dplyr mutate_if
#' @importFrom magrittr %>%

trainCV <- function(data, colIndex, K = 5) {
  # Performing checks of user input
  if (is.data.frame(data) == FALSE) {
    stop("data should be a data frame.")
  }

  if (col_index <= 0) {
    stop("col_index should be a positive integer indicating the index of the
         dependent variable column in data.")
  }

  if (K <= 0) {
    stop("K should be a positive integer indicating the number of groups that a
    given data set is to be split into.")
  }

  modelList <- list()
  prList <- list()
  testList <- list()

  data[[colIndex]] <- as.factor(data[[colIndex]])
  folds <- createFolds(as.factor(data[[colIndex]]), k = K, list = FALSE)

  for (i in 1:K) {
    # Creating training and test data sets
    trainingData <- data[which(folds != i), ]
    testData <- data[which(folds == i), ]

    # Calculating means and sds of numeric variables
    means <- trainingData %>% summarise_if(is.numeric, mean)
    sds <- trainingData %>% summarise_if(is.numeric, sd)

    # Standardize the training set
    standardizedTrain <- trainingData %>%
                              mutate_if(is.numeric, ~(scale(.) %>% as.vector))

    # Standardize the test set
    standardizedTest <- testData
    for (j in 1:ncol(means)) {
      colName <- colnames(means)[j]
      standardizedTest[colName] <-
        (testData[colName] - as.numeric(means[j])) / as.numeric(sds[j])
    }

    # Adding the test set to test_list
    testList[[i]] <- standardizedTest

    # Training the logit regression model
    model <- glm(
      as.formula(paste(colnames(data)[colIndex], "~",
                       paste('`', colnames(data)[-colIndex], '`',
                             collapse = "+", sep = ""),
                       sep = "")),
                 family = binomial(link = 'logit'),
                 data = standardizedTrain)

    # Adding the model to model_list
    modelList[[i]] <- model

    # Obtaining the probabilities predicted from the model
    pr <- predict(model, standardizedTest, type = "response")
    prList[[i]] <- pr
  }
  results <- list(models = modelList,
                  predictions = prList,
                  testSets = testList)
  class(results) <- "trainCV"

  return(results)
}

