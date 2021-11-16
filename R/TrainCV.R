#' Performs logistic model training and cross validation
#'
#' A function that trains the logistic regression model and performs repeated
#' K-fold cross validation.
#'
#' @param data A data frame to be trained. The data frame should only contain
#' one dependent variable and one or more independent variables. All independent
#' variables to be used in training the model should be of type numeric.
#' @param col_index A positive integer indicating the index of the dependent
#' variable column in the data set.
#' @param K A positive integer indicating the number of groups that a given
#' data set is to be split into.
#' @param repetition A positive integer indicating the number of repetitions for
#' the K-fold cross validation. Must be at least 2.
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
#' rbps$hasCanonicalRBDs <- as.numeric(rbps$hasCanonicalRBDs)
#'
#' # Remove the Human Gene and pLI columns
#' rbps <- subset(rbps, select = -c(1, 8))
#'
#' rbps_results <- trainCV(data = rbps, col_index = 10)
#'
#' # Show the first fitted model
#' rbps_results$models[[1]]
#'
#' @references
#' Alice, M. (2020, July 5). \emph{How to perform a logistic regression in R:
#' R-bloggers.} R. \href{https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/}{Link}.
#'
#' Selva, P. (n.d.). \emph{Logistic Regression with R.} r-statistics.co.
#' \href{http://r-statistics.co/Logistic-Regression-With-R.html}{Link}.
#'
#' @export
#' @import stats
#' @import caret
#' @import dplyr

trainCV <- function(data, col_index, K = 5, repetition = 3) {
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

  if(repetition <= 0) {
    stop("repetition should be a positive integer indicating the number of
    repetitions for the K-fold cross validation.")
  }

  model_list <- list()
  pr_list <- list()
  test_list <- list()

  folds <- createFolds(as.factor(data[[col_index]]), k = K, list = FALSE)
  data[[col_index]] <- as.factor(data[[col_index]])

  for (i in 1:repetition) {
    # Creating training and test data sets
    training_data <- data[which(folds != i), ]
    test_data <- data[which(folds == i), ]

    # Calculating means and sds of numeric variables
    means <- training_data %>% summarise_if(is.numeric, mean)
    sds <- training_data %>% summarise_if(is.numeric, sd)

    # Standardize the training set
    standardized_train <- training_data %>%
                              mutate_if(is.numeric, ~(scale(.) %>% as.vector))

    # Standardize the test set
    standardized_test <- test_data
    for (j in 1:ncol(means)) {
      col_name <- colnames(means)[j]
      standardized_test[col_name] <- (test_data[col_name] - as.numeric(means[j])) / as.numeric(sds[j])
    }

    # Adding the test set to test_list
    test_list[[i]] <- standardized_test

    # Training the logit regression model
    model <- glm(
      as.formula(paste(colnames(data)[col_index], "~",
                       paste('`', colnames(data)[-col_index], '`',
                             collapse = "+", sep = ""),
                       sep = "")),
                 family = binomial(link = 'logit'),
                 data = standardized_train)

    # Adding the model to model_list
    model_list[[i]] <- model

    # Obtaining the probabilities predicted from the model
    pr <- stats::predict(model, standardized_test, type = "response")
    pr_list[[i]] <- pr
  }
  results <- list(models = model_list,
                  predictions = pr_list,
                  test_sets = test_list)
  class(results) <- "trainCV"
  return(results)
}

