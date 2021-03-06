library(GeneCTLR)

test_that("a data frame with one dependent variable and multiple independent
          variables", {
            library(caret)

            dat <- twoClassSim(200)
            results <- trainCV(data = dat,
                               dependentVarIndex = 16,
                               K = 3)

            expect_type(results, "list")
            expect_s3_class(results, "trainCV")
            expect_length(results, 3)
          })

test_that("a data frame with one dependent variable and one independent
          variable", {
            library(caret)

            dat <- twoClassSim(200)
            dat2 <- subset(dat, select = c(1, 16))
            results <- trainCV(data = dat2,
                               dependentVarIndex = 2,
                               K = 3)

            expect_type(results, "list")
            expect_s3_class(results, "trainCV")
            expect_length(results, 3)
          })

test_that("TrainCV error upon invalid user input", {
  library(caret)
  # data provided as a list
  dat <- list(a = c(0, 293, 1), b = c(2, 0, 20))
  expect_error(results <- trainCV(data = dat, dependentVarIndex = 1))

  # col_index <= 0
  dat <- twoClassSim(200)
  expect_error(results <- trainCV(data = dat, dependentVarIndex = -1))

  # K <= 1
  dat <- twoClassSim(200)
  expect_error(results <- trainCV(data = dat, dependentVarIndex = 16, K = 1))
})


