library(GeneCTLR)

test_that("a data frame with one dependent variable and multiple independent
          variables", {
            library(caret)

            dat <- twoClassSim(200)
            length(dat) # 16
            results <- trainCV(data = dat, colIndex = 16)
            newDat <- addPredictions(results, dat)

            expect_type(results, "list")
            expect_s3_class(newDat, "data.frame")
            expect_length(newDat, 17)
          })

test_that("a data frame with one dependent variable and one independent
          variable", {
            library(caret)

            dat <- twoClassSim(200)
            dat2 <- subset(dat, select = c(1, 16))
            length(dat2) # 2
            results <- trainCV(data = dat2, colIndex = 2)
            newDat <- addPredictions(results, dat2)

            expect_type(results, "list")
            expect_s3_class(newDat, "data.frame")
            expect_length(newDat, 3)
          })

test_that("TrainCV error upon invalid user input", {
  # results provided as a string
  results <- "train"
  expect_error(newDat <- addPredictions(results, dat))

  # data provided as a list
  dat <- list(a = c(0, 293, 1), b = c(2, 0, 20))
  expect_error(newDat <- addPredictions(results, dat))
})
