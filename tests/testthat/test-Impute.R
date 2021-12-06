library(GeneCTLR)

test_that("a data frame with no missing values", {

  dat <- data.frame(a = c(20, 9, 10),
                    b = c(1, 293, 0),
                    c = c(2, 103, 284))
  results <- impute(dat, "mean")

  expected <- data.frame(a = c(20, 9, 10),
                             b = c(1, 293, 0),
                             c = c(2, 103, 284))

  expect_type(results, "list")
  expect_s3_class(results, "data.frame")
  expect_length(results, 3)
  expect_identical(results, expected)
})

test_that("a data frame with multiple missing values tested with mean", {

  dat <- data.frame(a = c(20, NA, 10, 2048, NA),
                    b = c(1, 293, 0, 1230, 23),
                    c = c(2, 103, NA, NA, NA))
  results <- impute(dat, "mean")

  aMean <- mean(dat$a, na.rm = T)
  cMean <- mean(dat$c, na.rm = T)
  expected <- data.frame(a = c(20, aMean, 10, 2048, aMean),
                         b = c(1, 293, 0, 1230, 23),
                         c = c(2, 103, cMean, cMean, cMean))

  expect_type(results, "list")
  expect_s3_class(results, "data.frame")
  expect_length(results, 3)
  expect_identical(results, expected)
})

test_that("a data frame with multiple missing values tested with median", {

  dat <- data.frame(a = c(20, NA, 10, 2048, NA),
                    b = c(1, 293, 0, 1230, 23),
                    c = c(2, 103, NA, NA, NA))
  results <- impute(dat, "median")

  expected <- data.frame(a = c(20, 20, 10, 2048, 20),
                         b = c(1, 293, 0, 1230, 23),
                         c = c(2.0, 103.0, 52.5, 52.5, 52.5))

  expect_type(results, "list")
  expect_s3_class(results, "data.frame")
  expect_length(results, 3)
  expect_identical(results, expected)
})

test_that("Impute error upon invalid user input", {
  # data provided as a list
  dat <- list(a = c(0, 293, 1), b = c(2, 0, 20))
  expect_error(results <- impute(dat, "mean"))

  # replace provided as a different string than mean or median
  expect_error(results <- impute(dat, "flower"))
})



