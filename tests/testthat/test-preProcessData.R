library(GeneCTLR)

test_that("a data frame with no deletion request from user", {

  dat <- data.frame(a = c(20, 9, 10),
                    b = c(1, 293, 0),
                    c = c(0, 1, 0))
  results <- preProcessData(data = dat,
                            dependentVarIndex = 3)

  expected <- data.frame(a = c(20, 9, 10),
                         b = c(1, 293, 0),
                         c = c(0, 1, 0))
  expected$c <- as.factor(expected$c)

  expect_type(results, "list")
  expect_s3_class(expected$c, "factor")
  expect_length(results, 3)
  expect_identical(results, expected)
})

test_that("a data frame with one user-specified column", {

  dat <- data.frame(a = c(20, 9, 10),
                    b = c(1, 293, 0),
                    c = c(0, 1, 0))
  results <- preProcessData(data = dat,
                            dependentVarIndex = 3,
                            deleteColumns = c(1))

  expected <- data.frame(b = c(1, 293, 0),
                         c = c(0, 1, 0))
  expected$c <- as.factor(expected$c)

  expect_type(results, "list")
  expect_s3_class(expected$c, "factor")
  expect_length(results, 2)
  expect_identical(results, expected)
})

test_that("a data frame with a different combination of dependentVarIndex and deleteColumns", {

  dat <- data.frame(a = c(20, 9, 10),
                    b = c(1, 293, 0),
                    c = c(0, 1, 0))
  results <- preProcessData(data = dat,
                            dependentVarIndex = 1,
                            deleteColumns = c(2))

  expected <- data.frame(a = c(20, 9, 10),
                         c = c(0, 1, 0))
  expected$a <- as.factor(expected$a)

  expect_type(results, "list")
  expect_s3_class(expected$a, "factor")
  expect_length(results, 2)
  expect_identical(results, expected)
})

test_that("Impute error upon invalid user input", {
  # data provided as a list
  dat <- list(a = c(0, 293, 1), b = c(2, 0, 20))
  expect_error(results <- preProcessData(dat, "mean"))

  # replace provided as a different string than mean or median
  expect_error(results <- preProcessData(dat, "flower"))
})



