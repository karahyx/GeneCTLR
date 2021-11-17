library(GeneCTLR)

test_that("a data frame with no missing values", {

  dat <- data.frame(a = c(20, 9, 10),
                    b = c(1, 293, 0),
                    c = c(2, 103, 284))
  results <- missingValues(dat)

  expected_NA <- setNames(c(0, 0, 0), c("a", "b", "c"))
  expected_unique <- setNames(c(3, 3, 3), c("a", "b", "c"))

  expect_type(results, "list")
  expect_s3_class(results, "MissingValues")
  expect_length(results, 2)
  expect_identical(trunc(results$NA_results), expected_NA)
  expect_identical(trunc(results$unique_results), expected_unique)
})

test_that("a data frame with multiple missing values", {

  dat <- data.frame(a = c(20, NA, 10, 2048, NA),
                    b = c(1, 293, 0, 1230, 23),
                    c = c(2, 103, NA, NA, NA))
  results <- missingValues(dat)

  expected_NA <- setNames(c(2, 0, 3), c("a", "b", "c"))
  expected_unique <- setNames(c(4, 5, 3), c("a", "b", "c"))

  expect_type(results, "list")
  expect_s3_class(results, "MissingValues")
  expect_length(results, 2)
  expect_identical(trunc(results$NA_results), expected_NA)
  expect_identical(trunc(results$unique_results), expected_unique)
})

test_that("a data frame with repeating values and missing values", {
  dat <- data.frame(a = c(20, 20, NA, 20, 20),
                    b = c(1, 293, 0, 0, 0),
                    c = c(NA, NA, NA, NA, NA))
  results <- missingValues(dat)

  expected_NA <- setNames(c(1, 0, 5), c("a", "b", "c"))
  expected_unique <- setNames(c(2, 3, 1), c("a", "b", "c"))

  expect_type(results, "list")
  expect_s3_class(results, "MissingValues")
  expect_length(results, 2)
  expect_identical(trunc(results$NA_results), expected_NA)
  expect_identical(trunc(results$unique_results), expected_unique)
})

test_that("MissingValues error upon invalid user input", {
  # data provided as a list
  dat <- list(a = c(0, 293, 1), b = c(2, 0, 20))
  expect_error(results <- missingValues(dat))
})
