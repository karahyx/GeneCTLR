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
