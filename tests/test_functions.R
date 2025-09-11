library(testthat)

test_that("process_data adds processed", {
  df <- data.frame(x=1:2)
  result <- process_data(df)
  expect_true("processed" %in% colnames(result))
})
