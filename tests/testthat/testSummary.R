library(Gboot)

test_that("correct output being used", {
  expect_error(summaryCI(1), "Please use the matrix returned from CalcGTheoryCI")
})
