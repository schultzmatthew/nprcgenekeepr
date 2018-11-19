context("getPossibleCols")
library(testthat)
library(stringi)
cols <- c("id", "sire", "dam", "sex", "gen", "birth", "exit", "age",
          "ancestry", "population", "origin", "status", "condition",
          "spf", "vasxOvx", "pedNum", "first", "second", "first_name",
          "second_name", "record_status")

test_that("getPossibleCols returns the right columns", {
  expect_equal(getPossibleCols(), cols)
})
