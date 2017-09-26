context("addParents")
library(testthat)
library(stringi)
cols <- c("id", "sire", "dam", "sex", "gen", "birth", "exit", "age",
          "ancestry", "population", "origin", "status", "condition",
          "spf", "vasx.ovx", "ped.num", "first", "second", "first_name",
          "second_name")

test_that("getPossibleCols returns the right columns", {
  expect_equal(getPossibleCols(), cols)
})
