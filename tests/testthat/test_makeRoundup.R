#' Copyright(c) 2017-2019 R. Mark Sharp
# This file is part of nprcmanager
context("makeRoundUp")
library(testthat)
test_that("makeRoundup correctly rounded values based on integer and modulas", {
  int1 <- makeRoundUp(int = 1, modulas = 2)
  expect_equal(int1, 2)
  int1 <- makeRoundUp(int = 1, modulas = 3)
  expect_equal(int1, 3)
  int1 <- makeRoundUp(int = 3, modulas = 2)
  expect_equal(int1, 4)
  int1 <- makeRoundUp(int = 16, modulas = 5)
  expect_equal(int1, 20)
})
