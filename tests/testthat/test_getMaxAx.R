#' Copyright(c) 2017-2019 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("getMaxAx")
library(testthat)
test_that("getMaxAx correctly rounded values based on integer and modulas", {
  int1 <- nprcgenekeepr:::getMaxAx(bins = list(male = 11, female = 5),
                                 axModulas = 5)
  expect_equal(int1, 15)
  int1 <- nprcgenekeepr:::getMaxAx(bins = list(male = 21, female = 5),
                                 axModulas = 5)
  expect_equal(int1, 25)
  int1 <- nprcgenekeepr:::getMaxAx(bins = list(male = 11, female = 15),
                                 axModulas = 5)
  expect_equal(int1, 20)
  int1 <- nprcgenekeepr:::getMaxAx(bins = list(male = 11, female = 203),
                                 axModulas = 5)
  expect_equal(int1, 205)
  int1 <- nprcgenekeepr:::getMaxAx(bins = list(male = 101, female = 5),
                                 axModulas = 105)
  expect_equal(int1, 105)
})
