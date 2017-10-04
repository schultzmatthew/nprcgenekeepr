context("getMaxAx")
library(testthat)
test_that("makeRoundup correctly rounded values based on integer and modulas", {
  int1 <- nprcmanager:::getMaxAx(bins = list(male = 11, female = 5),
                                 ax_modulas = 5)
  expect_equal(int1, 15)
  int1 <- nprcmanager:::getMaxAx(bins = list(male = 21, female = 5),
                                 ax_modulas = 5)
  expect_equal(int1, 25)
  int1 <- nprcmanager:::getMaxAx(bins = list(male = 11, female = 15),
                                 ax_modulas = 5)
  expect_equal(int1, 20)
  int1 <- nprcmanager:::getMaxAx(bins = list(male = 11, female = 203),
                                 ax_modulas = 5)
  expect_equal(int1, 205)
  int1 <- nprcmanager:::getMaxAx(bins = list(male = 101, female = 5),
                                 ax_modulas = 105)
  expect_equal(int1, 105)
})
