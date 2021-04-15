#' Copyright(c) 2017-2020 R. Mark Sharp
#' This file is part of nprcgenekeepr
<<<<<<< HEAD

context("convertFromCenter")
library(testthat)
original <- c("y", "yes", "Y", "Yes", "YES",
              "n", "N", "No", "NO", "no",
              "t", "T", "True", "true", "TRUE",
              "f", "F", "false", "False","FALSE",
              NA)
fromCenter <- convertFromCenter(original)
test_that("convertFromCenter makes correct transformations", {
  expect_true(is.logical(fromCenter))
  expect_equal(fromCenter[1], TRUE)
  expect_equal(fromCenter[2], TRUE)
  expect_equal(fromCenter[3], TRUE)
  expect_equal(fromCenter[4], TRUE)
  expect_equal(fromCenter[5], TRUE)
  expect_equal(fromCenter[6], FALSE)
  expect_equal(fromCenter[7], FALSE)
  expect_equal(fromCenter[8], FALSE)
  expect_equal(fromCenter[9], FALSE)
  expect_equal(fromCenter[10], FALSE)
  expect_equal(fromCenter[11], TRUE)
  expect_equal(fromCenter[12], TRUE)
  expect_equal(fromCenter[13], TRUE)
  expect_equal(fromCenter[14], TRUE)
  expect_equal(fromCenter[15], TRUE)
  expect_equal(fromCenter[16], FALSE)
  expect_equal(fromCenter[17], FALSE)
  expect_equal(fromCenter[18], FALSE)
  expect_equal(fromCenter[19], FALSE)
  expect_equal(fromCenter[20], FALSE)
  expect_equal(fromCenter[21], NA)

})
=======
context("convertFromCenter")
library(testthat)
original <- c("y", "yes", "Y", "Yes", "YES", "n", "N", "No", "NO", "no",
            "t", "T", "True", "true", "TRUE", "f", "F", "false", "False",
            "FALSE")
status <- convertFromCenter(original)
test_that("convertFromCenter makes correct transformations", {
  expect_true(is.logical(status))
  expect_equal(status[1], TRUE)
  expect_equal(status[2], TRUE)
  expect_equal(status[3], TRUE)
  expect_equal(status[4], TRUE)
  expect_equal(status[5], TRUE)
  expect_equal(status[6], FALSE)
  expect_equal(status[7], FALSE)
  expect_equal(status[8], FALSE)
  expect_equal(status[9], FALSE)
  expect_equal(status[10], FALSE)
  expect_equal(status[11], TRUE)
  expect_equal(status[12], TRUE)
  expect_equal(status[13], TRUE)
  expect_equal(status[14], TRUE)
  expect_equal(status[15], TRUE)
  expect_equal(status[16], FALSE)
  expect_equal(status[17], FALSE)
  expect_equal(status[18], FALSE)
  expect_equal(status[19], FALSE)
  expect_equal(status[20], FALSE)
})
test_that("convertFromCenter() detects input error", {
  original <- c("y", "&")
  expect_error(convertFromCenter(original), "fromCenter field has ambiguous")
})

>>>>>>> upstream/issue8
