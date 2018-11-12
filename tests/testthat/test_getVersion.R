context("getVersion")
library(testthat)
library(stringi)
test_that("getVersion returns a version", {
  version <- getVersion()
  expect_true(stri_detect_regex(version, pattern = "^[0-9]{1,2}([.][0-9]{1,2})")) # version
  expect_true(stri_detect_regex(version, pattern = "[0-9]{4}")) # date
})

