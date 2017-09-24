context("getDatedFilename")
library(testthat)
library(lubridate)
library(stringi)
dateStamp <- stri_replace_all_fixed(
  stri_replace_all_fixed(as.character(now()), " ", "_"), ":", "_")
test_that("getDatedFilename form correctly dated file name", {
  expect_equal(getDatedFilename("testName"), stri_c(dateStamp, "_", "testName"))
})
