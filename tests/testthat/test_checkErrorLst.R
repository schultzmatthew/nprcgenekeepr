#' Copyright(c) 2017-2019 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("checkErrorLst")
library(testthat)
## Most of this is tested via testing of functions that call this.
test_that("checkErrorLst returns FALSE is given NULL", {
  expect_false(checkErrorLst(NULL))
})
