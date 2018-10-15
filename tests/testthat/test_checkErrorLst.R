context("checkErrorLst")
library(testthat)
## Most of this is tested via testing of functions that call this.
test_that("checkErrorLst returns FALSE is given NULL", {
  expect_false(checkErrorLst(NULL))
})
