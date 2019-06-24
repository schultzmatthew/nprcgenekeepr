context("print.summary.nprcmanagGV")
library(testthat)
library(lubridate)
library(stringi)

set_seed(10)
test_that("print.summary.nprcmanagGV prints expected output", {
  pedClean <- reportGV(nprcmanager::qcPed)
  expect_success(expect_output(print(summary(pedClean))))
  })
