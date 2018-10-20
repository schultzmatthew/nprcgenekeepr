context("getChangedColsTab")
library(testthat)
library(lubridate)
library(stringi)

set.seed(10)
data(pedSix)
changedColsTab <- getChangedColsTab(qcStudbook(pedSix, errors = TRUE), "test")
test_that("getChangedColsTab creates predictable output", {
  expect_true(stri_detect_fixed(changedColsTab$children[[1]]$children[[1]],
                                pattern = "egoid to id"))
})
