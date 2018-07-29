context("rankSubjects")
library(testthat)
## reportGV() unit test is weak.
data("finalRpt")
rpt <- rankSubjects(finalRpt)

test_that("rankSubjects ranks subject correctly", {
  expect_equal(nrow(rpt[[2]]), 0)
  expect_equal(rpt[[1]][1, "value"], "High Value")
  expect_equal(rpt[[3]][1, "value"], "Low Value")
  expect_equal(rpt[[3]][1, "rank"], 277)
  expect_equal(rpt[[1]][276, "rank"], 276)
 })
