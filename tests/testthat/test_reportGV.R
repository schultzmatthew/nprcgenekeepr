#' Copyright(c) 2017-2019 R. Mark Sharp
# This file is part of nprcmanager
context("reportGV")
library(testthat)
qcPed <- nprcmanager::qcPed
gvReport <- reportGV(qcPed, guIter = 100)
test_that("reportGV forms correct genetic value report", {
  expect_equal(names(gvReport), c("report", "kinship", "gu", "fe", "fg",
                                  "maleFounders", "femaleFounders", "total"))
  expect_equal(names(gvReport$report),
               c("id", "sex", "age", "birth", "exit", "population",
                 "indivMeanKin", "zScores", "gu", "totalOffspring",
                 "livingOffspring", "value", "rank"))
  expect_equal(nrow(gvReport$report), nrow(qcPed))
  expect_equal(nrow(gvReport$gu), nrow(qcPed))
  expect_equal(gvReport$maleFounders, 20)
  expect_equal(gvReport$femaleFounders, 61)
})
updateProgress <- function(n = 1, detail = NULL, value = 0, reset = FALSE) {
  "stub"
}

gvReport <- reportGV(qcPed, guIter = 100, updateProgress = updateProgress)
test_that("reportGV forms correct genetic value report with updateProgress defined", {
  expect_equal(names(gvReport), c("report", "kinship", "gu", "fe", "fg",
                                  "maleFounders", "femaleFounders", "total"))
  expect_equal(names(gvReport$report),
               c("id", "sex", "age", "birth", "exit", "population",
                 "indivMeanKin", "zScores", "gu", "totalOffspring",
                 "livingOffspring", "value", "rank"))
  expect_equal(nrow(gvReport$report), nrow(qcPed))
  expect_equal(nrow(gvReport$gu), nrow(qcPed))
  expect_equal(gvReport$maleFounders, 20)
  expect_equal(gvReport$femaleFounders, 61)
})
