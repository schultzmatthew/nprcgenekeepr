context("reportGV")
library(testthat)
data(baboonPed)
gvReport <- reportGV(baboonPed, guIter = 100)
test_that("reportGV forms correct genetic value report", {
  expect_equal(names(gvReport), c("report", "kinship", "gu", "fe", "fg",
                                  "maleFounders", "femaleFounders", "total"))
  expect_equal(names(gvReport$report),
               c("id", "sex", "age", "birth", "exit", "population",
                 "indivMeanKin", "zScores", "gu", "totalOffspring",
                 "livingOffspring", "value", "rank"))
  expect_equal(nrow(gvReport$report), nrow(baboonPed))
  expect_equal(nrow(gvReport$gu), nrow(baboonPed))
  expect_equal(gvReport$maleFounders, 20)
  expect_equal(gvReport$femaleFounders, 61)
})
updateProgress <- function(n = 1, detail = NULL, value = 0, reset = FALSE) {
  "stub"
}

gvReport <- reportGV(baboonPed, guIter = 100, updateProgress = updateProgress)
test_that("reportGV forms correct genetic value report with updateProgress defined", {
  expect_equal(names(gvReport), c("report", "kinship", "gu", "fe", "fg",
                                  "maleFounders", "femaleFounders", "total"))
  expect_equal(names(gvReport$report),
               c("id", "sex", "age", "birth", "exit", "population",
                 "indivMeanKin", "zScores", "gu", "totalOffspring",
                 "livingOffspring", "value", "rank"))
  expect_equal(nrow(gvReport$report), nrow(baboonPed))
  expect_equal(nrow(gvReport$gu), nrow(baboonPed))
  expect_equal(gvReport$maleFounders, 20)
  expect_equal(gvReport$femaleFounders, 61)
})
