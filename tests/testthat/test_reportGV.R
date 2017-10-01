context("reportGV")
library(testthat)
data(baboonPed)
gvReport <- reportGV(baboonPed, gu.iter = 100)
test_that("reportGV forms correct genetic value report", {
  expect_equal(names(gvReport), c("report", "kinship", "gu", "fe", "fg",
                                  "maleFounders", "femaleFounders", "total"))
  expect_equal(names(gvReport$report),
               c("id", "sex", "age", "birth", "exit", "population",
                 "indivMeanKin", "z.scores", "gu", "totalOffspring",
                 "living.offspring", "value", "rank"))
  expect_equal(nrow(gvReport$report), nrow(baboonPed))
  expect_equal(nrow(gvReport$gu), nrow(baboonPed))
  expect_equal(gvReport$maleFounders, 20)
  expect_equal(gvReport$femaleFounders, 61)
})
