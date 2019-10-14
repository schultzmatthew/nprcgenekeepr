#' Copyright(c) 2017-2019 R. Mark Sharp
#' This file is part of nprcmanager
context("filterReport")
library(testthat)

rpt <- nprcmanager::pedWithGenotypeReport$report
test_that("filterReport correctly subsets reports based on provide IDs", {
  rpt1 <- filterReport(c("GHH9LB", "BD41WW"), rpt)
  expect_equal(nrow(rpt1), 2)
  rpt1 <- filterReport(c(), rpt)
  expect_equal(nrow(rpt1), 0)
  rpt1 <- filterReport(rpt$id, rpt)
  expect_equal(nrow(rpt1), nrow(rpt))
})
