context("orderReport")
library(testthat)
data("pedWithGenotypeReport")
data("baboonPed")
ped <- baboonPed
rpt <- pedWithGenotypeReport$report

test_that("orderReport correctly orders the report", {
  rpt1 <- orderReport(rpt, ped)
  expect_equal(nrow(rpt1), nrow(rpt))
  expect_true(all(rpt1$id %in% rpt1$id))
  set.seed(100)
  rpt$origin <- ifelse(sample(c(TRUE, FALSE), size = nrow(rpt), replace = TRUE,
                              prob = c(0.8, 0.2)), "TEXAS", NA)
  rpt$totalOffspring <- sample(0:3, size = nrow(rpt), replace = TRUE,
                              prob = c(0.8, 0.05, 0.05, 0.1))
  countUnk <- function(ids) {
    length(ids[stri_detect_fixed(ids, "U")])
  }
  rpt1 <- orderReport(rpt, ped)
  expect_equal(countUnk(rpt1$id[1:100]), 34)
  expect_equal(countUnk(rpt$id[1:50]), 21)
})
