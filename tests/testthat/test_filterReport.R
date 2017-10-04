context("filterReport")
library(testthat)
data("pedWithGenotypeReport")

## set.seed(10) the report is deterministic and the pseudorandom number
## generator does not affect it.
rpt <- pedWithGenotypeReport$report
test_that("filterReport correctly subsets reports based on provide IDs", {
  rpt1 <- filterReport(c("14712", "30624"), rpt)
  expect_equal(nrow(rpt1), 2)
  rpt1 <- filterReport(c(), rpt)
  expect_equal(nrow(rpt1), 0)
  rpt1 <- filterReport(rpt$id, rpt)
  expect_equal(nrow(rpt1), nrow(rpt))
})
