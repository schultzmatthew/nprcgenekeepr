context("calcA")
library(testthat)
data("ped1_alleles")

test_that("alleleFreq forms dataframe with correct calculations", {
  rare <- calcA(ped1_alleles, threshold = 3, byID = FALSE)
  expect_equal(sum(rare[, 1]), 318)
  expect_equal(sum(rare[, 2]), 325)
  expect_equal(sum(rare[, 3]), 313)
  expect_equal(sum(rare[, 4]), 328)
})
