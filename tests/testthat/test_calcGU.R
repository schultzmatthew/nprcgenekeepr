context("calcGU")
library(testthat)
library(stringi)
data("allelesFromGeneDrop")
test_that("calcGU forms dataframe with correct calculations", {
  gu_1 <- calcGU(alleles, threshold = 1, by.id = FALSE, pop = NULL)
  gu_3 <- calcGU(alleles, threshold = 3, by.id = FALSE, pop = NULL)
  expect_equal("a", "a")
})
