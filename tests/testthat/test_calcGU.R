context("calcGU")
library(testthat)
data("ped1_alleles")
test_that("calcGU forms dataframe with correct calculations", {
  gu_1 <- calcGU(ped1_alleles, threshold = 1, by.id = FALSE, pop = NULL)
  gu_3 <- calcGU(ped1_alleles, threshold = 3, by.id = FALSE, pop = NULL)
  expect_equal(length(gu_1$gu[gu_1$gu == 50]), 110)
  expect_equal(length(gu_3$gu[gu_3$gu == 50]), 43)
  gu_1 <- calcGU(ped1_alleles, threshold = 2, by.id = TRUE, pop = NULL)
  gu_3 <- calcGU(ped1_alleles, threshold = 3, by.id = FALSE,
                 pop = ped1_alleles$id[20:60])
  expect_equal(length(gu_1$gu[gu_1$gu == 50]), 53)
  expect_equal(length(gu_3$gu[gu_3$gu == 50]), 0)
})
