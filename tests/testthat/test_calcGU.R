context("calcGU")
library(testthat)
data("ped1_alleles")
test_that("calcGU forms dataframe with correct calculations", {
<<<<<<< HEAD
  gu_1 <- calcGU(alleles, threshold = 1, byID = FALSE, pop = NULL)
  gu_3 <- calcGU(alleles, threshold = 3, byID = FALSE, pop = NULL)
  expect_equal(length(gu_1$gu[gu_1$gu == 50]), 110)
  expect_equal(length(gu_3$gu[gu_3$gu == 50]), 43)
  gu_1 <- calcGU(alleles, threshold = 2, byID = TRUE, pop = NULL)
  gu_3 <- calcGU(alleles, threshold = 3, byID = FALSE, pop = alleles$id[20:60])
=======
  gu_1 <- calcGU(ped1_alleles, threshold = 1, byID = FALSE, pop = NULL)
  gu_3 <- calcGU(ped1_alleles, threshold = 3, byID = FALSE, pop = NULL)
  expect_equal(length(gu_1$gu[gu_1$gu == 50]), 110)
  expect_equal(length(gu_3$gu[gu_3$gu == 50]), 43)
  gu_1 <- calcGU(ped1_alleles, threshold = 2, byID = TRUE, pop = NULL)
  gu_3 <- calcGU(ped1_alleles, threshold = 3, byID = FALSE,
                 pop = ped1_alleles$id[20:60])
>>>>>>> 128608fb032d578c2f3b5fe6b5dc4e9635a6ac43
  expect_equal(length(gu_1$gu[gu_1$gu == 50]), 53)
  expect_equal(length(gu_3$gu[gu_3$gu == 50]), 0)
})
