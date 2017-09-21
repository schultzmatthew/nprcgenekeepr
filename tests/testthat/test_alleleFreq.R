context("alleleFreq")
library(testthat)
data("ped1_alleles")
ids <- ped1_alleles$id
alleles <- ped1_alleles[, !(names(ped1_alleles) %in% c("id", "parent"))]
test_that("alleleFreq forms dataframe with correct calculations", {
  aF <- alleleFreq(alleles[[1]], ids = NULL)
  expect_equal(aF$freq[aF$allele == 20004], 10)
  expect_equal(aF$freq[aF$allele == 20012], 11)
  aF <- alleleFreq(alleles[[4]], ids = NULL)
  expect_equal(aF$freq[aF$allele == 20004], 14)
  expect_equal(aF$freq[aF$allele == 20012], 9)
  aF <- alleleFreq(ped1_alleles[[1]], ids = ids)
  expect_equal(aF$freq[aF$allele == 20004], 10)
  expect_equal(aF$freq[aF$allele == 20012], 10)
  aF <- alleleFreq(ped1_alleles[[4]], ids = ids)
  expect_equal(aF$freq[aF$allele == 20004], 13)
  expect_equal(aF$freq[aF$allele == 20012], 9)
})
