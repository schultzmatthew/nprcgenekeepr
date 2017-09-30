context("filterPairs")
library(testthat)
data("lacy1989Ped")
ped <- lacy1989Ped

ped$gen <- findGeneration(ped$id, ped$sire, ped$dam)
kmat <- kinship(ped$id, ped$sire, ped$dam, ped$gen)
kin <- reformatKinship(kmat, rm.dups = FALSE)
threshold <- 0.1
kin <- filterThreshold(kin, threshold = threshold)
ped$sex <- c("M", "F", "M", "M", "F", "F", "M")
kinNull <- filterPairs(kin, ped, ignore = NULL)
kinFF <- filterPairs(kin, ped, ignore = c("F", "F"))
kinMM <- filterPairs(kin, ped, ignore = c("M", "M"))

test_that("filterPairs removes the correct pairs", {
  expect_equal(nrow(kinNull), 39)
  expect_equal(nrow(kinFF), 16)
  expect_equal(nrow(kinMM), 7)
  expect_equal(nrow(kinFF[kinFF$id1 == "B", kinFF$id2 == "E", ]), 0)
  expect_equal(nrow(kinFF[kinFF$id1 == "B", kinFF$id2 == "F", ]), 0)
  expect_equal(nrow(kinMM[kinMM$id1 == "A", kinMM$id2 == "D", ]), 0)
})
