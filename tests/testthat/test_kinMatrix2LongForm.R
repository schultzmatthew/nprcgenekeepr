#' Copyright(c) 2017-2019 R. Mark Sharp
# This file is part of nprcgenekeepr
context("kinMatrix2LongForm")
library(testthat)

ped <- nprcgenekeepr::lacy1989Ped
ped$gen <- findGeneration(ped$id, ped$sire, ped$dam)
kmat <- kinship(ped$id, ped$sire, ped$dam, ped$gen)
reformatedKmat <- kinMatrix2LongForm(kmat, rm.dups = FALSE)
reformatedNoDupsKmat <- kinMatrix2LongForm(kmat, rm.dups = TRUE)

test_that("kinMatrix2LongForm makes correct transformation", {
  expect_equal(reformatedKmat[1, 3], as.numeric(kmat[1, 1]))
  expect_equal(reformatedKmat[3, 3], as.numeric(kmat[1, 3]))
  expect_equal(reformatedKmat[5, 3], as.numeric(kmat[1, 5]))
  expect_equal(reformatedKmat[10, 3], as.numeric(kmat[2, 3]))
  expect_equal(reformatedNoDupsKmat[9, 3], as.numeric(kmat[2, 3]))
})
