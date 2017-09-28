context("filterKinMatrix")
library(testthat)
data("baboonPed")
ped <- baboonPed
ped$gen <- findGeneration(ped$id, ped$sire, ped$dam)
kmat <- kinship(ped$id, ped$sire, ped$dam, ped$gen,
                sparse = FALSE)
ids <- baboonPed$id[c(189, 192, 194, 195)]
ncols <- ncol(kmat)
nrows <- nrow(kmat)
kmatFiltered <- filterKinMatrix(ids, kmat)
test_that("filterKinMatrix retains the correct rows and columns", {
  expect_equal(kmatFiltered[1, 2], kmat[189, 192])
  expect_equal(kmatFiltered[1, 3], kmat[189, 194])
  expect_equal(kmatFiltered[1, 4], kmat[189, 195])
  expect_equal(kmatFiltered[2, 3], kmat[192, 194])
})
