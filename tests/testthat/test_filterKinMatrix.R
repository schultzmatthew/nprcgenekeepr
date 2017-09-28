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
ids <- c("35315", "32732", "33873", "33895", "34163", "34183", "34984",
         "32358", "32560", "32650", "32741", "32743", "32798", "33164",
         "33607", "33866", "33893", "34155", "34162", "34210", "34832",
         "34953", "36017", "32771", "33088", "35153")
kmatFiltered <- filterKinMatrix(ids, kmat)
test_that("filterKinMatrix leaves the correct rows", {
  expect_equal(nrow(kmatFiltered), length(ids))
  expect_equal(ncol(kmatFiltered), length(ids))
  expect_equal(kmat[(1:nrow(kmat))[rownames(kmat) %in% ids[20:23]],
                    (1:ncol(kmat))[colnames(kmat) %in% ids[20:23]]],
               kmatFiltered[20:23, 20:23])
}
)
