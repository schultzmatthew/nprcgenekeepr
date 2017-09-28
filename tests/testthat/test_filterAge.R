context("filterAge")
library(testthat)
data("baboonPed")
ped <- baboonPed
ped$gen <- findGeneration(ped$id, ped$sire, ped$dam)
kmat <- kinship(ped$id, ped$sire, ped$dam, ped$gen,
                sparse = FALSE)
kin <- reformatKinship(kmat)
filteredKin1 <- filterAge(kin, ped, min.age = 1)
filteredKin5 <- filterAge(kin, ped, min.age = 5)
lessThan5 <- c("35315", "32732", "33873", "33895", "34163", "34183", "34984",
               "32358", "32560", "32650", "32741", "32743", "32798", "33164",
               "33607", "33866", "33893", "34155", "34162", "34210", "34832",
               "34953", "36017", "32771", "33088", "35153")
test_that("filterAge removes the correct rows", {
  expect_false(any("34183" %in% filteredKin1$id1))
  expect_true(any("35315" %in% filteredKin1$id1))
  expect_false(any("35315" %in% filteredKin5$id1))
}
)
