context("groupAddAssign")
library(testthat)
data("baboonBreeders")
data("pedWithGenotype")
data("pedWithGenotypeReport")
set.seed(10)
groupAssignKTest <- groupAddAssign(candidates = baboonBreeders,
                                     currentGroup = NULL,
                                     kmat = pedWithGenotypeReport$kinship,
                                     ped = pedWithGenotype,
                                     ignore = NULL, minAge = 1, numGp = 2,
                                     withKin = TRUE)
set.seed(10)
groupAddKTest <- groupAddAssign(candidates = baboonBreeders,
                                  currentGroup = baboonBreeders[1:3],
                                  kmat = pedWithGenotypeReport$kinship,
                                  ped = pedWithGenotype,
                                  ignore = NULL, minAge = 1, numGp = 1,
                                  withKin = TRUE)
set.seed(10)
groupAssignTest <- groupAddAssign(candidates = baboonBreeders,
                                  currentGroup = NULL,
                                  kmat = pedWithGenotypeReport$kinship,
                                  ped = pedWithGenotype,
                                  ignore = NULL, minAge = 1, numGp = 2,
                                  withKin = FALSE)
set.seed(10)
groupAddTest <- groupAddAssign(candidates = baboonBreeders,
                               currentGroup = baboonBreeders[1:3],
                               kmat = pedWithGenotypeReport$kinship,
                               ped = pedWithGenotype,
                               ignore = NULL, minAge = 1, numGp = 1,
                               withKin = FALSE)
test_that("groupAddAssign forms the correct groups", {
  expect_equal(groupAddTest$group[[1]][1:3], c("16808", "32358", "32560"))
  expect_equal(groupAddTest$group[[2]][1:3], c("32743", "33088", "32798"))
  expect_null(groupAddTest$groupKin[[1]][1:3])
}
)
test_that("groupAddAssign forms the correct groups", {
  expect_equal(groupAssignTest$group[[1]][1:3], c("32732", "34832", "33893"))
  expect_equal(groupAssignTest$group[[2]][1:3], c("34163", "31937", "27647"))
  expect_null(groupAssignTest$groupKin[[1]][1:3])
}
)
test_that("groupAddAssign forms the correct groups with kinship matrices", {
  expect_equal(groupAddKTest$group[[1]][1:3], c("16808", "32358", "32560"))
  expect_equal(groupAddKTest$group[[2]][1:3], c("32743", "33088", "32798"))
  expect_equal(groupAddKTest$groupKin[[1]][1:3], c(0.5625, 0.0000, 0.0000))
}
)
test_that("groupAddAssign forms the correct groups with kinship matrices", {
  expect_equal(groupAssignKTest$group[[1]][1:3], c("32732", "34832", "33893"))
  expect_equal(groupAssignKTest$group[[2]][1:3], c("34163", "31937", "27647"))
  expect_equal(groupAssignKTest$groupKin[[1]][1:3], c(0.59375, 0.0000, 0.0000))
}
)
