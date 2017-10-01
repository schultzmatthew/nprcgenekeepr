context("groupAddAssign")
library(testthat)
data("baboonBreeders")
data("pedWithGenotype")
data("pedWithGenotypeReport")
set.seed(10)
groupAssignTest <- groupAddAssign(candidates = baboonBreeders,
                                     currentGroup = NULL,
                                     kmat = pedWithGenotypeReport$kinship,
                                     ped = pedWithGenotype,
                                     ignore = NULL, minAge = 1, numGp = 2,
                                     withKin = TRUE)
groupAddTest <- groupAddAssign(candidates = baboonBreeders,
                                  currentGroup = baboonBreeders[1:3],
                                  kmat = pedWithGenotypeReport$kinship,
                                  ped = pedWithGenotype,
                                  ignore = NULL, minAge = 1, numGp = 1,
                                  withKin = TRUE)
test_that("groupAddAssign forms the correct groups", {
  expect_equal(groupAddTest$group[[1]][1:3], c("16808", "32358", "32560"))
  expect_equal(groupAddTest$group[[2]][1:3], c("32771", "27647", "32798"))
  expect_equal(groupAddTest$groupKin[[1]][1:3], c("16808", "32358", "32560"))
}
)
