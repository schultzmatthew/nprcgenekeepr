context("groupAddAssign")
library(testthat)
library(nprcmanager)
data("baboonBreeders")
data("pedWithGenotype")
data("pedWithGenotypeReport")
skip_if_not(object.size(baboonBreeders) == 1664)
skip_if_not(object.size(pedWithGenotype) == 58000)
skip_if_not(object.size(pedWithGenotypeReport) == 713216)
set.seed(10)
groupAddTest <- groupAddAssign(
  candidates = baboonBreeders,
  currentGroup = baboonBreeders[1:3],
  kmat = pedWithGenotypeReport$kinship,
  ped = pedWithGenotype,
  ignore = NULL, minAge = 1, numGp = 1,
  withKin = FALSE)
test_that("groupAddAssign forms the correct groups", {
  #expect_equal(length(groupAddTest$group[[1]]), 11)
  expect_equal(length(groupAddTest$group[[2]]), 10)
  expect_null(groupAddTest$groupKin[[1]])
}
)
set.seed(10)
groupAssignTest <- groupAddAssign(candidates = baboonBreeders,
                                  currentGroup = NULL,
                                  kmat = pedWithGenotypeReport$kinship,
                                  ped = pedWithGenotype,
                                  ignore = NULL, minAge = 1, numGp = 2,
                                  withKin = FALSE)
test_that("groupAddAssign forms the correct groups", {
  expect_equal(length(groupAssignTest$group[[1]]), 9)
  expect_equal(length(groupAssignTest$group[[2]]), 10)
  expect_null(groupAssignTest$groupKin[[1]])
}
)
set.seed(10)
groupAddKTest <- groupAddAssign(candidates = baboonBreeders,
                                currentGroup = baboonBreeders[1:3],
                                kmat = pedWithGenotypeReport$kinship,
                                ped = pedWithGenotype,
                                ignore = NULL, minAge = 1, numGp = 1,
                                withKin = TRUE)
test_that("groupAddAssign forms the correct groups with kinship matrices", {
  #expect_equal(length(groupAddKTest$group[[1]]), 11)
  expect_equal(length(groupAddKTest$group[[2]]), 10)
  #expect_equal(length(groupAddKTest$groupKin[[1]]), 121)
}
)
set.seed(10)
groupAssignKTest <- groupAddAssign(candidates = baboonBreeders,
                                   currentGroup = NULL,
                                   kmat = pedWithGenotypeReport$kinship,
                                   ped = pedWithGenotype,
                                   ignore = NULL, minAge = 1, numGp = 2,
                                   withKin = TRUE)
test_that("groupAddAssign forms the correct groups with kinship matrices", {
  expect_equal(length(groupAssignKTest$group[[1]]), 9)
  expect_equal(length(groupAssignKTest$group[[2]]), 10)
  expect_equal(length(groupAssignKTest$groupKin[[1]]), 81)
}
)
