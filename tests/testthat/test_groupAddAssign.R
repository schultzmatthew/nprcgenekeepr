context("groupAddAssign")
library(testthat)
library(nprcmanager)
data("baboonBreeders")
data("pedWithGenotype")
data("pedWithGenotypeReport")
skip_if_not(exists("baboonBreeders"))
skip_if_not(exists("pedWithGenotype"))
skip_if_not(exists("pedWithGenotypeReport"))
set.seed(10)
currentGroups <- list(1)
currentGroups[[1]] <- baboonBreeders[1:3]
groupAddTest <- groupAddAssign(
  candidates = baboonBreeders,
  currentGroups = currentGroups,
  kmat = pedWithGenotypeReport$kinship,
  ped = pedWithGenotype,
  ignore = NULL, minAge = 1, numGp = 1,
  harem = FALSE, sexRatio = 0, withKin = FALSE)
test_that("groupAddAssign forms the correct groups", {
  #expect_equal(length(groupAddTest$group[[1]]), 11)
  expect_equal(length(groupAddTest$group[[2]]), 10)
  expect_null(groupAddTest$groupKin[[1]])
}
)
set.seed(10)
groupAssignTest <- groupAddAssign(candidates = baboonBreeders,
                                  currentGroups = character(0),
                                  kmat = pedWithGenotypeReport$kinship,
                                  ped = pedWithGenotype,
                                  ignore = NULL, minAge = 1, numGp = 2,
                                  harem = FALSE, sexRatio = 0, withKin = FALSE)
test_that("groupAddAssign forms the correct groups", {
  expect_equal(length(groupAssignTest$group[[1]]), 9)
  expect_equal(length(groupAssignTest$group[[2]]), 10)
  expect_null(groupAssignTest$groupKin[[1]])
}
)
set.seed(10)
currentGroups <- list(1)
currentGroups[[1]] <- baboonBreeders[1:3]
groupAddKTest <- groupAddAssign(candidates = baboonBreeders,
                                currentGroups = currentGroups,
                                kmat = pedWithGenotypeReport$kinship,
                                ped = pedWithGenotype,
                                ignore = NULL, minAge = 1, numGp = 1,
                                harem = FALSE, sexRatio = 0, withKin = TRUE)
test_that("groupAddAssign forms the correct groups with kinship matrices", {
  #expect_equal(length(groupAddKTest$group[[1]]), 11)
  expect_equal(length(groupAddKTest$group[[2]]), 10)
  #expect_equal(length(groupAddKTest$groupKin[[1]]), 121)
}
)
set.seed(10)
groupAssignKTest <- groupAddAssign(candidates = baboonBreeders,
                                   currentGroups = character(0),
                                   kmat = pedWithGenotypeReport$kinship,
                                   ped = pedWithGenotype,
                                   ignore = NULL, minAge = 1, numGp = 2,
                                   harem = FALSE, sexRatio = 0, withKin = TRUE)
test_that("groupAddAssign forms the correct groups with kinship matrices", {
  expect_equal(length(groupAssignKTest$group[[1]]), 9)
  expect_equal(length(groupAssignKTest$group[[2]]), 10)
  expect_equal(length(groupAssignKTest$groupKin[[1]]), 81)
}
)
set.seed(10)
noSires <- removePotentialSires(baboonBreeders, minAge = 2,
                                            pedWithGenotype)
sires <- getPotentialSires(baboonBreeders, minAge = 2, pedWithGenotype)

test_that(paste0("groupAddAssign fails when no potential sires exist for ",
                 "harem creation"), {
  expect_error(groupAddAssign(candidates = noSires, currentGroups = character(0),
                              kmat = pedWithGenotypeReport$kinship,
                              ped = pedWithGenotype,
                              ignore = NULL, minAge = 1, numGp = 2,
                              harem = TRUE, sexRatio = 0, withKin = TRUE))
}
)
test_that(paste0("groupAddAssign when there are multiple potential sires in ",
                 "the candidates during harem creation"), {
  group <- groupAddAssign(candidates = baboonBreeders, currentGroups = character(0),
                          kmat = pedWithGenotypeReport$kinship,
                          ped = pedWithGenotype,
                          ignore = NULL, minAge = 1, numGp = 2,
                          harem = TRUE, sexRatio = 0, withKin = TRUE)
  expect_true(length(group) == 3)
  expect_equal(sum(seq_along(group[[1]][[3]])[group[[1]][[3]] %in% sires]), 0)
  expect_equal(sum(seq_along(group[[1]][[3]])[group[[1]][[2]] %in% sires]), 1)
                 }
)

