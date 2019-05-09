context("makeGroupMembers")
library(testthat)
library(nprcmanager)
data("baboonBreeders")
data("pedWithGenotype")
data("pedWithGenotypeReport")
skip_if_not(exists("baboonBreeders"))
skip_if_not(exists("pedWithGenotype"))
skip_if_not(exists("pedWithGenotypeReport"))
suppressWarnings(set.seed(10, sample.kind = "Rounding"))
test_that(paste0("makeGroupMembers fails when more than one potential sire ",
                 "exists in currentGroup and harem == TRUE."), {
  currentGroups <- list(1)
  currentGroups[[1]] = baboonBreeders
  expect_error(makeGroupMembers(
    numGp = 3, currentGroups = currentGroups,
    candidates = baboonBreeders, ped = pedWithGenotype, harem = TRUE,
    minAge = 2), paste0("User selected to form harems with more than one ",
                        "male, There are 3 at least 2 years old in the ",
                        "current group."))
}
)

test_that(paste0("makeGroupMembers fails when the number of potential sires ",
                 "in candidates is less than the number of groups being ",
                 "formed, there is not a current group, and harem == TRUE."), {
  noSires <- removePotentialSires(ids = baboonBreeders, minAge = 2,
                                  ped = pedWithGenotype)
  currentGroups <- list(1)
  expect_error(makeGroupMembers(
    numGp = 3, currentGroups = character(0),
    candidates = noSires,
    ped = pedWithGenotype, harem = TRUE,
    minAge = 2), paste0("User selected to form harems in 3 groups with only ",
                        "0 males at least 2 years old in the list of ",
                        "candidates."))
}
)

test_that(paste0(
  "makeGroupMembers initializes groupMembers correctly when the number of ",
  "potential sires in currentGroup is one and the candidate animals contain ",
  "one or more potential sires and harem == TRUE."), {
    currentGroups <- list(1)
    noSires <- removePotentialSires(ids = baboonBreeders, minAge = 2,
                                    ped = pedWithGenotype)
    sires <- getPotentialSires(baboonBreeders, minAge = 2, ped = pedWithGenotype)
    currentGroups[[1]] <- c(noSires[1:10], sires[1])
    candidates <- c(noSires[11:length(noSires)], sires[-1])
    groupMembers <- makeGroupMembers(
      numGp = 1, currentGroups = currentGroups, candidates = candidates,
      ped = pedWithGenotype, harem = TRUE, minAge = 2)
    expect_true(length(groupMembers) == 1)
    expect_equal(groupMembers[[1]], currentGroups[[1]])
  }
)
test_that(paste0(
  "makeGroupMembers initializes groupMembers correctly when ",
  "harem == TRUE, there are no animals in the currentGroup and the candidate ",
  "animals contain numGp or more potential sires"), {
    currentGroups <- character(0)
    candidates <- baboonBreeders
    groupMembers <- makeGroupMembers(
      numGp = 3, currentGroups = currentGroups,
      candidates = candidates,
      ped = pedWithGenotype, harem = TRUE,
      minAge = 2)
    expect_true(length(groupMembers) == 3)
    expect_equal(class(groupMembers[[1]][1]), "character")
    expect_equal(length(groupMembers[[1]][1]), 1)
  }
)
test_that(paste0(
  "makeGroupMembers initializes groupMembers correctly when the number of ",
  "potential sires in currentGroup is zero and the candidate animals contain ",
  "one or more potential sires"), {
    currentGroups <- list(1)
    noSires <- removePotentialSires(ids = baboonBreeders, minAge = 2,
                                    ped = pedWithGenotype)
    sires <- getPotentialSires(baboonBreeders, minAge = 2, ped = pedWithGenotype)
    currentGroups[[1]] <- c(noSires[1:10])
    candidates <- c(noSires[11:length(noSires)], sires)
    groupMembers <- makeGroupMembers(
      numGp = 1, currentGroups = currentGroups,
      candidates = candidates,
      ped = pedWithGenotype, harem = TRUE,
      minAge = 2)
    expect_equal(length(groupMembers), 1)
    expect_equal(length(groupMembers[[1]]), 11)
    expect_equal(groupMembers[[1]][-1],
                 c("32358", "32560", "32650", "32743", "32771", "33088", "33164",
                   "26450", "31937", "32732"))
  }
)
test_that(paste0(
  "makeGroupMembers initializes groupMembers correctly when the number of ",
  "potential sires in currentGroup is 1 and the candidate animals contain ",
  "no potential sires"), {
    currentGroups <- list(1)
    noSires <- removePotentialSires(ids = baboonBreeders, minAge = 2,
                                    ped = pedWithGenotype)
    sires <- getPotentialSires(baboonBreeders, minAge = 2, ped = pedWithGenotype)
    currentGroups[[1]] <- c(noSires[1:10], sires[1])
    candidates <- noSires[11:length(noSires)]
    groupMembers <- makeGroupMembers(
      numGp = 1, currentGroups = currentGroups,
      candidates = candidates,
      ped = pedWithGenotype, harem = TRUE,
      minAge = 2)
    expect_equal(length(groupMembers), 1)
    expect_equal(length(groupMembers[[1]]), 11)
    expect_equal(groupMembers[[1]], c(noSires[1:10], sires[1]))
  }
)

