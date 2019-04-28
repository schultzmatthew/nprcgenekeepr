context("fillGroupMembersWithSexRatio")
library(stringi)

data("examplePedigree")
data("pedWithGenotype")
data("pedWithGenotypeReport")
skip_if_not(exists("baboonBreeders"))
skip_if_not(exists("pedWithGenotype"))
skip_if_not(exists("pedWithGenotypeReport"))
set.seed(10)
currentGroups <- list(1)
currentGroups[[1]] <- examplePedigree[1:3]
candidates <- examplePedigree
currentGroups <- currentGroups
kmat <- pedWithGenotypeReport$kinship
ped <- pedWithGenotype
threshold <- 0.015625
kmat <- filterKinMatrix(union(candidates, unlist(currentGroups)), kmat)
kin <- nprcmanager:::getAnimalsWithHighKinship(kmat, ped, threshold, currentGroups, ignore,
                                 minAge)
# Filtering out candidates related to current group members
conflicts <- unique(c(unlist(kin[unlist(currentGroups)]), unlist(currentGroups)))
candidates <- setdiff(candidates, conflicts)


kin <- nprcmanager:::addAnimalsWithNoRelative(kin, candidates)

ignore <- NULL
minAge <- 1
numGp <- 1
harem <- FALSE
sexRatio <- 0
withKin <- FALSE
groupMembers <- makeGroupMembers(numGp, currentGroups, candidates, ped, harem = FALSE,
                                 minAge = 1)
groupMembersStart <- groupMembers
grpNum <- nprcmanager:::makeGrpNum(numGp)

test_that("fillGroupMembersWithSexRatio adds animals in the specified sex ratio", {
  expect_equal(groupMembers[[1]], c("16808", "32358", "32560"))
  for (i in 1:20) {
    groupMembers <- fillGroupMembersWithSexRatio(
      candidates, groupMembers, grpNum, kin, ped, minAge, numGp, sexRatio = 1)
  }
  expect_equal(calculateSexRatio(groupMembers[[1]], ped), 1.0,
               tolerance = .1, scale = 1)
  groupMembers <- groupMembersStart

  for (i in 1:40) {
    groupMembers <- fillGroupMembersWithSexRatio(
      candidates, groupMembers, grpNum, kin, ped, minAge, numGp, sexRatio = 0.5)
  }
  expect_equal(calculateSexRatio(groupMembers[[1]], ped), 1.0,
               tolerance = .1, scale = 1)
  groupMembersStart <- groupMembers

  })
