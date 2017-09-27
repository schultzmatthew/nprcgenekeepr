context("convertRelationships")
library(testthat)
data("lacy1989Ped")
ped <- lacy1989Ped
kmat <- kinship(ped$id, ped$sire, ped$dam, ped$gen, sparse = FALSE)
ids <- c("A", "B", "D", "E", "F", "G")
relIds <- convertRelationships(kmat, ped, ids)
rel <- convertRelationships(kmat, ped, NULL)

test_that("convertRelationships makes correct transformations", {
  expect_equal(relIds$id1[relIds$id1 %in% rel$id1], relIds$id1)
  expect_true(all(rel$id1[rel$id1 %in% relIds$id1] %in% relIds$id1))
  expect_equal(rel$kinship[rel$id1 == "A" & rel$id2 == "D"], 0.25)
  expect_equal(rel$relation[rel$id1 == "D" & rel$id2 == "G"],
               "Parent-Offspring")
})
