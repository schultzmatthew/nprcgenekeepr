context("convertRelationships")
library(testthat)
ped <- structure(
  list(id = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"),
       sire = c(NA, NA, "A", "A", NA, "D", "D", "A", "A", NA, NA, "C"),
       dam = c(NA, NA, "B", "B", NA, "E", "E", "B", "J", NA, NA, "K"),
       sex = c("M", "F", "M", "M", "F", "F", "F", "M", "F", "F", "F", "F"),
       gen = c(0, 0, 1, 1, 0, 2, 2, 1, 1, 0, 0, 1),
       population = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
                      TRUE, TRUE, TRUE)),
  .Names = c("id", "sire", "dam", "sex", "gen", "population"),
  row.names = c(NA, -12L),
  class = "data.frame")
kmat <- kinship(ped$id, ped$sire, ped$dam, ped$gen, sparse = FALSE)
ids <- c("A", "B", "D", "E", "F", "G", "I", "L")
relIds <- convertRelationships(kmat, ped, ids)
rel <- convertRelationships(kmat, ped, NULL)

test_that("convertRelationships makes correct transformations", {
  expect_equal(relIds$id1[relIds$id1 %in% rel$id1], relIds$id1)
  expect_true(all(rel$id1[rel$id1 %in% relIds$id1] %in% relIds$id1))
  expect_equal(rel$kinship[rel$id1 == "A" & rel$id2 == "D"], 0.25)
  expect_equal(rel$relation[rel$id1 == "D" & rel$id2 == "G"],
               "Parent-Offspring")
  expect_equal(rel$relation[rel$id1 == "C" & rel$id2 == "I"],
               "Half-Siblings")
  expect_equal(rel$relation[rel$id1 == "C" & rel$id2 == "J"],
               "No Relation")
  expect_equal(rel$relation[rel$id1 == "C" & rel$id2 == "C"],
               "Self")
  expect_equal(rel$relation[rel$id1 == "C" & rel$id2 == "G"],
               "Full-Avuncular")
  expect_equal(relIds$relation[relIds$id1 == "A" & relIds$id2 == "B"],
               "No Relation")
  expect_equal(relIds$relation[relIds$id1 == "A" & relIds$id2 == "F"],
               "Grandparent-Grandchild")
  expect_equal(relIds$relation[relIds$id1 == "F" & relIds$id2 == "G"],
               "Full-Siblings")
  expect_equal(relIds$relation[relIds$id1 == "F" & relIds$id2 == "I"],
               "Avuncular - Other")
  expect_equal(relIds$relation[relIds$id1 == "F" & relIds$id2 == "L"],
               "Full-Cousins")
})
