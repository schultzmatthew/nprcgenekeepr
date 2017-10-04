context("createPedTree")
library(testthat)
ped <- structure(
  list(id = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
              "N", "O", "P", "Q"),
       sire = c("Q", NA, "A", "A", NA, "D", "D", "A", "A", NA, NA, "C", "A",
                NA, NA, "M", NA),
       dam = c(NA, NA, "B", "B", NA, "E", "E", "B", "J", NA, NA, "K", "N",
               NA, NA, "O", NA),
       sex = c("M", "F", "M", "M", "F", "F", "F", "M", "F", "F", "F", "F", "M",
               "F", "F", "F", "M"),
       gen = c(1, 1, 2, 2, 1, 3, 3, 2, 2, 1, 1, 2, 1, 1, 2, 3, 0),
       population = rep(TRUE, 17)),
  .Names = c("id", "sire", "dam", "sex", "gen", "population"),
  row.names = c(NA, -17L),
  class = "data.frame")
pedTree <- createPedTree(ped)
test_that("createPedTree correctly breaks up pedigrees", {
  for (id in ped$id) {
    expect_equal(pedTree[[id]][["sire"]], ped$sire[ped$id == id])
    expect_equal(pedTree[[id]][["dam"]], ped$dam[ped$id == id])
  }
})
