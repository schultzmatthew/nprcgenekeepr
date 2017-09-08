context("addGenotype")
library(testthat)
library(stringi)
qc_ped_file <- stri_c("../../inst/extdata/",
                      "baboon_breeders_qc_ped.csv")

ped <- read.csv(qc_ped_file, header = TRUE, sep = ",",
                     stringsAsFactors = FALSE, na.strings = c("", "NA"),
                     check.names = FALSE)
genotype <- data.frame(id = ped$id[50 + 1:20],
                       first_name = stri_c("first", 1:20),
                       second_name = stri_c("second", 1:20),
                       stringsAsFactors = FALSE)

test_that("addGenotype forms correct dataframe", {
  newPed <- addGenotype(ped, genotype)
  expect_equal(as.character(newPed$first[newPed$id == "1X0872"]), "10001")
  expect_equal(as.character(newPed$second[newPed$id == "1X0872"]), "10021")
  expect_equal(as.character(newPed$first[newPed$id == "1X0945"]), "10012")
  expect_equal(as.character(newPed$second[newPed$id == "1X0945"]), "10032")
})
