context("addGenotype")
library(testthat)
library(stringi)
qc_ped_file <- stri_c("../../inst/extdata/",
                      "baboon_breeders_qc_ped.csv")

ped <- read.csv(qc_ped_file, header = TRUE, sep = ",",
                stringsAsFactors = FALSE, na.strings = c("", "NA"),
                check.names = FALSE)
genotype <- data.frame(id = ped$id[50 + 1:20],
                       first_name = stri_c("first_name", 1:20),
                       second_name = stri_c("second_name", 1:20),
                       stringsAsFactors = FALSE)

test_that("checkGenotypeFile allows correct dataframe", {
  expect_error(checkGenotypeFile(genotype), NA)
})
genotype <- data.frame(id = ped$id[50 + 1:20],
                       first_name = stri_c("first_name", 1:20),
                       second_name = stri_c("sec:ond_name", 1:20),
                       stringsAsFactors = FALSE)
test_that("checkGenotypeFile detects a bad dataframe", {
  expect_error(checkGenotypeFile(genotype))
})
genotype <- data.frame(id = ped$id[50 + 1:20],
                       first_name = stri_c("first_name", 1:20),
                       second_name = stri_c("second_name", 1:20),
                       stringsAsFactors = FALSE)
genotype$first_name[genotype$id == "1X1237"] <- "almost; ok"
test_that("checkGenotypeFile detects single error", {
  expect_error(checkGenotypeFile(genotype))
})
genotype <- data.frame(id = ped$id[50 + 1:20],
                       first_name = stri_c("first_name", 1:20),
                       second = stri_c("second_name", 1:20),
                       stringsAsFactors = FALSE)
test_that("checkGenotypeFile detects illegal column name", {
  expect_error(checkGenotypeFile(genotype))
})
