context("addUIds")
library(testthat)
library(stringi)
ped_file <- system.file("extdata", "baboon_breeders_ped.csv",
                           package = "nprcmanager")
ped <- read.csv(ped_file, header = TRUE, sep = ",",
                stringsAsFactors = FALSE, na.strings = c("", "NA"),
                check.names = FALSE)
names(ped) <- c("id", "dam", "sire", "sex", "birth")
test_that("addUIds modifies the correct IDs in the right way", {
  newPed <- addUIds(ped)
  expect_equal(newPed$sire[newPed$id == "7150"], "U0002")
  expect_true(is.na(newPed$sire[newPed$id == "6704"]))
  expect_equal(newPed$sire[newPed$id == "9461"], "U0010")
})
