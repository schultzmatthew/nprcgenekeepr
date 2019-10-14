#' Copyright(c) 2017-2019 R. Mark Sharp
#' This file is part of nprcmanager
context("getGenotypes")
## This is identical to getPedigree and needs to be strengthened. However,
## function does not do any data quality checks at this time.

test_that("getGenotypes recognizes no file and wrong file arguments", {
  expect_error(getGenotypes(), "\"fileName\" is missing, with no default")
  expect_error(suppressWarnings(getBreederPed(fileName = "breeding file.csv")),
               "cannot open the connection")
})
test_that("getGenotypes recognizes and opens Excel files.", {
  pedExcel <- suppressWarnings(getGenotypes(fileName = system.file("testdata", "qcPed.xlsx",
                                                                    package="nprcmanager")))
  expect_equal(nrow(pedExcel), 280)
})
test_that(paste0("getGenotypes recognizes and opens CSV files with default ",
                 "comma separator."), {
                   pedCsv <- getGenotypes(fileName = system.file("testdata", "qcPed.csv",
                                                                  package="nprcmanager"))
                   expect_equal(nrow(pedCsv), 280)
                 })
test_that(paste0("getGenotypes recognizes and opens CSV files with specified ",
                 "comma separator."), {
                   pedCsv2 <- getGenotypes(fileName = system.file("testdata", "qcPed.csv",
                                                                   package="nprcmanager"),
                                            sep = ",")
                   expect_equal(nrow(pedCsv2), 280)
                 })
test_that(paste0("getGenotypes recognizes and opens .txt files with specified ",
                 "tab separator."), {
                   pedTxt <- getGenotypes(fileName = system.file("testdata", "qcPed.txt",
                                                                  package="nprcmanager"),
                                           sep = "\t")
                   expect_equal(nrow(pedTxt), 280)
                 })
