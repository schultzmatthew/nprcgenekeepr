#' Copyright(c) 2017-2020 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("getFocalAnimalPed")

test_that("getFocalAnimalPed recognizes no file and wrong file arguments", {
  expect_error(getFocalAnimalPed(), "\"fileName\" is missing, with no default")
  expect_error(suppressWarnings(getFocalAnimalPed(
    fileName = "breeding file.csv")), "cannot open the connection")
})
