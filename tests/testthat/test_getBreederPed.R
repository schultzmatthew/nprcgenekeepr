#' Copyright(c) 2017-2019 R. Mark Sharp
#' This file is part of nprcmanager
context("getBreederPed")

test_that("getBreederPed recognizes no file and wrong file arguments", {
  expect_error(getBreederPed(), "\"fileName\" is missing, with no default")
  expect_error(suppressWarnings(getBreederPed(fileName = "breeding file.csv")),
               "cannot open the connection")
})
