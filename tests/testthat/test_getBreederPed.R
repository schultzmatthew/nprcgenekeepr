context("getBreederPed")

test_that("getBreederPed recognizes no file and wrong file arguments", {
  expect_error(getBreederPed(), "\"fileName\" is missing, with no default")
  expect_error(suppressWarnings(getBreederPed(fileName = "breeding file.csv")),
               "cannot open the connection")
})
