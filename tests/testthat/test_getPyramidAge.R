context("getPyramidAge")
library(testthat)
data("baboonPed")
ped <- baboonPed[ , c("id", "sire", "dam", "sex", "birth", "exit")]
ped <- getPyramidAgeDist(ped)
test_that("getPyramidAge classifies and ages animals correctly", {
  expect_equal(as.numeric(table(ped$status)[[1]]), 46)
  expect_true(ped$age[ped$id == "10173"] > 19.0 &
                ped$age[ped$id == "10173"] < 20.0)
})
