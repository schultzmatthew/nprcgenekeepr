context("getPyramidAgeDist")
library(testthat)
library(lubridate)

data("baboonPed")
ped <- baboonPed[ , c("id", "sire", "dam", "sex", "birth", "exit")]
ped <- getPyramidAgeDist(ped)
test_that("getPyramidAgeDist classifies and ages animals correctly", {
  expect_equal(as.numeric(table(ped$status)[[1]]), 46)
  expect_true(ped$age[ped$id == "10173"] > 19.0 &
                ped$age[ped$id == "10173"] < 20.0)
})
ped <- getPyramidAgeDist()
test_that("getPyramidAgeDist gets baboonPed by default", {
  expect_equal(as.numeric(table(ped$status)[[1]]), 46)
  expect_true(ped$age[ped$id == "10173"] > 19.0 &
                ped$age[ped$id == "10173"] < 20.0)
})
ped <- baboonPed[ , c("id", "sire", "dam", "sex", "birth", "exit")]
charDatePed <- ped
charDatePed$birth <- format(charDatePed$birth, format = "%Y-%m-%d")
ped <- getPyramidAgeDist(charDatePed)
test_that("getPyramidAgeDist converts character based birth date", {
  expect_equal(as.numeric(table(ped$status)[[1]]), 46)
  expect_true(ped$age[ped$id == "10173"] > 19.0 &
                ped$age[ped$id == "10173"] < 20.0)
})
ped <- baboonPed[ , c("id", "sire", "dam", "sex", "birth", "exit")]
charDatePed <- ped
charDatePed$exit <- format(charDatePed$exit, format = "%Y-%m-%d")
charDatePed$exit[176] <- "9999999999"
ped <- getPyramidAgeDist(charDatePed)
test_that(stri_c("getPyramidAgeDist converts 9999999999 exit_date to NA and ",
                 "makes status DECEASED"), {
  expect_equal(ped$status[ped$id == charDatePed$id[176]], "DECEASED")
  expect_true(is.na(ped$exit[ped$id == charDatePed$id[176]]))
})

