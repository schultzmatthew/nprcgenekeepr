context("obfuscateId")
library(testthat)

test_that("obfuscatePed creates correctly obfuscated pedigree", {
  pedSix <- qcStudbook(nprcmanager::pedSix)
  ped <- obfuscatePed(pedSix, size = 3, maxDelta = 20)
  expect_equal(nrow(ped), nrow(pedSix))
  expect_equal(ncol(ped), ncol(pedSix))
  expect_equal(ped$id[1], ped$dam[7])
  expect_equal(ped$id[1], ped$dam[7])
  expect_true(all(ped$id[2] == ped$dam[c(8, 11, 12)]))
  expect_true(max(abs(pedSix$birth[!is.na(pedSix$birth)] - ped$birth[!is.na(ped$birth)])) <= 20)
})

