context("obfuscateId")
library(testthat)
library(stringi)

set_seed(10)
test_that("obfuscateId creates new ID with expected size", {
  id <- c("abc123", "george", "autumn")
  obfuscatedId <- obfuscateId(id, 6)
  expect_equal(length(obfuscatedId), 3)
  expect_true(all(stri_length(obfuscatedId) == 6))
  expect_true(length(id) == length(unique(obfuscatedId)))
})
test_that("obfuscateId does not create duplicates", { # this test is weak
  id <- stri_c(1:10000)
  obfuscatedId <- obfuscateId(id, 5)
  expect_equal(length(obfuscatedId), 10000)
  expect_true(all(stri_length(obfuscatedId) == 5))
  expect_true(length(id) == length(unique(obfuscatedId)))
})
test_that("obfuscateId fails when duplicates cannot be avoided", { # this test is weak
  id <- stri_c(1:10000)
  expect_error(obfuscateId(id, size = 2))
})
