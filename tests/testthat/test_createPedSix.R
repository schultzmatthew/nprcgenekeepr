context("createPedSix")
library(testthat)
pedSix <- createPedSix()
test_that("createPedSix makes the right pedigree", {
  expect_equal(nrow(pedSix), 8)
  expect_equal(ncol(pedSix), 7)
  expect_equal(names(pedSix)[1], "Ego Id")
})
