context("createPedOne")
library(testthat)
pedOne <- createPedOne(savePed = FALSE)
test_that("createPedOne makes the right pedigree", {
  expect_equal(nrow(pedOne), 8)
  expect_equal(ncol(pedOne), 5)
  expect_equal(names(pedOne)[1], "ego_id")
})

