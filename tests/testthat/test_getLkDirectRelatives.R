context("getLkDirectRelatives")

test_that("getLkDirectRelatives throws an error with no LabKey session connection", {
  expect_error(getLkDirectRelatives(), "attempt to set an attribute on NULL")
})
