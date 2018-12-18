context("getDemographics")

test_that("getDemographics throws an error with no LabKey session connection", {
  expect_error(getDemographics(), "Could not resolve host:")
})
