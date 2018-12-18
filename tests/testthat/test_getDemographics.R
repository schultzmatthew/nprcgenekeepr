context("getDemographics")

test_that("getDemographics throws an error with no LabKey session connection", {
  expect_error(expect_warning(getDemographics(),
                              "The file should be named: ~/.nprcmanager_config."))
})
