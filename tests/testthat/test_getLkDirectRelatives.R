context("getLkDirectRelatives")

test_that("getLkDirectRelatives throws an error with no LabKey session connection", {
  expect_error(expect_warning(getLkDirectRelatives(),
                              "The file should be named: ~/.nprcmanager_config."),
               "attempt to set an attribute on NULL")
})
