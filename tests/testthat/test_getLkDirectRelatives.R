context("getLkDirectRelatives")

test_that("getLkDirectRelatives throws an error with no LabKey session connection", {
  expect_warning(getLkDirectRelatives(),
                              "The file should be named:")
})
