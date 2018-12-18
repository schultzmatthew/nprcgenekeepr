context("getLkDirectAncestors")

test_that("getLkDirectAncestors throws an error with no LabKey session connection", {
  expect_error(expect_warning(getLkDirectAncestors(),
                              "The file should be named: ~/.nprcmanager_config."),
               "attempt to set an attribute on NULL")
})
