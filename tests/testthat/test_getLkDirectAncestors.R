context("getLkDirectAncestors")

test_that("getLkDirectAncestors throws an error with no LabKey session connection", {
  expect_error(getLkDirectAncestors(), "attempt to set an attribute on NULL")
})
