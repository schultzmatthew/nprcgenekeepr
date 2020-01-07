#' Copyright(c) 2017-2019 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("getLkDirectAncestors")

test_that("getLkDirectAncestors throws an error with no nprcgenekeepr
          configuration file", {
  expect_error(expect_warning(getLkDirectAncestors(),
                              "The file should be named: ~/.nprcgenekeepr_config."),
               "attempt to set an attribute on NULL")
})
