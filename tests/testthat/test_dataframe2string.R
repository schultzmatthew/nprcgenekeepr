#' Copyright(c) 2017-2020 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("dataframe2string")
library(testthat)
library(stringi)

pedOne <- nprcgenekeepr::pedOne
errorLst <- qcStudbook(pedOne, reportErrors = TRUE)
text <- summary(errorLst)
lines <- dataframe2string(text$sp, row.names = FALSE, digits = 2)
test_that("dataframe2string forms single character vector from dataframe", {
  expect_true(
    stri_detect_regex(dataframe2string(text$sp, row.names = FALSE,
                                       digits = 2), "8.67  -6.5 \\n"))
  expect_true(stri_detect_regex(dataframe2string(text$sp, row.names = TRUE,
                                                 digits = 2), "Age\\n2d2"))
  expect_true(stri_detect_regex(dataframe2string(text$sp, row.names = FALSE,
                                                 digits = 2), "Age\\nd2"))
  expect_true(stri_detect_regex(dataframe2string(text$sp[0, ], row.names = FALSE,
                                                 digits = 2),
                                "(or 0-length row.names)"))
  expect_true(stri_detect_regex(dataframe2string(text$sp[ , 0], row.names = FALSE,
                                                 digits = 2),
                                "data frame with 0 columns and 3 rows"))
  expect_true(stri_detect_regex(dataframe2string(
    data.frame(text$sp, row.names = NULL), row.names = FALSE,
    digits = 2), "Age\\nd2"))
})
