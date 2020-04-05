#' Copyright(c) 2017-2020 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("dataframe2string")
library(testthat)
library(lubridate)
library(stringi)

set_seed(10)
pedOne <- data.frame(
  ego_id = c("s1", "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
  `si re` = c(NA, NA, NA, NA, "s1", "s1", "s2", "s2"),
  dam_id = c(NA, NA, NA, NA, "d1", "d2", "d2", "d2"),
  sex = c("F", "M", "M", "F", "F", "F", "F", "M"),
  birth_date = mdy(paste0(sample(1:12, 8, replace = TRUE), "-",
                          sample(1:28, 8, replace = TRUE), "-",
                          sample(seq(0, 15, by = 3), 8, replace = TRUE) +
                            2000)),
  stringsAsFactors = FALSE, check.names = FALSE)
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
