context("summary.nprcmanagErr")
library(testthat)
library(lubridate)
library(stringi)

set_seed(10)
pedOne <- data.frame(ego_id = c("s1", "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
                     `si re` = c(NA, NA, NA, NA, "s1", "s1", "s2", "s2"),
                     dam_id = c(NA, NA, NA, NA, "d1", "d2", "d2", "d2"),
                     sex = c("F", "M", "M", "F", "F", "F", "F", "M"),
                     birth_date = mdy(
                       paste0(sample(1:12, 8, replace = TRUE), "-",
                              sample(1:28, 8, replace = TRUE), "-",
                              sample(seq(0, 15, by = 3), 8, replace = TRUE) +
                                2000)),
                     stringsAsFactors = FALSE, check.names = FALSE)
test_that("summary.qcStudbook provides expected classes of output", {
  expect_equal(class(summary(qcStudbook(pedOne, reportErrors = TRUE))$txt), "character")
  expect_equal(class(summary(qcStudbook(pedOne, reportErrors = TRUE))$sp), "data.frame")
})
test_that("summary.qcStudbook provides expected output", {
  expect_equal(length(summary(qcStudbook(pedOne, reportErrors = TRUE))$txt), 1)
  expect_equal(nrow(summary(qcStudbook(pedOne, reportErrors = TRUE))$sp), 3)
  expect_equal(stri_count_regex(
    summary(qcStudbook(pedOne, reportChanges = TRUE, reportErrors = TRUE))$txt, "\\n"), 9)
  pedTwo <- pedOne
  pedTwo$sex <- NULL
  expect_true(stri_detect_regex(
    summary(qcStudbook(pedTwo, reportChanges = TRUE, reportErrors = TRUE))$txt,
    pattern = "sex.\\n The required columns are: id, sire, dam, sex, and birth"))
  pedTwo <- pedOne
  pedTwo$birth_date <- "badDate"
  expect_true(stri_detect_regex(
    summary(qcStudbook(pedTwo, reportChanges = TRUE, reportErrors = TRUE))$txt,
    pattern = stri_c("There are 8 rows having an invalid date. ",
                     "The first five records having bad dates are ",
                     "on rows 1, 2, 3, 4, and 5.")))
})
test_that("qcStudbook identifies individual bad dates in date columns", {
  birth <- as.character(pedOne$birth_date, format = "%Y-%m-%d")
  birth[5] <- "04-02-2015"
  birth[6] <- "03-17-2009"
  pedEight <- pedOne
  pedEight$birth_date <- NULL
  pedEight$birth <- birth
  ped8 <- qcStudbook(pedEight, minParentAge = NULL, reportErrors = TRUE)
  summary(ped8)
  expect_true(stri_detect_fixed(summary(ped8)$txt, "rows having an invalid date are: 5 and 6"))
})
