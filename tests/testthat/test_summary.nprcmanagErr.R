context("summary.nprcmangErr")
library(testthat)
library(lubridate)
library(stringi)

set.seed(10)
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
  expect_equal(class(summary(qcStudbook(pedOne, errors = TRUE))$txt), "character")
  expect_equal(class(summary(qcStudbook(pedOne, errors = TRUE))$sp), "data.frame")
})
test_that("summary.qcStudbook provides expected output", {
  expect_equal(length(summary(qcStudbook(pedOne, errors = TRUE))$txt), 1)
  expect_equal(nrow(summary(qcStudbook(pedOne, errors = TRUE))$sp), 3)
  expect_equal(stri_count_regex(summary(qcStudbook(pedOne, errors = TRUE))$txt, "\\n"), 8)
})
