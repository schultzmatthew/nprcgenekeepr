context("qcStudbook")
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
pedTwo <- data.frame(ego_id = c("UNKNOWN", "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
                     `si re` = c(NA, NA, NA, NA, "UNKNOWN", "s1", "s2", "s2"),
                     dam_id = c(NA, NA, NA, NA, "d1", "d2", "UNKNOWN", "d2"),
                     sex = c("F", "M", "M", "F", "F", "F", "F", "M"),
                     birth_date = mdy(
                       paste0(sample(1:12, 8, replace = TRUE), "-",
                              sample(1:28, 8, replace = TRUE), "-",
                              sample(seq(0, 15, by = 3), 8, replace = TRUE) +
                                2000)),
                     stringsAsFactors = FALSE, check.names = FALSE)
pedThree <- data.frame(id = c("s1", "d1", "s2", NA, "o1", "o2", "o3", "o4"),
                     sire = c(NA, NA, NA, NA, "s1", "s1", "s2", "s2"),
                     dam = c(NA, NA, NA, NA, "d1", "d2", "d2", "d2"),
                     sex = c("M", "F", "M", "F", "F", "F", "F", "M"),
                     stringsAsFactors = FALSE)
pedThree <- pedThree[!is.na(pedThree$id), ]
pedFour <- data.frame(id = c("s1", NA, NA, "d2", "o1", "o2", "o3", "o4"),
                     sire = c(NA, NA, NA, NA, "s1", "s1", "s2", "s2"),
                     dam = c(NA, NA, NA, NA, "d1", "d2", "d2", "d2"),
                     sex = c("M", "F", "M", "F", "F", "F", "F", "M"),
                     stringsAsFactors = FALSE)
pedFour <- pedFour[!is.na(pedFour$id), ]
test_that("qcStudbook detects errors in column names", {
  expect_error(qcStudbook(pedOne))
  expect_error(qcStudbook(pedOne[ , -1], minParentAge = NULL))
})
test_that("qcStudbook detects missing required column names", {
  expect_error(qcStudbook(pedOne[ , -3]))
})
test_that("qcStudbook corrects column names", {
  newPedOne <- qcStudbook(pedOne, minParentAge = NULL)
  expect_equal(names(newPedOne), c("id", "sire", "dam", "sex", "gen", "birth",
                                   "exit", "age"))
  expect_equal(as.character(newPedOne$sex[newPedOne$id == "d1"]), "F")
  expect_equal(as.character(newPedOne$sex[newPedOne$id == "s1"]), "M")
})
test_that("qcStudbook corrects use of 'UNKNOWN' in 'id', 'sire' and 'dam' IDS",
          {
  newPedTwo <- qcStudbook(pedTwo, minParentAge = NULL)
  expect_equal(newPedTwo$sire[newPedTwo$id == "o1"], "U0001")
  expect_equal(newPedTwo$dam[newPedTwo$id == "o3"], "U0002")
  expect_equal(newPedTwo$sire[newPedTwo$id == "o1"], "U0001")
  expect_true(is.na(newPedTwo$sire[newPedTwo$id == "U0001"]))
          })
