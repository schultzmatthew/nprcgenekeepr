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
                     status = c("A", "alive", "Alive", "1", "S", "Sale", "sold",
                                 "shipped"),
                     ancestry = c("china", "india", "hybridized", NA, "human",
                                   "gorilla", "human", "gorilla"),
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
  expect_error(suppressWarnings(qcStudbook(pedOne)))
  expect_error(suppressWarnings(qcStudbook(pedOne[ , -1], minParentAge = NULL)))
})
test_that("qcStudbook detects missing required column names", {
  expect_error(suppressWarnings(qcStudbook(pedOne[ , -3])))
})
test_that("qcStudbook corrects column names", {
  newPedOne <- suppressWarnings(qcStudbook(pedOne, minParentAge = NULL))
  expect_equal(names(newPedOne), c("id", "sire", "dam", "sex", "gen", "birth",
                                   "exit", "age"))
  expect_equal(as.character(newPedOne$sex[newPedOne$id == "d1"]), "F")
  expect_equal(as.character(newPedOne$sex[newPedOne$id == "s1"]), "M")
})
test_that("qcStudbook corrects use of 'UNKNOWN' in 'id', 'sire' and 'dam' IDS",
          {
  newPedTwo <- suppressWarnings(qcStudbook(pedTwo, minParentAge = NULL))
  expect_equal(newPedTwo$sire[newPedTwo$id == "o1"], "U0001")
  expect_equal(newPedTwo$dam[newPedTwo$id == "o3"], "U0002")
  expect_equal(newPedTwo$sire[newPedTwo$id == "o1"], "U0001")
  expect_true(is.na(newPedTwo$sire[newPedTwo$id == "U0001"]))
          })
test_that("qcStudbook corrects status", {
            newPedTwo <- suppressWarnings(qcStudbook(pedTwo,
                                                     minParentAge = NULL))
            newPedTwo$status <- as.character(newPedTwo$status)
            expect_equal(newPedTwo$status[newPedTwo$id == "s2"], "ALIVE")
            expect_equal(newPedTwo$status[newPedTwo$id == "d2"], "ALIVE")
            expect_equal(newPedTwo$status[newPedTwo$id == "o1"], "SHIPPED")
          })
test_that("qcStudbook corrects ancestry", {
  newPedTwo <- suppressWarnings(qcStudbook(pedTwo, minParentAge = NULL))
  newPedTwo$ancestry <- as.character(newPedTwo$ancestry)
  expect_equal(newPedTwo$ancestry[newPedTwo$id == "s2"], "HYBRID")
  expect_equal(newPedTwo$ancestry[newPedTwo$id == "d2"], "UNKNOWN")
  expect_equal(newPedTwo$ancestry[newPedTwo$id == "o1"], "OTHER")
})

