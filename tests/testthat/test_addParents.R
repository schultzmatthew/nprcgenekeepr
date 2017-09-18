context("addParents")
library(testthat)
library(stringi)
pedOne <- data.frame(id = c("s1", "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
                  sire = c(NA, NA, NA, NA, "s1", "s1", "s2", "s2"),
                  dam = c(NA, NA, NA, NA, "d1", "d2", "d2", "d2"),
                  sex = c("M", "F", "M", "F", "F", "F", "F", "M"),
                  stringsAsFactors = FALSE)
pedTwo <- data.frame(id = c(NA, "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
                     sire = c(NA, NA, NA, NA, "s1", "s1", "s2", "s2"),
                     dam = c(NA, NA, NA, NA, "d1", "d2", "d2", "d2"),
                     sex = c("M", "F", "M", "F", "F", "F", "F", "M"),
                     stringsAsFactors = FALSE)
pedTwo <- pedTwo[!is.na(pedTwo$id), ]
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

test_that("addParents adds parents correctly", {
  newPed <- addParents(pedOne)
  expect_equal(nrow(pedOne), nrow(newPed)) # no change
  newPed <- addParents(pedTwo)
  expect_equal(nrow(pedTwo) + 1, nrow(newPed)) # 1 sire added
  newPed <- addParents(pedThree)
  expect_equal(nrow(pedThree) + 1, nrow(newPed)) # 1 dam added
  newPed <- addParents(pedFour)
  expect_equal(nrow(pedFour) + 2, nrow(newPed)) # 1 sire and 1 dam added
})
