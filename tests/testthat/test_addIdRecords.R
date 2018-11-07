context("addIdRecords")
library(testthat)
uPedOne <- data.frame(id = c(NA, "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
                      sire = c(NA, "s0", "s4", NA, "s1", "s1", "s2", "s2"),
                      dam = c(NA, "d0", "d4", NA, "d1", "d2", "d2", "d2"),
                      sex = c("M", "F", "M", "F", "F", "F", "F", "M"),
                      stringsAsFactors = FALSE)
pedOne <- data.frame(id = c("s1", "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
                     sire = c(NA, "s0", "s4", NA, "s1", "s1", "s2", "s2"),
                     dam = c(NA, "d0", "d4", NA, "d1", "d2", "d2", "d2"),
                     sex = c("M", "F", "M", "F", "F", "F", "F", "M"),
                     stringsAsFactors = FALSE)
uPedOne <- uPedOne[!is.na(uPedOne$id), ]
test_that("addIdRecords adds parents correctly", {
  newPed <- addIdRecords(ids = "s1", pedOne, uPedOne)
  expect_equal(nrow(uPedOne) + 1, nrow(newPed)) # no change
  expect_true(is.na(newPed$sire[newPed$id == "s1"]))
  expect_true(is.na(newPed$dam[newPed$id == "s1"]))

})
