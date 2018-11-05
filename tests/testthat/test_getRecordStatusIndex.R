context("getRecordStatusIndex")
library(testthat)
test_that(
  "getRecordStatusIndex handles dataframe without a record_status column", {
    pedSix <- createPedSix()
    expect_equal(nprcmanager:::getRecordStatusIndex(pedSix), integer(0))
    pedSix <- cbind(pedSix, record_status = c(rep("original", 5),
                                              rep("added", 3)))
    expect_equal(nprcmanager:::getRecordStatusIndex(pedSix, status = "added"),
                 c(6:8))
    expect_equal(nprcmanager:::getRecordStatusIndex(pedSix, status = "original"),
                 c(1:5))

})
