context("insertSeparators")
library(testthat)
test_that(
  "insert separators at the right time and in the correct way", {
    expect_equal(nprcmanager:::insertSeparators(c("19991002", "20100228"))[[1]],
                 "1999-10-02")
    expect_equal(nprcmanager:::insertSeparators(c("19991002", "20100228"))[[2]],
                 "2010-02-28")
    expect_equal(nprcmanager:::insertSeparators(c("1999/10/02", "2010/02/28"))[[2]],
                 "2010/02/28")
})
