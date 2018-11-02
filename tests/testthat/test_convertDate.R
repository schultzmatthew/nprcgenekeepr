context("convertDate")
library(testthat)
library(lubridate)
set.seed(10)
someBirthDates <- paste0(sample(seq(0, 15, by = 3), 10, replace = TRUE) + 2000, "-",
                            sample(1:12, 10, replace = TRUE), "-",
                    sample(1:28, 10, replace = TRUE))
someBadBirthDates <- paste0(sample(1:12, 10, replace = TRUE), "-",
                         sample(1:28, 10, replace = TRUE), "-",
                         sample(seq(0, 15, by = 3), 10, replace = TRUE) + 2000)
someDeathDates <- sample(someBirthDates, length(someBirthDates), replace = FALSE)
someDepartureDates <- sample(someBirthDates, length(someBirthDates), replace = FALSE)
ped1 <- data.frame(birth = someBadBirthDates, death = someDeathDates, departure = someDepartureDates)
someDates <- ymd(someBirthDates)
ped2 <- data.frame(birth = someDates, death = someDeathDates, departure = someDepartureDates)
ped3 <- data.frame(birth = someBirthDates, death = someDeathDates, departure = someDepartureDates)
someNADeathDates <- someDeathDates
someNADeathDates[c(1, 3, 5)] <- ""
someNABirthDates <- someDates
someNABirthDates[c(2, 4, 6)] <- NA
ped4 <- data.frame(birth = someNABirthDates, death = someNADeathDates, departure = someDepartureDates)

test_that("convertDate identifies bad dates", {
  expect_error(convertDate(ped1))
})
test_that("convertDate with error flag returns error list", {
  expect_equal(convertDate(ped1, errors = TRUE),
               c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
})
test_that("convertDate likes good dates", {
  expect_true(all(is.Date(convertDate(ped2)$birth)))
  expect_true(all(is.Date(convertDate(ped3)$birth)))
})
test_that("convertDate with error flag returns NULL with good dates", {
  expect_true(all(is.null(convertDate(ped2, errors = TRUE))))
  expect_true(all(is.null(convertDate(ped3, errors = TRUE))))
})
test_that("convertDate handles NA and empty character string values correctly", {
  expect_equal(convertDate(ped4, errors = TRUE), as.character(1:6))
})

