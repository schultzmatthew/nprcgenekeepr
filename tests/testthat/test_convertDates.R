context("chooseDate")
library(testthat)
library(lubridate)
set.seed(10)
someDates <- mdy(paste0(sample(1:12, 10, replace = TRUE), "-",
                    sample(1:28, 10, replace = TRUE), "-",
                    sample(seq(0, 15, by = 3), 10, replace = TRUE) + 2000))
ped1 <- data.frame(birth = c("1", "2"), death = someDates[1:2], departure = c(1, 3))

test_that("convertDate identifies bad dates", {
  expect_error(convertDates(ped1))
})
