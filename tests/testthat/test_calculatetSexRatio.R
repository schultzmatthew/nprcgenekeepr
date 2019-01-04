context("calculateSexRatio")
library(testthat)
library(nprcmanager)
data("baboonBreeders")
data("pedWithGenotype")
skip_if_not(exists("baboonBreeders"))
skip_if_not(exists("pedWithGenotype"))
available <- c("32743", "32771", "33088", "26450", "27647", "31937", "32732",
  "32798", "34210", "34832", "34953", "34984", "33866", "33873", "33895",
  "34155", "34163", "35315")
nonMales <- c("32743", "32771", "33088", "26450", "31937", "32732", "32798",
              "34210", "34832", "34953", "34984", "33866", "33873", "33895",
              "34155", "34163", "35315")
male <- "27647"
expect_equal(calculateSexRatio(ids = character(0), ped = pedWithGenotype), 0)
expect_equal(calculateSexRatio(ids = male, ped = pedWithGenotype), 0)
expect_equal(calculateSexRatio(ids = nonMales, ped = pedWithGenotype), Inf)
expect_equal(calculateSexRatio(ids = available, ped = pedWithGenotype),
             17)
