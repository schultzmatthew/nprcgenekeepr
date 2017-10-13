context("findLoops")
library(testthat)
data("smallPed")
ped <- smallPed
data("smallPedTree")
pedTree <- smallPedTree
loops <- findLoops(pedTree)
## No test performed
## Calls makesLoop function, which does not work. It is looking for intersection
## between dams and sires instead of repeats of dams and repeats of sires.
