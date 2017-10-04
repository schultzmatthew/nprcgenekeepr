context("makeRelationsClasses")
library(testthat)
suppressMessages(library(dplyr))

data("baboonPed")
bkmat <- kinship(baboonPed$id, baboonPed$sire, baboonPed$dam, baboonPed$gen,
                 sparse = FALSE)
kin <- convertRelationships(bkmat, baboonPed)
relClasses <- as.data.frame(makeRelationClasseTable(kin))
relClasses$`Relationship Class` <- as.character(relClasses$`Relationship Class`)
relClassTbl <- kin[!kin$relation == "Self", ] %>%
  group_by(relation) %>%
  summarise(count = n())
test_that("makeRelationsClasses retains the correct counts", {
  for (rel in relClasses[ , "Relationship Class"]) {
    expect_equal(relClasses$Frequency[relClasses$`Relationship Class` == rel],
                 relClassTbl$count[relClassTbl$relation == rel])
  }
})
