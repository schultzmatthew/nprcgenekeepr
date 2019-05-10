context("geneDrop")
library(testthat)
data(lacy1989Ped)
data(lacy1989PedAlleles)
set_seed(10)
## This test is entirely dependent on repeatable pseudorandom sequence
## generation. If this is disturbed, it will need to be rewritten.
ped <- lacy1989Ped
alleles <- lacy1989PedAlleles
pedFactors <- data.frame(
  id = as.factor(ped$id),
  sire = as.factor(ped$sire),
  dam = as.factor(ped$dam),
  gen = ped$gen,
  population = ped$population,
  stringsAsFactors = TRUE
)
genotype <- data.frame(id = ped$id,
                       first_allele = c(NA, NA, "A001_B001", "A001_B002", NA,
                                 "A002_B003", NA),
                       second_allele = c(NA, NA, "A010_B001", "A001_B001", NA,
                                  "A005_B002", NA),
                       stringsAsFactors = FALSE)
pedGenotype <- addGenotype(ped, genotype)
pedGenotype <- getGVGenotype(pedGenotype)
allelesFactors <- geneDrop(pedFactors$id, pedFactors$sire, pedFactors$dam,
                           pedFactors$gen, genotype = NULL, n = 5000,
                           updateProgress = NULL)
allelesNew <- geneDrop(ped$id, ped$sire, ped$dam,
                       ped$gen, genotype = NULL, n = 5000,
                       updateProgress = NULL)
allelesNewGen <- geneDrop(ped$id, ped$sire, ped$dam, ped$gen,
                          genotype = pedGenotype,
                          n = 5000, updateProgress = NULL)

test_that("geneDrop correctly drops gene down the pedigree using
          random segregation by Mendelian rules", {
            expect_equal(table(as.numeric(allelesNew[7, 1:5000]))[[1]], 2521)
            expect_equal(table(as.numeric(allelesNew[7, 1:5000]))[[2]], 2479)
            expect_equal(table(as.numeric(allelesFactors[7, 1:5000]))[[1]],
                         2486)
            expect_equal(table(as.numeric(allelesFactors[7, 1:5000]))[[2]],
                         2514)
            expect_equal(table(as.numeric(allelesNewGen[7, 1:5000]))[[1]],
                         5000)
            expect_equal(table(as.numeric(allelesNewGen[9, 1:5000]))[[1]],
                         5000)
            expect_equal(table(as.numeric(allelesNewGen[13, 1:5000]))[[1]],
                         2555)
            expect_equal(table(as.numeric(allelesNewGen[13, 1:5000]))[[2]],
                         2445)
          })
