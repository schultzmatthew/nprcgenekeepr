###library(nprcmanager)
# write.csv(ped, file = "/Users/msharp/Documents/Development/R/r_workspace/library/nprcmanager/inst/extdata/snprc_baboon_ped.csv", row.names = FALSE)
# d <- read.csv(
#   "/Users/msharp/Documents/Development/R/r_workspace/library/nprcmanager/inst/extdata/BaboonLivingForMetrics.csv",
#               header = TRUE, sep = ",", stringsAsFactors = FALSE,
#               na.strings = c("", "NA"), check.names = FALSE)
# d <- read.csv(
#   "/Users/msharp/Documents/Development/R/r_workspace/library/nprcmanager/inst/extdata/Example_Pedigree.csv",
#   header = TRUE, sep = ",", stringsAsFactors = FALSE,
#   na.strings = c("", "NA"), check.names = FALSE)
# d <- read.csv(
#   "/Users/msharp/Documents/Development/R/r_workspace/library/nprcmanager/inst/extdata/snprc_baboon_ped.csv",
#   header = TRUE, sep = ",", stringsAsFactors = FALSE, na.strings = c("", "NA"), check.names = FALSE)
# d <- qcStudbook(d)
# p <- d
# p <- resetPopulation(NULL, p)
# p["ped.num"] <- findPedigreeNumber(p$id, p$sire, p$dam)
# p["gen"] <- findGeneration(p$id, p$sire, p$dam)
# probands <- p$id[p$population]
# p <- trimPedigree(probands, p, removeUninformative = FALSE,
#                   addBackSingles = FALSE)
# test <- reportGV(p, gu.iter = 100)
library(RODBC)
library(stringi)
conn <- odbcConnect("frogstar-vortex-animal-msharp")
proband_file <- stri_c("/Users/msharp/Documents/Development/R/r_workspace/",
                       "library/nprcmanager/inst/extdata/",
                       "baboon_breeders_probands.csv")
ped_file <- stri_c("/Users/msharp/Documents/Development/R/r_workspace/",
                      "library/nprcmanager/inst/extdata/",
                      "baboon_breeders_ped.csv")
qc_ped_file <- stri_c("/Users/msharp/Documents/Development/R/r_workspace/",
                       "library/nprcmanager/inst/extdata/",
                       "baboon_breeders_qc_ped.csv")
genotype_file <- stri_c("/Users/msharp/Documents/Development/R/r_workspace/",
                        "library/nprcmanager/inst/extdata/",
                        "baboon_breeders_genotypes.csv")
ped_genotype_file <- stri_c("/Users/msharp/Documents/Development/R/r_workspace/",
                      "library/nprcmanager/inst/extdata/",
                      "baboon_breeders_ped_genotype.csv")
# probands <- sqlQuery(conn, stri_c("select id from current_data
#                              where location in (102.06, 102.07, 102.08)
#                              and at_sfbr = 'Y'"))
# write.csv(probands,
#           file = proband_file, row.names = FALSE)
# write.csv(genotype,
#          file = genotype_file, row.names = FALSE)
probands <- read.csv(proband_file, header = TRUE, sep = ",",
                     stringsAsFactors = FALSE, na.strings = c("", "NA"),
                     check.names = FALSE)
library(animalr)
library(rmsutilityr)
probands <- blank_fill_ids(probands$id)
ped <- get_direct_ancestors(conn, probands)
ped <- add_birth_date(conn, ped)
ped$birth_date <- format(ped$birth_date, format = "%Y-%m-%d")
ped$id <- stri_trim_both(ped$id)
ped$sire_id <- stri_trim_both(ped$sire_id)
ped$dam_id <- stri_trim_both(ped$dam_id)

probands <- stri_trim_both(probands)
write.csv(ped,
          file = ped_file, row.names = FALSE)
ped_qc <- qcStudbook(ped)
p <- trimPedigree(probands, ped_qc, removeUninformative = FALSE,
                  addBackSingles = FALSE)
write.csv(p,
           file = qc_ped_file, row.names = FALSE)
p <- read.csv(qc_ped_file, header = TRUE, sep = ",",
                     stringsAsFactors = FALSE, na.strings = c("", "NA"),
                     check.names = FALSE)
genotype <- data.frame(id = p$id[50 + 1:20], first = 10000 + 1:20,
                       second = 20000 + 1:20,
                       first_name = stri_c("first", 1:20),
                       second_name = stri_c("second", 1:20),
                       stringsAsFactors = FALSE)
genotype_empty <- NULL
alleles <- geneDrop(p$id, p$sire, p$dam, p$gen, genotype, n = 1000)
p_genotype <- merge(p, genotype, by = "id", all = TRUE)
write.csv(p_genotype,
         file = ped_genotype_file, row.names = FALSE)
p_genotype <- read.csv(ped_genotype_file, header = TRUE, sep = ",",
              stringsAsFactors = FALSE, na.strings = c("", "NA"),
              check.names = FALSE)
alleles2 <- geneDrop(p_genotype$id, p_genotype$sire, p_genotype$dam,
                     p_genotype$gen, p_genotype[ , c("id", "first", "second",
                                                     "first_name",
                                                     "second_name")], n = 1000)
gu <- calc.gu(alleles, threshold = 1, by.id = TRUE, pop = probands)
gu <- gu[probands, ,drop = FALSE]
length(p$sex[p$sex == "F"])

