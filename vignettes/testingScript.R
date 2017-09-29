###library(nprcmanager)
# UID.founders is not used; It may be a mistake, but it could be vestiges of
# something planned that was not done.
#UID.founders <- founders[grepl("^U", founders, ignore.case = TRUE)]
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
#                   addBackParents = FALSE)
# test <- reportGV(p, gu.iter = 100)
#library(RODBC)
library(stringi)
#conn <- odbcConnect("frogstar-vortex-animal-msharp")
proband_file <- stri_c("/Users/msharp/Documents/Development/R/r_workspace/",
                       "library/nprcmanager/inst/extdata/",
                       "baboon_breeders_only.csv")
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
probands <- as.character(probands$id)
#ped <- get_direct_ancestors(conn, probands)
ped <- getLkDirectRelatives(stri_trim_both(probands), unrelatedParents = FALSE)
ped$birth <- format(ped$birth, format = "%Y-%m-%d")
ped$death <- format(ped$death, format = "%Y-%m-%d")
ped$exit <- format(ped$exit, format = "%Y-%m-%d")
# write.csv(ped,
#           file = ped_file, row.names = FALSE)
ped_qc <- qcStudbook(ped)
p <- trimPedigree(probands, ped_qc, removeUninformative = FALSE,
                  addBackParents = FALSE)
# write.csv(p,
#            file = qc_ped_file, row.names = FALSE)
# p <- read.csv(qc_ped_file, header = TRUE, sep = ",",
#                      stringsAsFactors = FALSE, na.strings = c("", "NA"),
#                      check.names = FALSE)
genotype <- data.frame(id = p$id[50 + 1:20], first = 10000 + 1:20,
                       second = 20000 + 1:20,
                       first_name = stri_c("first", 1:20),
                       second_name = stri_c("second", 1:20),
                       stringsAsFactors = FALSE)
genotype_empty <- NULL
alleles <- geneDrop(p$id, p$sire, p$dam, p$gen, genotype, n = 4)
p_genotype <- addGenotype(p, genotype)
report <- reportGV(p_genotype, gu.iter = 500, gu.thresh = 1, pop = NULL,
                   byID = TRUE, updateProgress = NULL)
#data(baboonPed)
#kmat <- kinship(baboonPed$id, baboonPed$sire, baboonPed$dam, baboonPed$gen)
debug(groupAssign)
groupAssignTest <- groupAssign(candidates = probands, kmat = report$kinship,
                               ped = p_genotype,
                               ignore = NULL, minAge = 1, numGp = 2,
                               withKin = TRUE)
newGroupAssignTest <- groupAddAssign(candidates = probands, currentGroup = NULL,
                                     kmat = report$kinship, ped = p_genotype,
                               ignore = NULL, minAge = 1, numGp = 2,
                               withKin = TRUE)
groupAddTest <- groupAddition(candidates = probands,
                              currentGroup = probands[1:3],
                              kmat = report$kinship, ped = p_genotype,
                              ignore = NULL, minAge = 1, numGp = 1,
                              withKin = TRUE)
newGroupAddTest <- groupAddAssign(candidates = probands,
                                  currentGroup = probands[1:3],
                                  kmat = report$kinship, ped = p_genotype,
                                  ignore = NULL, minAge = 1, numGp = 1,
                                  withKin = TRUE)
# write.csv(p_genotype,
#          file = ped_genotype_file, row.names = FALSE)
p_genotype <- read.csv(ped_genotype_file, header = TRUE, sep = ",",
              stringsAsFactors = FALSE, na.strings = c("", "NA"),
              check.names = FALSE)
alleles2 <- geneDrop(p_genotype$id, p_genotype$sire, p_genotype$dam,
                     p_genotype$gen, p_genotype[ , c("id", "first", "second",
                                                     "first_name",
                                                     "second_name")], n = 1000)
gu <- calcGU(alleles, threshold = 1, byID = TRUE, pop = probands)
gu <- gu[probands, ,drop = FALSE]
length(p$sex[p$sex == "F"])

rhesus_file <- system.file("extdata", "BreedingGroups1_4MendozaTest.csv",
                        package = "nprcmanager")
rhesus_proband_file <- "inst/extdata/rhesus_mhc_proband_genotypes.csv"
rhesus_ped_file <- "inst/extdata/rhesus_mhc_ped.csv"
rhesus_proband <- read.csv(rhesus_file, header = TRUE, sep = ",",
                stringsAsFactors = FALSE, na.strings = c("", "NA"),
                check.names = FALSE)
rhesus_proband$id <- as.character(rhesus_proband$`Animal ID`)
rhesus_proband$first_name <- paste0(rhesus_proband[[3]], "_", rhesus_proband[[5]])
rhesus_proband$second_name <- paste0(rhesus_proband[[4]], "_", rhesus_proband[[6]])
rhesus_proband[ , 1:6] <- NULL
# write.csv(rhesus_proband,
#             file = rhesus_proband_file, row.names = FALSE)
rhesus_probands <- blank_fill_ids(rhesus_proband$id)
rhesus_ped <- get_direct_ancestors(conn, rhesus_probands)
rhesus_ped <- add_birth_date(conn, rhesus_ped)
rhesus_ped$birth_date <- format(rhesus_ped$birth_date, format = "%Y-%m-%d")
rhesus_ped$id <- stri_trim_both(rhesus_ped$id)
rhesus_ped$sire_id <- stri_trim_both(rhesus_ped$sire_id)
rhesus_ped$dam_id <- stri_trim_both(rhesus_ped$dam_id)
# write.csv(rhesus_ped,
#           file = rhesus_ped_file, row.names = FALSE)

