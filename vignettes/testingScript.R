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
# d <- qc.Studbook(d)
# p <- d
# p <- resetPopulation(NULL, p)
# p["ped.num"] <- findPedigreeNumber(p$id, p$sire, p$dam)
# p["gen"] <- findGeneration(p$id, p$sire, p$dam)
# probands <- p$id[p$population]
# p <- trimPedigree(probands, p, removeUninformative = FALSE,
#                   addBackSingles = FALSE)
# test <- reportGV(p, gu.iter = 100)
# library(RODBC)
# library(stringi)
# conn <- odbcConnect("frogstar-vortex-animal-msharp")
# proband_file <- stri_c("/Users/msharp/Documents/Development/R/r_workspace/",
#                        "library/nprcmanager/inst/extdata/",
#                        "baboon_breeders_probands.csv")
# # probands <- sqlQuery(conn, stri_c("select id from current_data
# #                              where location in (102.06, 102.07, 102.08)
# #                              and at_sfbr = 'Y'"))
# # write.csv(probands,
# #           file = proband_file, row.names = FALSE)
# probands <- read.csv(proband_file, header = TRUE, sep = ",",
#                      stringsAsFactors = FALSE, na.strings = c("", "NA"),
#                      check.names = FALSE)
# library(animalr)
# library(rmsutilityr)
# probands$id <- blank_fill_ids(probands$id)
# ped <- get_direct_ancestors(conn, probands$id)
# ped <- add_birth_date(conn, ped)
# ped$birth_date <- format(ped$birth_date, format = "%Y-%m-%d")
# ped_qc <- qc.Studbook(ped)
# p <- trimPedigree(probands, ped_qc, removeUninformative = FALSE,
#                   addBackSingles = FALSE)
