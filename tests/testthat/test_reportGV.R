
# library(nprcmanager)
# library(RODBC)
# library(animalr)
# library(rmsutilityr)
#
# d <- read.csv("../../inst/extdata/BaboonLivingForMetrics.csv", header = TRUE,
#               sep = ",", stringsAsFactors = FALSE, na.strings = "",
#               check.names = FALSE)
# d <- qcStudbook(d)
# p <- d
# p <- resetPopulation(NULL, p)
# p["ped.num"] <- findPedigreeNumber(p$id, p$sire, p$dam)
# p["gen"] <- findGeneration(p$id, p$sire, p$dam)
# probands <- p$id[p$population]
# p <- trimPedigree(probands, p)
# gv_result <- reportGV(p, gu.iter = 100)
# ids <- c("31879", "31882", "31900", "31902", "31938", "31940",
#          "31951", "31952", "31966", "31970", "31980", "32043",
#          "32130", "32132", "32303", "31648", "31659", "31660",
#          "31738", "31740", "31764", "31786", "31792", "31800",
#          "31817", "31844", "31852", "31993", "32040")
# currentGroup <- character(0)
# p_org <- p
# p <- resetGroup(ids, p)
#
# conn <- odbcConnect("kittyhawk-animal-sa")
# da <- get_direct_ancestors(conn, ids)
# deb_ped <- read.csv2(file = "/Users/msharp/Desktop/2cage_bab_brdrs_ped.csv")
# names(deb_ped)
# names(deb_ped) <- c("id", "sire_id", "dam_id", "sex", "birth_date", "exit_date")
# deb_ped[deb_ped$id %in% blank_fill_ids(da$id), ]
