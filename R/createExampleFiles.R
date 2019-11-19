#' Creates a folder with CSV files containing example pedigrees and ID lists
#' used to demonstrate the package.
#'
#' Creates a folder named \code{~/tmp/ExamplePedigrees} if it does not already
#' exist. It then proceeds to write each example pedigree into a CSV file named
#' based on the name of the example pedigree.
#' @export
createExampleFiles <- function() {
  examplePedigrees <-
    list(examplePedigree = nprcmanager::examplePedigree,
         focalAnimals = nprcmanager::focalAnimals,
         lacy1989Ped = nprcmanager::lacy1989Ped,
         pedDuplicateIds = nprcmanager::pedDuplicateIds,
         pedFemaleSireMaleDam = nprcmanager::pedFemaleSireMaleDam,
         pedGood = nprcmanager::pedGood,
         pedInvalidDates = nprcmanager::pedInvalidDates,
         pedMissingBirth = nprcmanager::pedMissingBirth,
         pedOne = nprcmanager::pedOne,
         pedSameMaleIsSireAndDam = nprcmanager::pedSameMaleIsSireAndDam,
         pedSix = nprcmanager::pedSix,
         pedWithGenotype = nprcmanager::pedWithGenotype,
         qcBreeders = as.data.frame(nprcmanager::qcBreeders, drop = FALSE),
         qcPed = nprcmanager::qcPed,
         smallPed = nprcmanager::smallPed)
  pedigree_dir <- "~/tmp"
  suppressWarnings(dir.create(pedigree_dir))
  pedigree_dir <- paste0(pedigree_dir, "/ExamplePedigrees")
  suppressWarnings(dir.create(pedigree_dir))
  setwd(pedigree_dir)
  message(paste0("Example pedigree files ",
                 get_and_or_list(names(examplePedigrees)),
                 " will be created in ", pedigree_dir, ".\n"))
  saveDataframesAsFiles(examplePedigrees, "csv")
}
