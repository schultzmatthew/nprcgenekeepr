#' Creates a folder with CSV files containing example pedigrees and ID lists
#' used to demonstrate the package.
#'
#' Creates a folder named \code{~/tmp/ExamplePedigrees} if it does not already
#' exist. It then proceeds to write each example pedigree into a CSV file named
#' based on the name of the example pedigree.
#' @export
createExampleFiles <- function() {
  examplePedigrees <-
    list(examplePedigree = nprcgenekeepr::examplePedigree,
         focalAnimals = nprcgenekeepr::focalAnimals,
         lacy1989Ped = nprcgenekeepr::lacy1989Ped,
         pedDuplicateIds = nprcgenekeepr::pedDuplicateIds,
         pedFemaleSireMaleDam = nprcgenekeepr::pedFemaleSireMaleDam,
         pedGood = nprcgenekeepr::pedGood,
         pedInvalidDates = nprcgenekeepr::pedInvalidDates,
         pedMissingBirth = nprcgenekeepr::pedMissingBirth,
         pedOne = nprcgenekeepr::pedOne,
         pedSameMaleIsSireAndDam = nprcgenekeepr::pedSameMaleIsSireAndDam,
         pedSix = nprcgenekeepr::pedSix,
         pedWithGenotype = nprcgenekeepr::pedWithGenotype,
         qcBreeders = as.data.frame(nprcgenekeepr::qcBreeders, drop = FALSE),
         qcPed = nprcgenekeepr::qcPed,
         smallPed = nprcgenekeepr::smallPed)
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
