#' Calculates the sex ratio (number of non-males / number of males) given
#' animal Ids and their pedigree
#'
#' @return Numeric value of sex ratio of the animals provided.
#'
#' @description The Males are counted when the \code{ped$sex} value is
#' \code{"M"}.
#' When females are counted when the \code{ped$sex} value is not
#' \code{"M"}. This means animals with ambiguous sex are counted with the
#' females.
#'
#' @param ids character vector of animal Ids
#' @param ped datatable that is the `Pedigree`. It contains pedigree
#' information including the IDs listed in \code{candidates}.
#' @param additionalMales Integer value of males to add to those within the
#' group when calculating the ratio. Ignored if calculated ratio is 0 or Inf.
#' Default is 0.
#' @param additionalFemales Integer value of females to add to those within the
#' group when calculating the ratio. Ignored if calculated ratio is 0 or Inf.
#'Default is 0.
#'
#' @export
calculateSexRatio <- function(ids, ped, additionalMales = 0,
                              additionalFemales = 0) {
  if (length(ids) == 0) {
    if (additionalFemales > 0) {
      if (additionalMales == 0) {
        ratio <- Inf
      } else if (additionalMales > 0) {
        ratio <- sexRatioWithAdditions(ids, ped, additionalMales, additionalFemales)
      }
    } else if (additionalFemales == 0) {
      if (additionalMales == 0) {
        ratio <- NA
      } else {
        ratio <- 0.0
      }
    }
  } else if (length(ped$sex[ped$id %in% ids & ped$sex == "M"]) == 0) { #no males
    if (additionalMales > 0) {
      ratio <- sexRatioWithAdditions(ids, ped, additionalMales, additionalFemales)
    } else {
      ratio <- Inf
    }
  } else {
    ratio <- sexRatioWithAdditions(ids, ped, additionalMales, additionalFemales)
  }
 
  ratio
}
