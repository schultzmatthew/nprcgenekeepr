#' @param ids character vector of animal Ids
#' @param ped datatable that is the `Pedigree`. It contains pedigree
#' information including the IDs listed in \code{candidates}.
#' @param additionalMales Integer value of males to add to those within the
#' group when calculating the ratio. Ignored if calculated ratio is 0 or Inf.
#' Default is 0.
#' @param additionalFemales Integer value of females to add to those within the
#' group when calculating the ratio. Ignored if calculated ratio is 0 or Inf.
#'Default is 0.
sexRatioWithAdditions <- function(ids, ped, additionalMales, additionalFemales) {
  (length(ped$sex[ped$id %in% ids & ped$sex != "M"]) + additionalFemales) /
    (length(ped$sex[ped$id %in% ids & ped$sex == "M"]) + additionalMales)
}
