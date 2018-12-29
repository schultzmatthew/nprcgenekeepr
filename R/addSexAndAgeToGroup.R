#' Forms a dataframe with Id, Sex, and current Age given a list of Ids and a pedigree
#'
#' @return Dataframe with Id, Sex, and Current Age
#'
#' @param ids character vector of animal Ids
#' @param ped datatable that is the `Pedigree`. It contains pedigree
#' information including the IDs listed in \code{candidates}.
#' @export
addSexAndAgeToGroup <- function(ids, ped) {
  group <- data.frame(ids,
             sex = sapply(ids, function(id) {ped$sex[ped$id == id]}),
             age = sapply(ids, function(id) {currentAge(ped$birth[ped$id == id])}),
             stringsAsFactors = FALSE)
  group
}
