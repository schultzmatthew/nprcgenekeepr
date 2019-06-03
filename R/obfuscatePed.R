#' obfuscatePed takes a pedigree object and creates aliases for all IDs and
#' adjusts all date within a specified amount.
#'
#' User provides a pedigree object (\code{ped}), the number of characters to be
#' used for alias IDs (\code{size}), and the maximum number of days that the
#' birthdate can be shifted (\code{maxDelta}).
#'
#' @return obfuscated pedigree
#'
#' @param ped pedigree object
#' @param size integer value indicating number of characters in alias IDs
#' @param maxDelta integer value indicating maximum number of days that
#' the birthdate can be shifted
#' @param existingIds character vector of existing aliases to avoid duplication.
#' @export
obfuscatePed <- function(ped, size = 6, maxDelta = 30, existingIds = character(0)) {
  alias <- obfuscateId(ped$id, size = size, existingIds = existingIds)
  ped$sire <- alias[ped$sire]
  ped$dam <- alias[ped$dam]
  ped$id <- alias
  for (col in names(ped)) {
    if (any("Date" %in% class(ped[[col]]))) {
      ped[[col]] <- obfuscateDate(ped[[col]], maxDelta = maxDelta)
    }
  }
  if (any("age" %in% names(ped)) & any("birth" %in% names(ped)) & any("exit" %in% names(ped))) {
    if (all(is.Date(ped$birth)))
      ped["age"] <- calcAge(ped$birth, ped$exit)
  }
  ped
}
