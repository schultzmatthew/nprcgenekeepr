#' getEmptyErrorLst
#'
#' @return errorLst object with placeholders for error types found in a pedigree
#' file by \code{qcStudbook}.
#' @export
getEmptyErrorLst <- function() {
  list(missingColumns = character(0),
       invalidDateRows = character(0),
       suspiciousParents = data.frame(),
       femaleSires = character(0),
       maleDams = character(0),
       duplicateIds = character(0),
       changedHeaders = list(caseChange = character(0),
                             spaceRemoved = character(0),
                             backslashRemoved = character(0),
                             underScoreRemoved = character(0),
                             egoidToId = character(0),
                             sireIdToSire = character(0),
                             damIdToDam =  character(0),
                             birthdateToBirth = character(0),
                             deathdateToDeath = character(0))
  )
}
