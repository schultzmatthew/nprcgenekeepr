#' Converts columns of dates in text form to \code{Date} object columns
#'
#' Finds date errors in columns defined in
#' \code{convertDate} as dates and converts date strings to \code{Date} objects.
#'
#' @return a list with the pedigree, \code{sb}, and the \code{errorLst} with
#' invalid date rows (\code{errorLst$invalidDateRows})
#'
#' If there are no errors that prevent the calculation of exit dates, they are
#' calculated and added to the pedigree otherwise the pedigree is not updated.
#' @param sb A dataframe containing a table of pedigree and demographic
#' information.
#' @param errorLst object with placeholders for error types found in a pedigree
#' file by \code{qcStudbook} through the functions it calls.
#' @export
getDateErrorsAndConvertDatesInPed <- function(sb, errorLst) {
  invalidDateRows <- convertDate(sb, time.origin = as.Date("1970-01-01"),
                                 reportErrors = TRUE)
  if (!is.null(invalidDateRows)) {
    errorLst$invalidDateRows <- invalidDateRows
    invalidAndAdded <- c(as.integer(invalidDateRows),
                         getRecordStatusIndex(sb, "added"))
    if (nrow(sb[-invalidAndAdded, ]) > 0) {
      sb[-invalidAndAdded, ] <-
        convertDate(sb[-invalidAndAdded, ],
                    time.origin = as.Date("1970-01-01"),
                    reportErrors = FALSE)
    }
  } else {
    sb <- convertDate(sb, time.origin = as.Date("1970-01-01"),
                      reportErrors = FALSE)
    sb <- setExit(sb, time.origin = as.Date("1970-01-01"))
  }
  list(sb = sb, errorLst = errorLst)
}
