#' summary.nprcmanagerErr Summary function for class nprcmanagerErr
#'
#' @return object of class summary.nprcmanagerErr
#'
#' @param errorLst object of class nprcmanagerErr and class list
#' @importFrom stringi stri_c
#' @export
summary.nprcmanagerErr <- function(errorLst) {
  if (is.null(checkErrorLst(errorLst)))
    return(NULL)
  txt <- "Changes made to pedigree file during import are listed below.\n"

  txt <- addErrTxt(txt, errorLst$missingColumns, "missing column is",
                   "missing columns are")
  }
  if (length(errorLst$invalidDateRows) == 1) {
    txt <- stri_c("The row having an invalid date is: ", errorLst$invalidDateRows, ".\n")
  } else if (length(errorLst$missingColumns) > 1) {
    txt <- stri_c("The rows (up to the first 5) having an invalid date are: ",
                  get_and_or_list(errorLst$invalidDateRows), ".\n")
  }
  if (length(errorLst$sireIsDam) == 1) {
    txt <- stri_c("The animal listed as both sire and dam is: ", errorLst$sireIsDam, "'.\n")
  } else if (length(errorLst$sireIsDam) > 1) {
    txt <- stri_c("The animals listed as both sire and dam are: ",
                  get_and_or_list(errorLst$sireIsDam), ".\n")
  }
  addErrorTxt <- function(txt, err, singularTxt, pluralTxt) {
    if (length(err) == 1) {
      txt <- stri_c(txt, "The ", singularTxt, ": ", err, ".\n")
    } else if (length(err) > 1) {
      txt <- stri_c(txt, "The ", pluralTxt, ": ",
                    get_and_or_list(err), ".\n")
    }
    txt
  }
  class(errorLst) <- "summary.nprcmanagerErr"
  errorLst
}
