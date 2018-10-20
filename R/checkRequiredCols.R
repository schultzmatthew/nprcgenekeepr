#' checkRequiredCols examines column names, \code{cols}, to see if all
#' required column names are present.
#'
#' @return NULL is returned if all required columns are present. See description
#' of \code{errors} for return values when required columns are missing.
#' @param cols character vector of column names
#' @param errors logical value when \code{TRUE} and missing columns are found
#' the \code{errorLst} object is updated with the names of the missing
#' columns and returned and when \code{FALSE} and missing columns are found
#' the program is stopped.
#' @importFrom rmsutilityr str_detect_fixed_all
#' @export
checkRequiredCols <- function(cols, errors) {
  requiredCols <- getRequiredCols()
  # Checking for the required fields (id, sire, dam, sex)
  if (!all(str_detect_fixed_all(cols, requiredCols))) {
    if (errors) {
      missingColumns <-
        requiredCols[!str_detect_fixed_all(cols, requiredCols,
                                           ignore_na = TRUE)]
#      if (any(c("id", "sire", "dam") %in% missingColumns))
      if (length(missingColumns) > 0)
        return(missingColumns)
    } else {
      stop(paste0("Required field(s) missing: ", paste0(requiredCols[
        !str_detect_fixed_all(cols, requiredCols, ignore_na = TRUE)],
        collapse = ", "), "."))
    }
  }
  return(NULL)
}
