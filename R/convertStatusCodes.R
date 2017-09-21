#' Converts status indicators to a Standardized code
#'
#' Part of Pedigree Curation
#'
#'
#' @param status character vector or NA. Flag indicating an individual's
#' status as alive, dead, sold, etc.
#'
#' @return factor {levels: ALIVE, DECEASED, SHIPPED, UNKNOWN}. Vector of
#' standardized status codes with the possible values
#' ALIVE, DECEASED, SHIPPED, or UNKNOWN
#' @export
convertStatusCodes <- function(status) {
  status <- toupper(status)
  status[is.na(status)] <- "UNKNOWN"
  status[status %in% c("ALIVE", "A", "1")] <- "ALIVE"
  status[status %in% c("DECEASED", "DEAD", "D", "2")] <- "DECEASED"
  status[status %in% c("SHIPPED", "SOLD", "SALE", "S", "3")] <- "SHIPPED"
  status[status %in% c("UNKNOWN", "U", "4")] <- "UNKNOWN"

  status <- factor(status, levels = c("ALIVE", "DECEASED", "SHIPPED", "UNKNOWN"))
  return(status)
}
