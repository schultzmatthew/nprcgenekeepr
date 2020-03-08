#' Converts status indicators to a Standardized code
#'
## Copyright(c) 2017-2019 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Part of Pedigree Curation
#'
#'
#' @param status character vector or NA. Flag indicating an individual's
#' status as alive, dead, sold, etc.
#'
#' @return factor {levels: ALIVE, DECEASED, SHIPPED, UNKNOWN}. Vector of
#' standardized status codes with the possible values
#' ALIVE, DECEASED, SHIPPED, or UNKNOWN
#' @examples
#' \donttest{
#' library(nprcgenekeepr)
#' original <- c("A", "alive", "Alive", "1", "S", "Sale", "sold", "shipped",
#'               "D", "d", "dead", "died", "deceased", "2",
#'               "shiped", "3", "U", "4", "unknown", NA,
#'               "Unknown", "H", "hermaphrodite", "U", "Unknown", "4")
#' convertStatusCodes(original)
#' }
#' @export
convertStatusCodes <- function(status) {
  status <- toupper(status)
  status[is.na(status)] <- "UNKNOWN"
  status[status %in% c("ALIVE", "A", "1")] <- "ALIVE"
  status[status %in% c("DECEASED", "DEAD", "DIED", "D", "2")] <- "DECEASED"
  status[status %in% c("SHIPPED", "SHIPED", "SOLD", "SALE", "S", "3")] <-
    "SHIPPED"
  status[status %in% c("UNKNOWN", "U", "4")] <- "UNKNOWN"

  status <- factor(status, levels = c("ALIVE", "DECEASED", "SHIPPED",
                                      "UNKNOWN"))
  return(status)
}
