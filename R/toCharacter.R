#' Force dataframe columns to character
#'
#' Converts designated columns of a dataframe to character. Defaults to
#' converting columns \code{id}, \code{sire}, and \code{dam}.
#'
#' NOTE:
#' Function was created to deal with a display bug in xtables (used by Shiny)
#' The bug causes Date columns to be displayed improperly
#' Shiny also displays all Numbers (including integers) with 2 decimal places
#' Comment by R. Mark Sharp - this is just one way to modify the number of
#' digits displayed.
#' library(DT)
#'
#'   set_seed(10)
#' data.frame(x=runif(10), y=rnorm(10), z=rpois(10, 1)) %>%
#'   datatable() %>%
#'   formatRound(columns=c('x', 'y'), digits=3)
#'
#' @param  df a dataframe where the first three columns can be coerced to
#' character.
#' @param headers character vector with the columns to be converted to
#' character class. Defaults to \code{c("id", "sire", "dam")}/
#' @return A dataframe with the specified columns converted to class
#' "character" for display with xtables (in shiny)
#' @export
toCharacter <- function(df, headers = c("id", "sire", "dam")) {
  headers <- intersect(names(df), headers)
  for (col in headers) {
    df[[col]] <- as.character(df[[col]])
  }
  return(df)
}
