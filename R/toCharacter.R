#' Force dataframe columns to character
#'
## Copyright(c) 2017-2019 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Converts designated columns of a dataframe to character. Defaults to
#' converting columns \code{id}, \code{sire}, and \code{dam}.
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
