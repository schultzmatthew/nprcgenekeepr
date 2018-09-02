#' Append the rows of one dataframe to another.
#'
#' Part of Pedigree Curation
#'
#' Appends the rows of df2 to df1, can handle cases where df2
#' has a subset of the columns of df1
#'
#' @param df1 the target dataframe to append to.
#' @param df2 the the donor dataframe information should be appended from
#'
#' @return The appended dataframe with \code{NA} inserted into columns as
#' needed.
#' @export
rbindFill <- function(df1, df2) {

  # Find columns in df1 not in df2
  add.headers <- setdiff(names(df1), names(df2))

  # Add the missing columns to df2 (containing NA values)
  if (!isEmpty(add.headers)) {
    for (i in 1:length(add.headers)) {
      col <- df1[[add.headers[i]]] # We want to extract not subset
      col.type <- mode(col)
      if (col.type == "numeric") {
        if (class(col) == "Date") {
          df2[, add.headers[i]] <- as.Date(NA, origin = as.Date("1970-01-01"))
        } else{
          df2[, add.headers[i]] <- NaN
        }
      }
      else if (col.type %in% c("character", "logical")) {
        df2[, add.headers[i]] <- NA
      }
      else{
        stop(col.type, " : unknown column type")
      }
    }
  }
  return(rbind(df1, df2))
}
