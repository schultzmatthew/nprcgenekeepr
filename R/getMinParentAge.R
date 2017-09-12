#' Get minimum parent age.
#'
#' This can be set to anything greater than or equal to 0.
#'
#' Set to 0 if you do not want to enforce parents being sexually mature
#' by age. Animals that do not have an age are ignored.
#' @export
getMinParentAge <- function() {
  minParentAge <- as.numeric(renderText({input$minParentAge}))
  if (minParentAge < 0)
    stop("Minimum Parent Age must be >= 0.")
  else {
    return(minParentAge)
  }
}
