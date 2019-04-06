#' global variables
#'
globalMinParentAge <- 3
MAXGROUPS <- 10
# takes in two arguments
sumN <- function(a, x){
  a <- sum(a, as.numeric(x), na.rm = TRUE)
  return(a)
}
