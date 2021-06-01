#' Counts the number of occurrences of each kinship value seen for a pair of
#' individuals.
#'
## @examples
## \donttest{
## set_seed(20210529)
## kSamples <- sample(c(0, 0.0675, 0.125, 0.25, 0.5, 0.75), 10000, replace = TRUE,
##                    prob = c(0.005, 0.3, 0.15, 0.075, 0.0375, 0.01875))
## kVC <- list(kinshipValues = numeric(0),
##             kinshipCounts = numeric(0))
## for (kSample in kSamples) {
##   kVC <- countKinshipValues(kSample, kVC$kinshipValues, kVC$kinshipCounts)
## }
## kVC
## ## $kinshipValues
## ## [1] 0.7500 0.1250 0.0675 0.2500 0.5000 0.0000
## ##
## ## $kinshipCounts
## ## [1]  301 2592 5096 1322  592   97
## }
#'
#' @param kValue numeric value being counted (kinship value in
#' \emph{nprcgenekeepr})
#' @param kinshipValues vector of unique values of \code{kValue} seen
#' thus far.
#' @param kinshipCounts vector of the counts of the unique values of
#' \code{kValue} seen thus far.
#' @export
countKinshipValues <- function(kValue, kinshipValues = numeric(0),
                              kinshipCounts = numeric(0)) {
  for (row in seq_len(nrow(kValue))) {
    kinshipValue <- match(kValue[row, ], kinshipValues[row, ], nomatch = -1L)
    if (kinshipValue == -1L) {
      kinshipValues <- c(kinshipValues, kValue)
      kinshipCounts[length(kinshipCounts) + 1] <- 1
    } else {
      kinshipCounts[kinshipValue] <- kinshipCounts[kinshipValue] + 1
    }

  }
  list(kinshipValues = kinshipValues,
       kinshipCounts = kinshipCounts)
}

## set_seed(seed = 1)
## kSamples <- sample(c(0, 0.0675, 0.125, 0.25, 0.5, 0.75), 10000, replace = TRUE,
##                    prob = c(0.005, 0.3, 0.15, 0.075, 0.0375, 0.01875))
## kVC <- list(kinshipValues = numeric(0),
##             kinshipCounts = numeric(0))
## for (kSample in kSamples) {
##   kVC <- countKinshipValues(kSample, kVC$kinshipValues, kVC$kinshipCounts)
## }
## kVC
## kinshipValuesAndCounts <-
##   countKinshipValues(0.625,
##                      kinshipValues = kinshipValuesAndCounts$kinshipValues,
##                      kinshipCounts = kinshipValuesAndCounts$kinshipCounts)
##
