#' Makes a list object of kinship matrices from simulated pedigrees of possible
#' parents for animals with unknow parents
#'
#' \code{createSimKinships} uses \code{makeSimPed} with the \code{ped} object
#' and the \code{allSimParents} object to create a set of kinship matrices to
#' be used in forming the \emph{Monte Carlo} estimates for the kinship values.
#'
#' @examples
#' \donttest{
#' ped <- nprcgenekeepr::smallPed
#' simParent_1 <- list(id = "A",
#'                     sires = c("s1_1", "s1_2", "s1_3"),
#'                     dams = c("d1_1", "d1_2", "d1_3", "d1_4"))
#' simParent_2 <- list(id = "B",
#'                     sires = c("s2_1", "s2_2", "s2_3"),
#'                     dams = c("d2_1", "d2_2", "d2_3", "d2_4"))
#' simParent_3 <- list(id = "E",
#'                     sires = c("s3_1", "s3_2", "s3_3"),
#'                     dams = c("d3_1", "d3_2", "d3_3", "d3_4"))
#' allSimParents <- list(simParent_1, simParent_2, simParent_3)
#' pop <- LETTERS[1:7]
#' simKinships <- createSimKinships(ped, allSimParents, pop, n = 10)
#' }
#'
#' @param ped The pedigree information in data.frame format
#' @param allSimParents list made up of lists where the internal list
#'        has the offspring ID \code{id}, a vector of representative sires
#'        (\code{sires}), and a vector of representative dams(\code{dams}).
#' @param pop Character vector with animal IDs to consider as the population of
#' interest. The default is NULL.
#' @param cumulative logical where if \code{FALSE} a list of lenght \code{n}
#' long of simulated kinship matrices is generated and if \code{FALSE} a named
#' list of length 2 is generated where the first element is the mean of the
#' simulated kinships and the second element is the standard deviation of the
#' simulated kinships.
#' @param n integer value of the number of simulated pedigrees to generate.
#' @export
createSimKinships <- function(ped, allSimParents, pop = NULL,
                              cumulative = FALSE, n = 10L) {
  ## If user has limited the population of interest by defining 'pop',
  ## that information is incorporated via the 'population' column.
  ped$population <- getGVPopulation(ped, pop)

  # Get the list of animals in the population to consider
  probands <- as.character(ped$id[ped$population])
  nIds <- nrow(ped)
  squaredKinship <- sumKinship <- matrix(data = 0, nrow = nIds, ncol = nIds)
  kVC <- list(kinshipValues = numeric(0), kinshipCounts = numeric(0))
  simKinships <- list(n)
  first_time <- TRUE

  for (i in seq_len(n)) {
    simPed <- makeSimPed(ped, allSimParents)
    if (cumulative) {
      kmat <- kinship(simPed$id, simPed$sire,
                     simPed$dam, simPed$gen)
      if (first_time) { # initializes minKinship correctly and adds IDs
        minKinship <- kmat
        maxKinship <- kmat
        first_time <- FALSE
      } else {
        minKinship <- pmin(minKinship, kmat)
        maxKinship <- pmax(maxKinship, kmat)
      }
      sumKinship <- sumKinship + kmat
      squaredKinship <- squaredKinship + kmat^2
    } else {
      simKinships[[i]] <- kinship(simPed$id, simPed$sire,
                                                  simPed$dam, simPed$gen)
    }
  }
  if (cumulative) {
    list(meanKinship = sumKinship / n,
          sdKinship = sqrt(
            ((n * squaredKinship) - sumKinship^2) /
              (n * (n - 1))
            ),
         minKinship = minKinship,
         maxKinship = maxKinship
         )
  } else {
    simKinships
  }
}
