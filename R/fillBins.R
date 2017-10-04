#' Fill bins represented by list of two lists \code{males} and \code{females}.
#'
#'
#' @param age_dist dataframe with \code{sex} and \code{age} columns
#' @param lower_ages integer vector of lower age boundaries; must be the same
#' length as \code{upper_ages}
#' @param upper_ages integer vector of upper age boundaries; must be the same
#' length as \code{lower_ages}
#' @export
fillBins <- function(
  age_dist, lower_ages,
  upper_ages = NULL) {
  if (is.null(upper_ages))
    upper_ages <- c(lower_ages[-1], 100)
  male_bins <- c()
  female_bins <- c()
  for (bin in seq_along(lower_ages)) {
    male_bins <- c(male_bins,
                   nrow(age_dist[age_dist$sex == 'M' &
                                   age_dist$age >= lower_ages[bin] &
                                   age_dist$age < upper_ages[bin] &
                                   !is.na(age_dist$age), ]))
    female_bins <- c(female_bins,
                     nrow(age_dist[age_dist$sex == 'F' &
                                     age_dist$age >= lower_ages[bin] &
                                     age_dist$age < upper_ages[bin] &
                                     !is.na(age_dist$age), ]))
  }
  list(males = male_bins, females = female_bins)
}
