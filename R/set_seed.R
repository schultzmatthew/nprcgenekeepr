#' Work around for unit tests using sample()
#'
#' @param seed argument to \code{set.seed}
set_seed <- function(seed = 1) {
  version <- as.integer(R.Version()$major) + (as.numeric(R.Version()$minor) / 10.0)
  if (version >= 3.6) {
    suppressWarnings(set.seed(seed, sample.kind = "Rounding"))
  } else {
    set.seed()
  }
}
