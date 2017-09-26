#' Convert internal column names to display or header names.
#'
#' Converts the column names of a Pedigree or Genetic value Report to
#' something more descriptive.
#'
#' @param headers a character vector of column (header) names
#'
#' @return Updated list of column names
#' @export
headerDisplayNames <- function(headers) {
  nameConversion <- c(
    id = "Ego ID",
    sire = "Sire ID",
    dam = "Dam ID",
    sex = "Sex",
    gen = "Generation #",
    birth = "Birth Date",
    exit = "Exit Date",
    age = "Age (in years)",
    population = "Breeding Colony Member",
    group = "Subset Member",
    origin = "Origin",
    indivMeanKin = "Individual Mean Kinship",
    z.scores = "Z-score (Mean Kinship)",
    genome.unique = "Genome Uniqueness",
    totalOffspring = "Total Offspring",
    living.offspring = "Living Offspring",
    rank = "Rank",
    value = "Value Designation",
    status = "Status",
    ancestry = "Ancestry",
    gu = "Genome Uniqueness (%)",
    ped.num = "Pedigree #",
    spf = "SPF",
    condition = "Condition",
    first_name = "First Allele",
    second_name = "Second Allele"
  )
  return(as.character(nameConversion[headers]))
}
