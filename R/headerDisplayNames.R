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
    zScores = "Z-score (Mean Kinship)",
    genomeUnique = "Genome Uniqueness",
    totalOffspring = "Total Offspring",
    livingOffspring = "Living Offspring",
    rank = "Rank",
    value = "Value Designation",
    status = "Status",
    vasxOvx = "Vasectomy or Overiectomy Status",
    ancestry = "Ancestry",
    gu = "Genome Uniqueness (%)",
    pedNum = "Pedigree #",
    spf = "SPF",
    condition = "Condition",
    first = "First Allele Code",
    second = "Second Allele Code",
    first_name = "First Allele",
    second_name = "Second Allele",
    recordStatus = "Original/ Added"
  )
  return(as.character(nameConversion[headers]))
}
