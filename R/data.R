#' ped1_alleles is a dataframe created by the geneDrop function
#'
#' @format A dataframe with 554 rows and 6 varialbes
#' \describe{
#' \item{V1}{alleles assigned to the parents of the animals identified in
#' the \code{id} column during iteration 1 of gene dropping performed by
#' \code{geneDrop}.}
#' \item{V2}{alleles assigned to the parents of the animals identified in
#' the \code{id} column during iteration 1 of gene dropping performed by
#' \code{geneDrop}.}
#' \item{V3}{alleles assigned to the parents of the animals identified in
#' the \code{id} column during iteration 1 of gene dropping performed by
#' \code{geneDrop}.}
#' \item{V4}{alleles assigned to the parents of the animals identified in
#' the \code{id} column during iteration 1 of gene dropping performed by
#' \code{geneDrop}.}
#' \item{id}{character vector of animal IDs provided to the gene dropping
#' function \code{geneDrop}.}
#' \item{parent}{the parent type ("sire" or "dam") of the parent who supplied
#' the alleles as assigned during each of the 4 gene dropping iterations
#' performed by \code{geneDrop}.}
#' }
#' @source example baboon pedigree file provided by Deborah Newman,
#' Southwest National Primate Center.
"ped1_alleles"
#' baboon_ped is a dataframe with 277 rows and 6 columns
#'
#' \describe{
#' \item{id}{character column of animal IDs}
#' \item{sire}{the male parent of the animal indicated by the \code{id} column.}
#' \item{dam}{the female parent of the animal indicated by the \code{id}
#' column.}
#' \item{sex}{sex of the animal indicated by the \code{id} column.}
#' \item{gen}{generation number (integers beginning with 0 for the founder
#' generation) of the animal indicated by the \code{id} column.}
#' \item{birth}{birth date in \code{Date} format of the animal indicated by the
#'  \code{id} column.}
#' \item{exit}{exit date in \code{Date} format of the animal indicated by the
#'  \code{id} column.}
#' \item{age}{age in year (numeric) of the animal indicated by the \code{id}
#' column.}
#' }
"baboon_ped"
#' lacy1989Ped small example pedigree
#'
#' @source lacy1989Ped is a dataframe containing the small example pedigree used
#' by Robert C. Lacy in "Analysis of Founder Representation in Pedigrees:
#' Founder Equivalents and Founder Genome Equivalents" Zoo Biology 8:111-123
#' (1989).
#'
#' \describe{
#' \item{id}{character column of animal IDs}
#' \item{sire}{the male parent of the animal indicated by the \code{id} column.
#' Unknown sires are indicated with \code{NA}}
#' \item{dam}{the female parent of the animal indicated by the \code{id}
#' column.Unknown dams are indicated with \code{NA}}
#' \item{gen}{generation number (integers beginning with 0 for the founder
#' generation) of the animal indicated by the \code{id} column.}
#' \item{population}{logical vector with all values set TRUE}
#' }
"lacy1989Ped"
#' lacy1989PedAlleles is a dataframe produced by \code{geneDrop} on
#' \code{lacy1989Ped} with 5000 iterations.
#'
#' @source lacy1989Ped is a dataframe containing the small example pedigree used
#' by Robert C. Lacy in "Analysis of Founder Representation in Pedigrees:
#' Founder Equivalents and Founder Genome Equivalents" Zoo Biology 8:111-123
#' (1989).
#'
#' \describe{
#' There are 5000 columns, one for each iteration in \code{geneDrop}
#' containing alleles randomly selected at each
#' generation of the pedigree using Mendelian rules.
#'
#' Column 5001 is the \code{id} column with two rows for each member of the
#' pedigree (2 * 7).
#'
#' Column 5002 is the \code{parent} colun with values of \code{sire} and
#' \code{dam} alternating.
#' }
"lacy1989PedAlleles"
