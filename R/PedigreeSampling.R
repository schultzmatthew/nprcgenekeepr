# PedigreeSampling.R
# 2016-01-28
#
# Contains functions to build pedigrees from sub-samples
# of genotyped individuals.
#
# The goal of sampling is to reduce the number of inbreeding
# loops in the resulting pedigree, and thus, reduce the
# amount of time required to perform calculations with
# SIMWALK2 or similar programs.
#
#' Create a pedigree tree (PedTree).
#'
#' The PedTree is a list containing sire and dam information for an individual.
#'
#' @param ped dataframe of pedigree and demographic information potentially
#' containing columns indicating the birth and death dates of an individual.
#' The table may also contain dates of sale (departure). Optional columns
#' are \code{birth}, \code{death}, \code{departure}.
#'
#' @return A list of named lists forming a pedigree tree (PedTree or ptree).
#' Each sublist represents an ID in the pedigree and contains the sire ID and
#' the dam ID as named elements.
#' @export
createPedTree <- function(ped) {

  p <- rep(list(list(sire = NA, dam = NA)), nrow(ped))
  names(p) <- ped$id

  for (i in 1:nrow(ped)) {
    p[[ped$id[i]]]$sire <- ped$sire[i]
    p[[ped$id[i]]]$dam <- ped$dam[i]
  }
  return(p)
}
#' Recursively create a character vector of ancestors for an individual ID.
#'
#' @param id character vector of length 1 having the ID of interest
#' @param ptree a list of lists forming a pedigree tree as constructed by
#' \code{createPedTree(ped)} where \code{ped} is a standard pedigree dataframe.
#' @export
getAncestors <- function(id, ptree) {

  if (is.na(id)) {
    return(c())
  }

  sire <- ptree[[id]]$sire
  dam <- ptree[[id]]$dam

  if (!is.na(sire)) {
    s_anc <- getAncestors(sire, ptree)
    sire_lineage <- c(sire, s_anc)
  } else{
    sire_lineage <- c()
  }

  if (!is.na(dam)) {
    d_anc <- getAncestors(dam, ptree)
    dam_lineage <- c(dam, d_anc)
  } else{
    dam_lineage <- c()
  }

  return(c(sire_lineage, dam_lineage))
}
#' Tests to see if sires and dams for an individual in a ptree have a common
#' ancester.
#'
#' @return TRUE if one or more common ancestor exists
#' @param id character vector of length 1 having the ID of interest
#' @param ptree a list of lists forming a pedigree tree as constructed by
#' \code{createPedTree(ped)} where \code{ped} is a standard pedigree dataframe.
#' @export
makesLoop <- function(id, ptree) {

  s_anc <- getAncestors(ptree[[id]]$sire, ptree)
  d_anc <- getAncestors(ptree[[id]]$dam, ptree)
  overlap <- intersect(s_anc, d_anc)

  return(length(overlap) > 0)
}
#' Find loops in a pedigree tree
#'
#' @return a named list of logical values where each named element is
#' named with an \code{id} from \code{ptree}. The value of the list element
#' is set to \code{TRUE} if the \code{id} has a loop in the pedigree.
#' Loops occur when an animal's sire and dam have a common ancestor.
#'
#' @param ptree a list of lists forming a pedigree tree as constructed by
#' \code{createPedTree(ped)} where \code{ped} is a standard pedigree dataframe.
#' @export
findLoops <- function(ptree) {

  ids <- names(ptree)
  loops <- vector("list", length(ids))
  names(loops) <- ids

  for (id in ids) {
    if (makesLoop(id, ptree)) {
      loops[[id]] <- TRUE
    } else{
      loops[[id]] <- FALSE
    }
  }
  return(loops)
}
#' Count the number of loops in a pedigree tree.
#'
#' Uses the \code{loops} data structure and the list of all ancestors for
#' each individual to calculate the number of loops for each individual.
#'
#' @return A list indexed with each ID in the pedigree tree (\code{ptree})
#' containing the number of loops for each individual.
#'
#' @param loops a named list of logical values where each named element is
#' named with an \code{id} from \code{ptree}. The value of the list element
#' is set to \code{TRUE} if the \code{id} has a loop in the pedigree.
#' Loops occur when an animal's sire and dam have a common ancestor.
#' @param ptree a list of lists forming a pedigree tree as constructed by
#' \code{createPedTree(ped)} where \code{ped} is a standard pedigree dataframe.
#' @export
countLoops <- function(loops, ptree) {

  ids <- names(ptree)
  counts <- list()

  for (id in ids) {
    anc <- getAncestors(id, ptree)
    l <- loops[c(id, anc)]
    counts[id] <- sum(unlist(l))
  }
  return(counts)
}







