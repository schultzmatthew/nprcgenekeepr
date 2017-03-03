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
###############################################################################
# Data Definition:
# 
# PedTree
# A list containing sire and dam information for an individual.
# 
###############################################################################

createPedTree <- function(ped){
  
  p <- rep(list(list(sire=NA, dam=NA)), nrow(ped))
  names(p) <- ped$id
  
  for(i in 1:nrow(ped)){
    p[[ped$id[i]]]$sire <- ped$sire[i]
    p[[ped$id[i]]]$dam <- ped$dam[i]
  }
  return(p)
}

getAncestors <- function(id, ptree){
  
  if(is.na(id)){
    return(c())
  }
  
  sire <- ptree[[id]]$sire
  dam <- ptree[[id]]$dam
  
  if(!is.na(sire)){
    s_anc <- getAncestors(sire, ptree)
    sire_lineage <- c(sire, s_anc)
  } else{
    sire_lineage <- c()
  }
  
  if(!is.na(dam)){
    d_anc <- getAncestors(dam, ptree)
    dam_lineage <- c(dam, d_anc)
  } else{
    dam_lineage <- c()
  }
  
  return(c(sire_lineage, dam_lineage))
}
#####
makesLoop <- function(id, ptree){
  
  s_anc <- getAncestors(ptree[[id]]$sire, ptree)
  d_anc <- getAncestors(ptree[[id]]$dam, ptree)
  overlap <- intersect(s_anc, d_anc)
  
  return(length(overlap) > 0)
}

findLoops <- function(ptree){
  
  ids <- names(ptree)
  loops <- vector("list", length(ids))
  names(loops) <- ids
  
  for(id in ids){
    if(makesLoop(id, ptree)){
      loops[[id]] <- TRUE
    } else{
      loops[[id]] <- FALSE
    }
  }
  return(loops)
}

countLoops <- function(loops, ptree){
  
  ids <- names(ptree)
  counts <- list()
  
  for(id in ids){
    anc <- getAncestors(id, ptree)
    l <- loops[c(id, anc)]
    counts[id] <- sum(unlist(l))
  }
  return(counts)
}







