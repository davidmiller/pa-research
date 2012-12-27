##
## Processing prescription data
##
## Based on the excellent work of https://github.com/MastodonC
##
require(reshape)

setwd("/home/david/src/ohc/data")

#' Calculate Spend totals by GP Practice
#'
#' @param [problemfile], "problem_drugs.csv"
#' @return data.frame
spend_totals <- function(problemfile="problem_drugs.csv"){

  ## load aggregate.data
  file.list<-read.csv("file_list.txt")$x
  spend.practice<-read.csv("spend_practice.csv")
  problem.drugs<-read.csv(problemfile)

  ## Merge spend with problem data
  spend.practice$item.pct<-spend.practice$items.thisdrug/spend.practice$items.alldrugs
  total.problem.spend<-merge(total.problem.spend,problem.drugs,all.x=TRUE)
  total.problem.spend$amount.wasted<-total.problem.spend$Spend*total.problem.spend$saving
  wasted.totals<-aggregate(total.problem.spend[,c("Spend","Items","amount.wasted")],
                           by=list("Drug"=total.problem.spend$Drug,
                                   "category"=total.problem.spend$category),
                           FUN=sum)

  ## Calculate waste per practice
  spend.practice<-merge(spend.practice,problem.drugs,all.x=TRUE)
  spend.practice$amount.wasted<-spend.practice$cost.thisdrug*as.numeric(spend.practice$saving)

  totals.headings<- list("Drug"=spend.practice$Drug,
                         "Practice.name"=spend.practice$V3,
                         "Practice.code"=spend.practice$Practice.code,
                         "Postcode"=spend.practice$V8,
                         "category"=spend.practice$category)
  spend.practice.totals<-
    aggregate(spend.practice[,c("cost.thisdrug","items.thisdrug",
                                "amount.wasted")],
              by=totals.headings,
              FUN=sum)

  temp.totals<-spend.practice[,c("Practice.code","Month","cost.alldrugs","items.alldrugs")]
  temp.totals<-temp.totals[!duplicated(temp.totals),]
  temp.totals<-aggregate(temp.totals[,c("cost.alldrugs","items.alldrugs")],
                         by=list("Practice.code"=temp.totals$Practice.code),FUN=sum)
  spend.practice.totals<-merge(spend.practice.totals,temp.totals,all.x=TRUE)

  spend.practice.totals$item.pct<-spend.practice.totals$items.thisdrug /
    spend.practice.totals$items.alldrugs

  return(spend.practice.totals)

}

#' Refine the set of prescriptions by interesting dimensions
#'
#' @param scrips Data frame of prescription data
#' @param practices Vector of practice codes
#'
#' @return data.frame
scripset <- function(scrips, practices=NULL){
  if(!is.null(practices)){
    myscrips <- subset(scrips, Practice.code %in% practices)
  }
  return(myscrips)
}

statinwittery <- function(statins){
  statins[statins$Drug %in% c("Atorvastatin",
                              "Rosuvastatin Calcium"),]$item.bad <- TRUE
  return(statins)
}

#' Locate Sartans that are Bad
sartanwittery <- function(sartans){
  sartans[sartans$Drug == "Candesartan Cilexetil",]$item.bad <- TRUE
  return(sartans)
}

#' Calculate Problem details by Practice.
#'
#' @param scrips data.frame of prescription details
#' @param months number of months we're dealing with
#' @param fun function to set $item.bad
#' @return data.frame
practice_mapping <- function(scrips, months, fun){
  scrips$item.bad <- FALSE
  scrips <- fun(scrips)
  practice.agg <- aggregate(
                            scrips$items.thisdrug,by=list(scrips$item.bad,
                                         scrips$Practice.code),FUN=sum)

  practice.agg <- cast(practice.agg,Group.2~Group.1)
  names(practice.agg) <- c("Practice.code","ok.drugs","problem.drugs")
  practice.agg$practice.problem <- practice.agg$problem.drugs/
    (practice.agg$problem.drugs+practice.agg$ok.drugs)

  practice.agg$total.items.month <- (practice.agg$problem.drugs+practice.agg$ok.drugs)/months
  practice.agg <- practice.agg[,c("Practice.code", "total.items.month", "practice.problem")]
  practice.agg$total.items.month <- round(practice.agg$total.items.month, 0)
  practice.agg$practice.problem <- round(practice.agg$practice.problem, 3)

  practice.agg <- subset(practice.agg, total.items.month > 0)
  practice.details <- unique(scrips[,c("Practice.code", "Practice.name", "Postcode")])
  practice.located <- merge(practice.agg, practice.details, all.x=TRUE)

  return(practice.located)

}
