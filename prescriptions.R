##
## Processing prescription data
##
## Based on the excellent work of https://github.com/MastodonC
##
require(reshape)

setwd("/home/david/src/ohc/pa-research/data")

spend_totals <- function(){

  ## load aggregate.data
  file.list<-read.csv("file_list.txt")$x
  spend.practice<-read.csv("spend_practice.csv")
  problem.drugs<-read.csv("problem_drugs.csv")
  problem.spend<-read.csv("problem_spend.csv")
  total.problem.spend<-read.csv("total_problem_spend.csv")
  simva.price<-read.csv("simva_price.csv")
  simva.price<-simva.price[1,1]

  spend.practice$item.pct<-spend.practice$items.thisdrug/spend.practice$items.alldrugs
  total.problem.spend<-merge(total.problem.spend,problem.drugs,all.x=TRUE)
  total.problem.spend$amount.wasted<-total.problem.spend$Spend*total.problem.spend$saving
  wasted.totals<-aggregate(total.problem.spend[,c("Spend","Items","amount.wasted")],
                           by=list("Drug"=total.problem.spend$Drug,"category"=total.problem.spend$category),
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
  spend.practice.totals$item.pct<-spend.practice.totals$items.thisdrug/spend.practice.totals$items.alldrugs

  return(spend.practice.totals)

}

practice_mapping <- function(statins, months){
  statins$item.bad <- FALSE
  statins[statins$Drug %in% c("Atorvastatin","Rosuvastatin Calcium"),]$item.bad<-TRUE

  practice.agg <- aggregate(
                            statins$items.thisdrug,by=list(statins$item.bad,
                                         statins$Practice.code),FUN=sum)
  practice.agg <- cast(practice.agg,Group.2~Group.1)
  names(practice.agg) <- c("Practice.code","ok.drugs","problem.drugs")
  practice.agg$practice.problem <- practice.agg$problem.drugs/(
                                                               practice.agg$problem.drugs+practice.agg$ok.drugs)
  practice.agg$total.items.month <- (practice.agg$problem.drugs+practice.agg$ok.drugs)/months
  practice.agg <- practice.agg[,c("Practice.code", "total.items.month", "practice.problem")]
  practice.agg$total.items.month <- round(practice.agg$total.items.month, 0)
  practice.agg$practice.problem <- round(practice.agg$practice.problem, 3)

  practice.agg <- subset(practice.agg, total.items.month > 0)
  practice.details <- unique(statins[,c("Practice.code", "Practice.name", "Postcode")])
  practice.located <- merge(practice.agg, practice.details, all.x=TRUE)

  return(practice.located)

}

spend_two_way <- function(totals){

}
