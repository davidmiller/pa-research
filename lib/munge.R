##
## Munging raw data for focus & clarity
##
## This file is "interesting" inasmuch as it mixes two approaches
## to dealing with NHS IC Prescription data.
##
## In the frist case, we have the approach based on the processing
## code from the initial PrescriptionAnalytics.com code.
##
## In the later case, we also have a subsequent, more modular heuristic
## aimed at a world where ad-hoc analysis of the data is likely to be
## required on a semi-frequent basis, suitable to modularising the code
## && designing for it's re-use based on the natural "foreign keys" into
## external datasets-
## * BNF Code
## * Practice Code
##
require(gdata)

#' Get BNF codes for a Pattern
#'
#' @param pattern
#' @return data.frame of interesting bnf codes
bnfgrep <- function(pattern){
  drugs <- read.csv("T201109PDP IEXT.CSV", header=TRUE)
  codename <- unique(drugs[,c("BNF.CODE", "BNF.NAME")])
  names(codename) <- c("code", "name")
  drug.list <- unique(codename$name)
  interesting <- drug.list[grep(pattern,drug.list)]
  interesting.codes <- subset(codename, name %in% interesting)
  return(interesting.codes)
}

#' Get GP Addresses
#'
#' @param filename
#' @return data.frame
get_addresses <- function(filename="T201204ADD REXT.CSV"){
  addresses <- read.csv(filename)
  addresses <- addresses[,c(2,3,6,8)]
  names(addresses) <- c("practice.code", "practice.name", "town", "postcode")
  return(addresses)
}

#' Data for a set of BNF drugs
#'
#' @param codes vector of BNF codes we want
#' @param infiles vector of filenames to read
#' @return data.frameaddrs <- get_addresses()
#' @examples
#' codes <- c("1234", "4657")
#' infiles <- c("frist.csv", next.csv")
#' myset <- bnfset(codes, infiles)
bnfset <- function(codes, infiles){
  ## addrs <- get_addresses()

  ## Load
  file.name <- infiles[1]
  print(file.name)
  scrips <- read.csv(file.name, header=TRUE)

  ## Clean
  scrips$BNF.NAME <- trim(scrips$BNF.NAME)

  ## Transform
  cost <- aggregate(scrips[,c("ACT.COST", "ITEMS")],
                    by=list(scrips$PRACTICE, scrips$PERIOD), FUN=sum)
  names(cost) <- c("practice.code", "month", "cost.all", "items.all")

  spend <- subset(scrips, BNF.CODE %in% codes)
  spend <- aggregate(spend[,c("ACT.COST", "ITEMS")],
                     by=list(spend$BNF.CODE, spend$PERIOD), FUN=sum)

  return(NULL)
}

#' Interesting Drugs to analyse
#'
#' @return Data Frame
get_interesting <- function(){
  GP.drugs <- read.csv("T201109PDP IEXT.CSV", header=TRUE)
  drug.list<-unique(GP.drugs$BNF.NAME)
  drug.list<-drug.list[order(drug.list)]
  statins<-drug.list[grep("statin",drug.list)]
  statins<-statins[-grep("Nystatin",statins)]
  clopidogrel<-c("Clopidogrel")
  plavix <- drug.list[grep("plavix", drug.list)]

  sartans<-c(
             "Azilsartan Medoxomil",
             "Candesartan Cilexetil",
             "Eprosartan",
             "Irbesartan",
             "Olmesartan Medoxomil",
             "Telmisartan",
             "Valsartan",
             "Losartan Potassium")

  problem.drugs<-as.data.frame(rbind(cbind(as.character(statins), "statin"),
                                     cbind(as.character(plavix), "plavix"),
                                     cbind(clopidogrel,"clopidogrel"),cbind(sartans,"sartan")))
  names(problem.drugs)<-c("Drug","category")
  problem.drugs$Drug<-trim(problem.drugs$Drug)
  return(problem.drugs)
}

#' Take IC data and munge it into a format for further analysis
#'
#' Writes CSV files with subsets of the NHS IC data.
scripclean <- function(interesting=NULL){
  ## Filenames for our raw analysis
  file.list <- read.csv("file_list.txt")$x

  addresses <- read.csv("T201204ADD REXT.CSV",header=FALSE)
  short.addresses<-addresses[,c(2,3,6,8)]

  if(is.null(interesting)){
    interesting.drugs <- get_interesting()
  }else{
    interesting.drugs <- interesting
  }


  ## Set up data frames for results
  total.problem.spend<-data.frame(matrix(nrow=0,ncol=4))
  spend.practice<-data.frame(matrix(nrow=0,ncol=11))
  spend.pct<-data.frame(matrix(nrow=0,ncol=6))
  spend.practice.total<-data.frame(matrix(nrow=0,ncol=4))

  ## Loop to load, analyse, and remove large data files
  for (i in 1:length(file.list)){
    file.name<-file.list[i]
    print(file.name)
    GP.drugs <- read.csv(file.name, header=TRUE)

    print("Sleeping 5")
    Sys.sleep(5)
    print("Woke up")

    GP.drugs$BNF.NAME<-trim(GP.drugs$BNF.NAME)
    surgery.subtotal<-aggregate(GP.drugs[,c("ACT.COST","ITEMS")],
                                by=list(GP.drugs$PRACTICE,GP.drugs$PERIOD),FUN=sum)
    names(surgery.subtotal)<-c("Practice.code","Month","cost.alldrugs","items.alldrugs")

    t<-subset(GP.drugs,BNF.NAME %in% problem.drugs$Drug)
    problem.spend<-aggregate(t[,c("ACT.COST","ITEMS")],by=list(t$BNF.NAME,t$PERIOD),FUN=sum)
    names(problem.spend)<-c("Drug","Period","Spend","Items")
    problem.spend$Spend<-round(problem.spend$Spend,digits=0)
    problem.spend$Drug<-as.character(problem.spend$Drug)
    total.problem.spend<-rbind(total.problem.spend,problem.spend)

    ## Calculations by practice
    s<-aggregate(t[,c("ACT.COST","ITEMS")],by=list(t$PRACTICE,t$PERIOD,t$BNF.NAME),FUN=sum)
    names(s)<-c("Practice.code","Month","Drug","cost.thisdrug","items.thisdrug")
    s<-merge(s,surgery.subtotal,all.x=TRUE)
    s<-merge(s,short.addresses,by.x="Practice.code",by.y="V2",all.x=TRUE)
    spend.practice<-rbind(spend.practice,s)

    print("End of file, sleeping 15")
    Sys.sleep(15)
    print("woke up")
  }

  ## Calculate Simvastatin 40mg price and other median actual prices
  preparation.level<-read.csv("T201206PDPI BNFT.CSV")
  simvastatin<-preparation.level[grep("Simvastatin_Tab 40mg",preparation.level$BNF.NAME),]
  simva.price<-median(simvastatin$ACT.COST/simvastatin$ITEMS)
  write.csv(simva.price,"simva_price.csv",row.names=FALSE)
  atorvastatin<-spend.practice[grep("Atorvastatin",spend.practice$Drug),]
  atorva.price<-median(atorvastatin$cost.thisdrug/atorvastatin$items.thisdrug)
  rosuvastatin<-spend.practice[grep("Rosuvastatin Calcium",spend.practice$Drug),]
  rosuva.price<-median(rosuvastatin$cost.thisdrug/rosuvastatin$items.thisdrug)

  ## Calculate and file savings figures
  problem.drugs$saving<-as.numeric(0)
  problem.drugs[problem.drugs$Drug=="Rosuvastatin Calcium",]$saving<-1-(simva.price/rosuva.price)
  problem.drugs[problem.drugs$Drug=="Atorvastatin",]$saving<-1-(simva.price/atorva.price)

  ## Write out summary files for main analysis process
  write.csv(spend.practice,"spend_practice.csv",row.names=FALSE,quote=FALSE)
  write.csv(problem.drugs,"problem_drugs.csv",row.names=FALSE,quote=FALSE)
  write.csv(problem.spend,"problem_spend.csv",row.names=FALSE,quote=FALSE)
  write.csv(total.problem.spend,"total_problem_spend.csv",row.names=FALSE,quote=FALSE)

}
