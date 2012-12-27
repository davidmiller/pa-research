##
## Analysis of GP prescription data with a focus on
## a specific subset of practices.
##
source("/home/david/src/ohc/pa-research/lib/prescriptions.R")

setwd("/home/david/src/ohc/data")

## Load data
hurleys <- read.csv("hurley_codes.csv")$code
months <- length(read.csv("file_list.txt")$x)
totals <- scripset(spend_totals(), practices=hurleys)

## Statin analysis
statin.totals <- subset(totals, category=="statin")
practice.statin.totals <- practice_mapping(statin.totals, months, statinwittery)

## Sartan analysis
sartan.totals <- subset(totals,
                        category=="sartan" &
                        as.character(Drug) %in% c("Candesartan Cilexetil",
                                                  "Losartan Potassium"))
practice.sartan.totals <- practice_mapping(sartan.totals, months, sartanwittery)

## Rollup
names(practice.sartan.totals)[names(practice.sartan.totals)=="practice.problem"] <- "sartan.problem"
names(practice.sartan.totals)[names(practice.sartan.totals)=="total.items.month"] <- "sartan.items"
names(practice.statin.totals)[names(practice.statin.totals)=="practice.problem"] <- "statin.problem"
names(practice.statin.totals)[names(practice.statin.totals)=="total.items.month"] <- "statin.items"


hurling <- merge(practice.statin.totals, practice.sartan.totals)
write.csv(hurling, "hurley_practice_analysis3.csv", row.names=FALSE)

saves <- aggregate(sartan.totals[,c("items.thisdrug", "cost.thisdrug")],
                   by=list(sartan.totals$Drug),
                   FUN=sum)

statsaves <- aggregate(statin.totals[,c("items.thisdrug", "cost.thisdrug")],
                   by=list(statin.totals$Drug),
                   FUN=sum)
