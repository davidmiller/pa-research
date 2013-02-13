##
## Analysis of prescribing data to get a practice & CCG level rollup
## of the ratio of harmful inhaler prescribing
##
require(reshape)

setwd("/home/david/src/ohc/data")

months <- 22

## Load data
metered <- read.csv("metered.txt")
names(metered) <- c("bnf.code")
inhaler.practice <- read.csv("presentation/spend_practice.inh.csv")

## Establish buckets
inhaler.practice$metered <- FALSE
inhaler.practice[inhaler.practice$Drug %in% metered$bnf.code, ]$metered <- TRUE

## Aggregates over all time
inhaler.agg <- aggregate(inhaler.practice$items.thisdrug,
                         by=list(
                           inhaler.practice$metered,
                           inhaler.practice$Practice.code
                           ),
                         FUN=sum)
inhaler.agg <- cast(inhaler.agg,Group.2~Group.1)
names(inhaler.agg) <- .(practice.code, powder, metered)

## Calculate ratios
inhaler.agg$problem <- inhaler.agg$metered / (inhaler.agg$powder + inhaler.agg$metered)
inhaler.agg$problem <- round(inhaler.agg$problem, 3)
inhaler.agg$total.per.month <- (inhaler.agg$metered+inhaler.agg$powder)/months
inhaler.agg$total.per.month <- round(inhaler.agg$total.per.month, 0)
inhaler.agg$problem <- inhaler.agg$problem * 100

write.csv(inhaler.agg, "inhaler.ratios.by.practice.csv", row.names=TRUE)
