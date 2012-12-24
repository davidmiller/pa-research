##
## Analysis of GP prescription data with a focus on
## a specific subset of practices.
##
source("/home/david/src/ohc/pa-research/lib/prescriptions.R")

setwd("/home/david/src/ohc/pa-research/data")

hurleys <- read.csv("hurley_codes.csv")$code
months <- length(read.csv("file_list.txt")$x)

totals <- scripset(spend_totals(), hurleys)

statin.totals <- subset(totals, category=="statin")
practice.statin.totals <- practice_mapping(statin.totals, months)

sartan.totals <- subset(totals, category="sartan")

write.csv(practice.statin.totals, "hurley_practice_statins.csv",
          row.names=FALSE)
