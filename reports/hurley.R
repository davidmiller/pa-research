##
## Analysis of GP prescription data with a focus on
## a specific subset of practices.
##
source("../lib/prescriptions.R")

setwd("/home/david/src/ohc/pa-research/data")

hurleys <- read.csv("hurley_codes.csv")$code

totals <- scripset(spend_totals(), hurleys)
