##
## Generate a two way table of Statin prescribing
## for further Statin analysis
##

source("prescriptions.R")
source("dispenseries.R")

stripchar <- function(x) str_trim(as.character(x))

totals <- spend_totals()
statin.totals <- subset(totals, category=="statin")
months <- length(read.csv("file_list.txt")$x)

practice.totals <- practice_mapping(statin.totals, months)

dispenseries <- get_dispenseries()

practice.totals$dispensing <- 0
practice.totals[stripchar(practice.totals$Postcode) %in% dispenseries$postcode, ]$dispensing <- 1

write.csv(practice.totals, "dispensing_practice_totals.csv", row.names=FALSE)
