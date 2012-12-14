##
## Identifying Dispensing practices.
##
require(plyr)
require(stringr)

setwd("/home/david/src/ohc/pa-research/data")

get_dispenseries <- function(){

  dispensing.practices <- read.csv("dispensing_practices.csv")

  dispensing.practices <- subset(dispensing.practices,
                                 Dispensing.Practices.Address.Details != "Primary Care Trust:" &
                                 Dispensing.Practices.Address.Details != "Report For:" &
                                 Dispensing.Practices.Address.Details != "Practice Name and Address"
                                 )

  dispensing.practices <- dispensing.practices$Dispensing.Practices.Address.Details
  dispensing.practices <- data.frame(dispensing.practices)
  names(dispensing.practices) <- c("address")

  dispensing.practices <- ddply(dispensing.practices, .(address), summarise, postcode=get_pc(address))

  ## Clean data.
  dispensing.practices$postcode <- laply(dispensing.practices$postcode, function(x) sub("\n", " ", x))

  return(dispensing.practices)

}

get_pc <- function(address){

  return(
         str_trim(
                  tail(unlist(
                              strsplit(as.character(address), ",")
                              ),
                       n=1)
                       )
                  )

}
