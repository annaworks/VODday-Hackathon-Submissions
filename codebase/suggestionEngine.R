setwd("") #working dir of all files

library(jsonlite)
library(magrittr)
library(dplyr)
library(tidyr)

## read in data
clusteredDat <- read.csv("./clusteredDat.csv")
clusteredDat$address <- as.character(clusteredDat$address)

#* @get /suggest
suggestProperties <- function(streetnum) {

  ## get same clustered addresses
  tmp <- clusteredDat %>% filter(address == streetnum)
  if(dim(tmp)[1] > 1) {
    tmp <- tmp[1,]
  }
  sameClusterDat <- clusteredDat %>% filter(clusters == tmp$clusters)

  # ## not use addresses within a 0.5 km range of the address
  # not working yet
  # upperLat <- tmp$LATITUDE + 0.0005
  # leftLong <- tmp$LONGITUDE - 0.0005
  #
  # lowerLat <- tmp$LATITUDE - 0.0005
  # rightLong <- tmp$LONGITUDE + 0.0005
  #
  # outsideDat <- sameClusterDat %>%
  #   filter(LATITUDE >= upperLat, LATITUDE <= lowerLat, LONGITUDE <= rightLong, LONGITUDE >= leftLong)

  outsideDat <- sameClusterDat
  suggestedProps <- outsideDat %>% filter(as.numeric(as.character(CURRENT_LAND_VALUE)) <= as.numeric(as.character(tmp$CURRENT_LAND_VALUE)))

  results <- list(suggestedProps$address)

  return(results)
}
