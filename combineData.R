setwd("") #workign dir with the business_licences data and property_tax_report_latlong data from open data
# lat long was added to the property tax report with geocoder

# packages
library(dplyr)
library(magrittr)
library(tidyr)
library(clustMixType)

## businessdata
businessData <- read.csv("./business_licences.csv")
businessDataCleaned <- businessData %>%
  filter(Status == "Issued", !is.na(Latitude)) %>% mutate(business = paste(BusinessType, BusinessSubType, sep = " - ")) %>%
  dplyr::select(business, Latitude, Longitude)
businessDataCleaned[,"business"] <- as.factor(businessDataCleaned[,"business"])

## property data
propertyTax <- read.csv("./property_tax_report_latlong.csv")
p <- propertyTax %>%
  select(TO_CIVIC_NUMBER, STREET_NAME, ZONE_CATEGORY, CURRENT_LAND_VALUE, LONGITUDE, LATITUDE, NEIGHBOURHOOD_CODE) %>%
  filter(!is.na(CURRENT_LAND_VALUE))
p$LONGITUDE <- as.numeric(as.character(p$LONGITUDE))
p$LATITUDE <- as.numeric(as.character(p$LATITUDE))
p[,"address"] <- paste(p[,"TO_CIVIC_NUMBER"], p[,"STREET_NAME"], sep = " ")

## add summarised business data metrics for each propoerty
## should probably use doparallel to multicore this. so slow...
for (ii in 1:dim(p)[1]) {
  tmp <- p[ii,]

  ## identify the boundaries of a 2 km box around this address
  upperLat <- tmp$LATITUDE + 0.003
  leftLong <- tmp$LONGITUDE - 0.003

  lowerLat <- tmp$LATITUDE - 0.003
  rightLong <- tmp$LONGITUDE + 0.003

  ## find businesses within this box
  businessesWithinRegion <- businessDataCleaned %>%
    filter(Latitude <= upperLat, Latitude >= lowerLat, Longitude <= rightLong, Longitude >= leftLong)
  if (dim(businessesWithinRegion)[1] == 0) {
    tmpRow <- data.frame(business=levels(businessDataCleaned$business), total = 0) %>% spread(business, total)
  } else {
    businessesWithinRegion <- businessesWithinRegion %>% group_by(business) %>% summarise(total=n()) %>% complete(business) %>% as.data.frame()
    businessesWithinRegion$total[is.na(businessesWithinRegion$total)] <- 0
    tmpRow <- businessesWithinRegion %>% spread(business, total)
  }

  ## combine with property
  result <- cbind(tmp, tmpRow)
  if (!exists("results")) {
    results <- result
  } else {
    results <- rbind(results, result)
  }

}
## can expand to other data types e.g. vancouver parks data
write.csv(results, "aggregateDat.csv")

## clustering
clusters <- results[,-c(1,3,4,5)] %>% kproto(10)
results[,"clusters"] <- clusters$cluster

write.csv(results, "clusteredDat.csv")
