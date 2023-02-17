#occupancy rate
options(java.parameters='-Xmx2g')
library(magrittr)
library(lubridate)
library(dplyr)
library(sp)
library(tidyverse)
library(DBI)
library(rJava)
library(RJDBC)

#read in mapping table and chargepoint data
KoppelPostcode <- read.csv(file = 'Postcode/pc6-gwb2020.csv', sep = ';')
DIM_CHARGEPOINT <- DIM_CHARGEPOINT(username, password)
DIM_CHARGEPOINT <- DIM_CHARGEPOINT[, c('ChargePoint_skey', 'NumberOfSockets')]

clear_chargingsession_occupancy <- function(chargingData){
  #clear rows with NA in critical columns
  chargingData <- chargingData[!is.na(chargingData$RFID_skey),]
  chargingData <- chargingData[!is.na(chargingData$kWh),]
  chargingData <- chargingData[!is.na(chargingData$IsValid),]
  chargingData <- chargingData[!is.na(chargingData$StartConnectionDateTime),]
  chargingData <- chargingData[!is.na(chargingData$EndConnectionDateTime),]
  chargingData <- chargingData[!is.na(chargingData$ChargePoint_skey),]
  chargingData <- chargingData[!is.na(chargingData$ConnectionTimeHours),]
  
  #clear rows with values that are not correct
  chargingData <- chargingData[chargingData$RFID_skey >= 0,]
  chargingData <- chargingData[chargingData$kWh > 0,]
  chargingData <- chargingData[chargingData$IsValid == 1,]
  chargingData <- chargingData[chargingData$ChargePoint_skey %in% relevant_chargers,]#relevant_chargers as in Relevant_charging_points script
  
  # determine sum of connectiontime hours per chargingpoint
  chargingData <- chargingData[,c("ChargePoint_skey", "kWh", "ConnectionTimeHours", "Location_skey")]
  chargingData <- chargingData %>% 
    group_by(ChargePoint_skey) %>% 
    summarise(Sum_hours = sum(ConnectionTimeHours), Location_skey = min(Location_skey) )
  
  #divide by the number of connectors at the chargingpoint
  chargingData <- left_join(chargingData, DIM_CHARGEPOINT)
  chargingData$Sum_hours <- chargingData$Sum_hours / chargingData$NumberOfSockets
  
  #get the location data and map by postcode to neighborhood
  DIM_LOCATION <- DIM_LOCATION(username, password)
  chargingData <- left_join(chargingData, DIM_LOCATION)
  chargingData <- chargingData[,c("ChargePoint_skey", "Sum_hours", "Location_skey", "PostalCode")]
  chargingData <- left_join(chargingData, KoppelPostcode, by = c("PostalCode" = "PC6"))
  
  #take the average of the chargingstation per neighborhood
  chargingData <- chargingData %>% 
    group_by(Buurt2020) %>% 
    summarise(avg_hours = mean(Sum_hours))
  return(chargingData)
}

months <- seq(from = 1, to = 12, by = 1)
years <- seq(from = 22, to = 22, by = 1)

#code to process all transaction data per month and save csv file with occupacy hours
for(year in years){
  for(month in months){
    if(length(month)<1){
      month <- paste("0",month,sep="")
    }
    date_string <- paste("20",year,"-",month,"-01",sep = "")
    start_date <- as.Date(date_string, "%Y-%m-%d")
    days_month <- days_in_month(start_date)
    date_string <- paste("20",year,"-",month,"-",days_month,sep = "")
    end_date <- as.Date(date_string, "%Y-%m-%d")
    start_date <- paste ("'", start_date, "'", sep = "")
    end_date <- paste ("'", end_date, "'", sep = "")
    print(end_date)
    
    data <- FACT_CHARGESESSION(username,password,country = "'NLD'" ,
                               startDateView = start_date, endDateView = end_date)
    data_transformed <- clear_chargingsession_occupancy(data)
    filename <- paste("Data_monthly_home_chargers_occupancy//", month,"20", year,".csv",sep ="")
    write.csv(data_transformed,filename, row.names = FALSE)
    print(paste('Processed file ',filename))
  }
}