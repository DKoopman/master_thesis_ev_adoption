#import dependencies and set correct settings
options(java.parameters='-Xmx2g')
library(magrittr)
library(lubridate)
library(dplyr)
library(sp)
library(tidyverse)
library(DBI)
library(rJava)
library(RJDBC)

#function to transform the data
clear_chargingsession_rfid <- function(chargingData){
  #clear rows with NA in critical columns
  chargingData <- chargingData[!is.na(chargingData$RFID_skey),]
  chargingData <- chargingData[!is.na(chargingData$UseType),]
  chargingData <- chargingData[!is.na(chargingData$kWh),]
  chargingData <- chargingData[!is.na(chargingData$IsValid),]
  chargingData <- chargingData[!is.na(chargingData$StartConnectionDateTime),]
  chargingData <- chargingData[!is.na(chargingData$EndConnectionDateTime),]
  chargingData <- chargingData[!is.na(chargingData$ChargePoint_skey),]
  
  #clear rows with values that are not correct
  chargingData <- chargingData[chargingData$UseType == 'regulier',]
  chargingData <- chargingData[chargingData$RFID_skey >= 0,]
  chargingData <- chargingData[chargingData$kWh > 0,]
  chargingData <- chargingData[chargingData$IsValid == 1,]
  #relevant_chargers as in Relevant_charging_points script
  chargingData <- chargingData[chargingData$ChargePoint_skey %in% relevant_chargers,]
  
  #now select only rfid's that have less than 50 charging sessions per month
  chargeSessionsPerMonth <- chargingData
  chargeSessionsPerMonth <- chargeSessionsPerMonth %>%
    mutate(
      start_month = month(chargeSessionsPerMonth$StartConnectionDateTime),
      year = year(chargeSessionsPerMonth$StartConnectionDateTime)
    )
  
  monthlySessions <-  chargeSessionsPerMonth %>% group_by(start_month, year, RFID_skey) %>% 
    summarise(sessionsMonth=n(),
              .groups = 'drop')
  delete_overusage_users <- monthlySessions[monthlySessions$sessionsMonth >= 50,]$RFID_skey
  chargingData <- chargingData[!chargingData$RFID_skey %in% delete_overusage_users, ]
  
  #now get the starting time of the charge sessions
  chargingData <- chargingData %>%
    mutate(
      start_hour = hour(chargingData$StartConnectionDateTime)
    )
  
  #select only transactions that started between 16pm and 4 am
  chargers_evening <- chargingData[chargingData$start_hour >= 16,]
  chargers_night <- chargingData[chargingData$start_hour < 4,]
  chargingData <- rbind(chargers_evening, chargers_night)
  
  #add location data to charging data
  DIM_LOCATION <- DIM_LOCATION(username, password)
  charingPointsData <- left_join(chargingData, DIM_LOCATION)
  
  #delete sessions where no postcode is known
  charingPointsData <- charingPointsData[!is.na(charingPointsData$PostalCode),]
  charingPointsData <- charingPointsData[!is.na(charingPointsData$Location_skey),]
  
  #now select only the transactions of people that charge more than five times a month as EV user
  #this has to be done with geo analysis since nearby points are also valid
  possible_ev_users <- chargingData$RFID_skey
  possible_ev_users <- unique(possible_ev_users)
  
  #create dataframe to save ev users
  Postcodes <- unique(chargersPerPostcode$PostalCode)
  Homechargers <- rep(0,length(Postcodes))
  df <- data.frame(Postcodes, Homechargers)
  df <- na.omit(df)
  
  #create progresbar
  pb <- txtProgressBar(min=0, max=length(possible_ev_users), style=3, width=50, char="=")
  
  for(i in seq_along(possible_ev_users)){
    #select charging sessions for that rfid
    selection <- charingPointsData[charingPointsData$RFID_skey == possible_ev_users[[i]],]
    distinct_locations <- length(unique(selection$Location_skey))
    distinct_sessions <- length(unique(selection$ChargeSession_skey))
    
    if(distinct_sessions > 5){
      #group by charger and get occurrences and sort descending
      chargers <- selection %>%
        group_by(Location_skey) %>% 
        summarise(occurences=n(),
                  .groups = 'drop') %>%
        arrange(desc(occurences))
      
      if(chargers[1,'occurences'] > 5){
        #add the $Location_skey since it is a tibble
        skey <- chargers[1,"Location_skey"]$Location_skey
        location_charger = selection[selection$Location_skey == skey,]
        postal_code <- location_charger[1,"PostalCode"]
        value <- df[df$Postcodes == postal_code,"Homechargers"]
        if(length(value) != 0 && value == 0){
          df[df$Postcodes == postal_code,"Homechargers"] <- c(possible_ev_users[[i]])
        }else{
          #add to list
          value <- append(value,possible_ev_users[[i]])
          df[df$Postcodes == postal_code,"Homechargers"] <- toString(value)
        }
      }else{
        #create a coordinate matrix
        coordinates <- subset(selection, select =  c("Latitude", "Longitude"))
        coordinates <- matrix(unlist(coordinates), ncol = 2, byrow = FALSE)
        
        #count the number of points that are within 200 meters of each point
        close_points <- apply(spDists(coordinates, longlat=TRUE), 2, 
                              function(x) paste(length(which(x < 1.2)), collapse=', '))
        
        #get the maximum charging sessions within 200 meter radius
        max_charging_sessions = max(close_points)
        
        #if the maximum occurred charging sessions in a 200 meter radius exceeds 5 than count the observation
        if(max_charging_sessions > 5){
          #take the postcode of the charger with the highest number of chargin session for the user
          max_charger = which.max(close_points)
          max_occurence_charger <- selection$Location_skey[[max_charger]]
          
          location_charger = selection[selection$Location_skey == max_occurence_charger,]
          postal_code <- location_charger[1,"PostalCode"]
          value <- df[df$Postcodes == postal_code,"Homechargers"]
          if(length(value) != 0 && value == 0){
            df[df$Postcodes == postal_code,"Homechargers"] <- c(possible_ev_users[[i]])
          }else{
            #add new rfid user to list
            value <- append(value,possible_ev_users[[i]])
            df[df$Postcodes == postal_code,"Homechargers"] <- toString(value)
          }
          setTxtProgressBar(pb, i)
        }
      }
    }else{
      setTxtProgressBar(pb, i)
    }
  }
  return(df)
}

#select month to run
months <- seq(from = 1, to = 12, by = 1)
years <- seq(from = 22, to = 22, by = 1)

#code to create ev users per postcode per month and write to csv
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
    data_transformed <- clear_chargingsession_rfid(data)
    filename <- paste("Data_monthly_home_chargers_rfid//", month,"20", year,"_rfid.csv",sep ="")
    write.csv(data_transformed,filename, row.names = FALSE)
    print(paste('Processed file ',filename))
  }
}
