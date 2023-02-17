#This script returns the following:
# neighborhoodsMRA: geodf with the polygons of all neighborhoods that can be considered
#   in the modelling of ev adoption in the mra region. the valid column presents whether the 
#   neighborhood meets the requirements to be included in the modelling.
# relevant_chargers: all the relevant charging points that can be taken into consideration

#import dependencies and set correct settings
options(java.parameters='-Xmx2g')
library(magrittr)
library(lubridate)
library(dplyr)
library(sp)
library(sf)
library(tidyverse)
library(DBI)
library(rJava)
library(RJDBC)
library(stringr)
library(readr)
library(tmap)

#First run buurt_level_data script before this one

#Filter data based on DIM_ChargePoint table
chargingPoints <- DIM_CHARGEPOINT(username, password)
chargingPoints <- chargingPoints[chargingPoints$IsFastCharger == '0',] #however all charging points have value 0, is this correct?
chargingPoints <- chargingPoints[chargingPoints$LastActiveDateTime != chargingPoints$FirstActiveDateTime,] #so there is an interval
chargingPoints <- chargingPoints[chargingPoints$LastActiveDateTime >= chargingPoints$FirstActiveDateTime,] #interval should be positive
chargingPoints <- chargingPoints[chargingPoints$LastActiveDateTime >= "2022-01-01",] #interval should be positive

#decide the interval the charging point is 'open' as good as that can be done
chargingPoints <- chargingPoints %>%
  mutate(OperationInterval = difftime(LastActiveDateTime,FirstActiveDateTime, units = 'weeks'))
chargingPoints <- chargingPoints[chargingPoints$OperationInterval >= 4,]
chargingPoints <- chargingPoints[chargingPoints$status == 1,]
chargingPoints <- chargingPoints[chargingPoints$NumberOfSockets >= 1,]

#remove rows without ChargePoint_skey, 
chargingPoints <- chargingPoints %>% drop_na(ChargePoint_skey)
chargingPoints <- chargingPoints %>% drop_na(OperationInterval)

#merge with location table
LOOKUP_LOCATION_CHARGEPOINT_USER <- LOOKUP_LOCATION_CHARGEPOINT_USER(username,password)
chargingPoints <- left_join(chargingPoints, LOOKUP_LOCATION_CHARGEPOINT_USER)
DIM_LOCATION <- DIM_LOCATION(username, password)
chargingPoints <- left_join(chargingPoints, DIM_LOCATION)
chargingPoints <- chargingPoints %>% drop_na(PostalCode)

#Code to see the chargers per postcode
#test <- chargingPoints %>% group_by(PostalCode) %>% 
#  summarise(total_count=n(),
#            .groups = 'drop')
#rm(test)

#drop the following postcodes since they have more than 10 occurences and are manually checked and found invalid
chargingPoints <- chargingPoints[!chargingPoints$PostalCode == "3812PH",] #would have 235 chargers, are not in the area
chargingPoints <- chargingPoints[!chargingPoints$PostalCode == "3433PG",] #would have 210 chargers, are in the area but not on that postcode
chargingPoints <- chargingPoints[!chargingPoints$PostalCode == "3709JK",] #would have 42 chargers, are in the area but not on that postcode

#remove the duplicates
chargingPointsDuplicates <- chargingPoints[duplicated(chargingPoints[ , c("ChargePoint_skey")]), ]
chargingPointsNoDuplicates <- chargingPoints[!duplicated(chargingPoints[ , c("ChargePoint_skey")]), ]
relevant_chargers <- chargingPointsNoDuplicates$ChargePoint_skey
rm(chargingPointsDuplicates)

#count the number of chargers per postcode area
chargersPerPostcode <- chargingPointsNoDuplicates %>%
  group_by(PostalCode) %>%
  summarise(charging_points = n_distinct(ChargePoint_ID))

#load mapping tables
KoppelPostcodeBuurt <- read.csv(file = 'Postcode/brt2020.csv', sep = ';')
KoppelPostcode <- read.csv(file = 'Postcode/pc6-gwb2020.csv', sep = ';')

#join mapping tbales to shapefile
KoppelPostcode <- left_join(KoppelPostcode, KoppelPostcodeBuurt, by = c('Buurt2020' = 'buurtcode2020'))
KoppelPostcode <- KoppelPostcode[,c("PC6","Buurt2020","buurtnaam2020","GM2020", "GM_NAAM")]

#join the neighborhood data
chargersPerNeighborhood <- left_join(chargersPerPostcode, KoppelPostcode, by = c('PostalCode' = 'PC6'))
chargersPerNeighborhood <- chargersPerNeighborhood %>%
  group_by(Buurt2020) %>%
  summarise(charging_points = sum(charging_points))
sum(chargersPerNeighborhood$charging_points)
chargersPerNeighborhood <- chargersPerNeighborhood %>% drop_na(Buurt2020)

#read in neighborhood data
my_spdf_buurt <- read_sf( 
  dsn= paste0(getwd(),"/Buurtdata/WijkBuurtkaart_2020_v3/buurt_2020_v3.shp"),
)
my_spdf_buurt <- my_spdf_buurt[,c("BU_CODE", "AANT_INW", "WONINGEN", "AANTAL_HH")]
my_spdf_buurt <- my_spdf_buurt %>%
  mutate_at("BU_CODE", str_replace, "BU", "") %>%
  mutate_at("BU_CODE", str_remove, "^0+")
my_spdf_buurt$BU_CODE = as.numeric(as.character(my_spdf_buurt$BU_CODE))

#join with the chargersPerNeighborhood
chargersPerNeighborhood <- left_join(my_spdf_buurt, chargersPerNeighborhood, by = c('BU_CODE'= 'Buurt2020'))

#only select neighborhoods with citizens and homes
chargersPerNeighborhood$AANT_INW[chargersPerNeighborhood$AANT_INW <= 0] <- NA
chargersPerNeighborhood$WONINGEN[chargersPerNeighborhood$WONINGEN <= 0] <- NA
chargersPerNeighborhood$AANTAL_HH[chargersPerNeighborhood$AANTAL_HH <= 0] <- NA

#create column whether neighborhood can be taken into the modelling
chargersPerNeighborhood$Valid <- 1
chargersPerNeighborhood[is.na(chargersPerNeighborhood$AANT_INW), "Valid"] <- 0
chargersPerNeighborhood[is.na(chargersPerNeighborhood$WONINGEN), "Valid"] <- 0
chargersPerNeighborhood[is.na(chargersPerNeighborhood$AANTAL_HH), "Valid"] <- 0

#determing the available municipalities
unique_cities <- as.vector(LOOKUP_LOCATION_CHARGEPOINT_USER$City)
unique_cities <- unique(unique_cities)
unique_cities <- replace(unique_cities, unique_cities=="Bergen NH", "Bergen (NH.)")
unique_cities <- replace(unique_cities, unique_cities=="Ouder Amstel", "Ouder-Amstel")
unique_cities <- replace(unique_cities, unique_cities=="Stichtse vecht", "Stichtse Vecht")
unique_cities <- replace(unique_cities, unique_cities=="Ijsselstein", "IJsselstein")
unique_cities <- replace(unique_cities, unique_cities=="Edam Volendam", "Edam-Volendam")
unique_cities <- append(unique_cities, "Velsen") #for IJmuiden
unique_cities <- append(unique_cities, "Heerhugowaard") #for Dijk en Waard
unique_cities <- append(unique_cities, "Beemster") #for Middenbeemster
unique_cities <- append(unique_cities, "Langedijk") #for Middenbeemster
unique_cities <- append(unique_cities, "Edam-Volendam") #for Middenbeemster
unique_cities <- append(unique_cities, "Woudenberg") #for Middenbeemster

#join mapping tbales to shapefile
chargersPerNeighborhood <- left_join(chargersPerNeighborhood, KoppelPostcodeBuurt, by = c('BU_CODE' = 'buurtcode2020'))

#select only available municipalities
chargersPerNeighborhood$MRA_AREA <- 0
chargersPerNeighborhood$MRA_AREA[chargersPerNeighborhood$GM_NAAM %in% unique_cities] <- 1
chargersPerNeighborhoodMRA <- chargersPerNeighborhood[chargersPerNeighborhood$MRA_AREA == 1,]

#make plot of the chargingpoint in the MRA region
tm_shape(chargersPerNeighborhoodMRA) +
  tm_fill(col = 'charging_points') +
  tm_layout(legend.title.size = 0.5,
            legend.text.size = 0.3,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.8)

tm_shape(chargersPerNeighborhood) +
  tm_fill(col = 'MRA_AREA', legend.show = FALSE)

#create neighborhoodsMRA dataframe
neighborhoodsMRA <- chargersPerNeighborhoodMRA# %>% drop_na(charging_points)
neighborhoodsMRA[is.na(neighborhoodsMRA$charging_points), "Valid"] <- 0
neighborhoodsMRA <- neighborhoodsMRA[,c("BU_CODE", "AANT_INW", "charging_points", "Valid", "geometry")]

#remove variables
rm(unique_cities)
rm(KoppelPostcodeBuurt)
rm(chargingPoints)
rm(my_spdf_buurt)
rm(LOOKUP_LOCATION_CHARGEPOINT_USER)
rm(chargingPointsNoDuplicates)
