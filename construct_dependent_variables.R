#This script process the EV adoption kpi's as produced by their corresponding scripts to
#and aggregates them for the whole year of 2022. Thereafter, it adds them to the 
#neighborhoodsMRA dataframe so that they can be used for modelling and plotting.

#import dependencies
library(dplyr)
library(stringr)
library(readr)

################################################################################
#occupancy rate

#combine the data of all months for the occupancy rates, based on the csv files written
#by the script occupancy_rate_kpi.R
l <- list()
l2 <- data.frame()
months <- seq(from = 1, to = 12, by = 1)
for(month in months){
  df <- read_csv(paste("Data_monthly_home_chargers_occupancy/", month, "2022.csv", sep = ""))
  l2 <- rbind(l2, df)
}

#take the mean values over the months
occupancy_rates <- l2 %>%
  group_by(Buurt2020) %>%
  summarise(occupancy_rate=mean(avg_hours),
            .groups = 'drop')

#devide by the average hours in a month
occupancy_rates$occupancy_rate <- occupancy_rates$occupancy_rate/730.48

#add to neighborhoodsMRA df, as made in relevant_charging_points.R
neighborhoodsMRA <- left_join(neighborhoodsMRA, occupancy_rates, by = c("BU_CODE" = "Buurt2020"))
neighborhoodsMRA[is.na(neighborhoodsMRA$occupancy_rate) & neighborhoodsMRA$Valid == 1, "occupancy_rate"] <- 0
neighborhoodsMRA[neighborhoodsMRA$Valid == 0, "occupancy_rate"] <- NA
rm(occupancy_rates)

#create statistics and histogram
occupancy_rate <- neighborhoodsMRA$occupancy_rate
hist(occupancy_rate)
mean(neighborhoodsMRA$occupancy_rate, na.rm = TRUE)
sd(neighborhoodsMRA$occupancy_rate, na.rm = TRUE)

#make plot of MRA region with occupancy rates
tm_shape(neighborhoodsMRA) +
  tm_fill(col = 'occupancy_rate', breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.45,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 0)


################################################################################
#ev users

#combine the data of all months for the ev users, based on the csv files written
#by the script ev_users.R
l <- list()
l2 <- data.frame()
months <- seq(from = 1, to = 12, by = 1)
for(month in months){
  df <- read_csv(paste("Data_monthly_home_chargers_rfid/", month, "2022_rfid.csv", sep = ""))
  avector <- as.vector(df['Homechargers'])
  l <- append(l,avector)
  l2 <- rbind(l2, df)
}
l2 <- separate_rows(l2, Homechargers, sep = "\\, ")

#seperate the values in one row to multiple rows
vec <- Reduce(c,l)
vec <- unlist(strsplit(vec, "\\, "))
v1 <- table(vec)
rfids <- as.integer(names(v1)[v1 >= 4])
#delete first element since this is the 0 occurrences
rfids <- rfids[-1]

#create new dataframe with ev users per postocde
ev_users_df <- data.frame()
for(rfid in rfids){
  selection <- l2[l2$Homechargers == rfid,]
  unique_values <- length(unique(selection$Postcodes))
  if(unique_values == 1){
    ev_users_df <- rbind(ev_users_df, selection[1,])
  }else{
    ev_users_df <- rbind(ev_users_df, selection)
  }
}

#add neighborhood data to the ev_users_df
ev_users_df <- left_join(ev_users_df, KoppelPostcode, by = c('Postcodes'= 'PC6'))
drop <- c('buurtnaam2020','GM2020', 'GM_NAAM')
ev_users_df = ev_users_df[,!(names(ev_users_df) %in% drop)]

#group per neighborhood and count the distinct rfid's
home_chargers_per_buurt <- ev_users_df%>%
  group_by(Buurt2020) %>% 
  summarise(ev_users=n_distinct(Homechargers),
            .groups = 'drop')
home_chargers_per_buurt$ev_users[is.na(home_chargers_per_buurt$ev_users)] <- 0

#add the data to the neighborhoodsMRA dataframe and replace na values by 0
neighborhoodsMRA <- left_join(neighborhoodsMRA, home_chargers_per_buurt, by = c("BU_CODE" = "Buurt2020"))
neighborhoodsMRA[is.na(neighborhoodsMRA$ev_users) & neighborhoodsMRA$Valid == 1, "ev_users"] <- 0
rm(home_chargers_per_buurt)

#give the statistics and make a histogram of the distribution
ev_users <- neighborhoodsMRA$ev_users
hist(ev_users, breaks = seq(from=0, to=42, by=1))
max(ev_users, na.rm = TRUE)
mean(ev_users, na.rm = TRUE)
sd(ev_users, na.rm = TRUE)
rm(ev_users)

#make a plot of the number of ev users for the MRA region
tm_shape(neighborhoodsMRA) +
  tm_fill(col = 'ev_users', breaks = c(0,1,5,10,15,20,45)) +
  tm_layout(legend.title.size = 1.1,
            legend.text.size = 0.5,
            legend.width = 1,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 0)

#remove variables
rm(l)
rm(l2)
rm(avector)
rm(selection)
