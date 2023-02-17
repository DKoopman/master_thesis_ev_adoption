#policy analysis

#make use of the ev users model and the finaldf created in model.R and determine potential scores
final_df$potential <- predict(model_ev_users, final_df,
                              type = c("count"))
selection <- final_df[,c('BU_CODE', 'potential')]
selection$BU_CODE <- as.character(selection$BU_CODE)

#add potential to neighborhoodsMRA
neighborhoodsMRA <- left_join(neighborhoodsMRA, selection, by = 'BU_CODE')
neighborhoodsMRA$unused_potential <- neighborhoodsMRA$potential - neighborhoodsMRA$ev_users
neighborhoodsMRA$prediction_error <- neighborhoodsMRA$potential - neighborhoodsMRA$ev_users

#change negative potential to zero
neighborhoodsMRA <- neighborhoodsMRA %>% mutate(unused_potential = if_else(unused_potential < 0.5, 0, unused_potential))

#create plots
tm_shape(neighborhoodsMRA) +
  tm_fill(col = 'prediction_error') +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.5,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 0)


tm_shape(neighborhoodsMRA) +
  tm_fill(col = 'unused_potential', breaks = c(0,0.5,5,10,20,40)) +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.5,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 0)

#determine third quartile value
quantile(neighborhoodsMRA$occupancy_rate, na.rm= TRUE)

#mark neighborhoods as additional charger based on this value
neighborhoodsMRA <-neighborhoodsMRA %>% mutate(high_demand = if_else(occupancy_rate >=  0.3493349, 1, 0))
neighborhoodsMRA$additional_chargers <- neighborhoodsMRA$high_demand * neighborhoodsMRA$unused_potential
neighborhoodsMRA <-neighborhoodsMRA %>% mutate(additional_chargers = if_else(additional_chargers >  0, 1, 0))

#create plot
tm_shape(neighborhoodsMRA) +
  tm_fill(col = 'additional_chargers', breaks = c(0,0.5,1)) +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.5,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 0)

#do the same for the ev user/charger analysis
neighborhoodsMRA$usersPerCharger <- neighborhoodsMRA$ev_users/neighborhoodsMRA$charging_points
quantile(neighborhoodsMRA$usersPerCharger, na.rm = TRUE)
neighborhoodsMRA$potentialPerCharger <- neighborhoodsMRA$potential/neighborhoodsMRA$charging_points
neighborhoodsMRA <-neighborhoodsMRA %>% mutate(additional_chargers2 = if_else(potentialPerCharger >=  1.333333, 1, 0))

#create plot
tm_shape(neighborhoodsMRA) +
  tm_fill(col = 'additional_chargers2', breaks = c(0,0.5,1)) +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.5,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 0)
