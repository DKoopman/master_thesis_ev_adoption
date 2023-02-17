#This file processes and prepares the independent variables and writes them to a csv file

#import dependencies
library("writexl")

#load the buurt shape file
my_spdf_buurt <- read_sf( 
  dsn= paste0(getwd(),"/Buurtdata/WijkBuurtkaart_2020_v3/buurt_2020_v3.shp"),
)

#green_votes
df_green_votes <- read.csv("Upload_data_sets/greenVotes.csv")
df_green_votes <- na.omit(df_green_votes)
df_green_votes <- df_green_votes %>%
  mutate_at("BU_CODE", str_replace, "BU", "") %>%
  mutate_at("BU_CODE", str_remove, "^0+")
df_green_votes <- df_green_votes[,c('BU_CODE', 'Green_votes_perc')]
names(df_green_votes)[names(df_green_votes) == 'Green_votes_perc'] <- 'Perc_green_votes'


#city_centre
df_city_centre <- my_spdf_buurt[,c("BU_CODE","AF_TREINST")]
df_city_centre[df_city_centre$AF_TREINST == -99999999,2] <- NA
df_city_centre$Has_city_centre <- as.numeric(cut(df_city_centre$AF_TREINST, 65))
df_city_centre[is.na(df_city_centre$Has_city_centre),'Has_city_centre'] <- 0
df_city_centre[df_city_centre$Has_city_centre > 1,'Has_city_centre'] <- 0
df_city_centre <- df_city_centre %>% st_drop_geometry()
df_city_centre <- df_city_centre %>%
  mutate_at("BU_CODE", str_replace, "BU", "") %>%
  mutate_at("BU_CODE", str_remove, "^0+")
names(df_city_centre)[names(df_city_centre) == 'AF_TREINST'] <- 'Dist_train_station'

#income
df_income <- my_spdf_buurt[,c("BU_CODE","P_HOOGINKH", "P_LAAGINKH")]
df_income[df_income$P_HOOGINKH == -99999999,2] <- NA
df_income[df_income$P_LAAGINKH == -99999999,3] <- NA
df_income <- df_income %>%
  mutate_at("BU_CODE", str_replace, "BU", "") %>%
  mutate_at("BU_CODE", str_remove, "^0+")
df_income <- df_income %>% st_drop_geometry()
names(df_income)[names(df_income) == 'P_HOOGINKH'] <- 'Perc_high_income'
names(df_income)[names(df_income) == 'P_LAAGINKH'] <- 'Perc_low_income'

#education
df_education <- my_spdf_buurt[,c("BU_CODE", "A_OPL_HG", "A_OPL_MD", "A_OPL_LG", "AANT_INW")]
df_education[df_education$A_OPL_HG == -99999999,2] <- NA
df_education[df_education$A_OPL_MD == -99999999,3] <- NA
df_education[df_education$A_OPL_LG == -99999999,4] <- NA
df_education[df_education$AANT_INW == -99999999,5] <- NA
df_education$P_OPL_HG <- df_education$A_OPL_HG/df_education$AANT_INW
df_education$P_OPL_MD <- df_education$A_OPL_MD/df_education$AANT_INW
df_education$P_OPL_LG <- df_education$A_OPL_LG/df_education$AANT_INW
df_education <- df_education[,c('BU_CODE','P_OPL_HG', 'P_OPL_LG')]
df_education <- df_education %>%
  mutate_at("BU_CODE", str_replace, "BU", "") %>%
  mutate_at("BU_CODE", str_remove, "^0+")
df_education <- df_education %>% st_drop_geometry()
names(df_education)[names(df_education) == 'P_OPL_HG'] <- 'Perc_high_educated'
names(df_education)[names(df_education) == 'P_OPL_LG'] <- 'Perc_low_educated'


#age
df_age <- my_spdf_buurt[,c("BU_CODE", "P_00_14_JR","P_15_24_JR","P_25_44_JR", "P_45_64_JR", "P_65_EO_JR")]
df_age[df_age$P_00_14_JR == -99999999,2] <- NA
df_age[df_age$P_15_24_JR == -99999999,3] <- NA
df_age[df_age$P_25_44_JR == -99999999,4] <- NA
df_age[df_age$P_45_64_JR == -99999999,5] <- NA
df_age[df_age$P_65_EO_JR == -99999999,6] <- NA
df_age <- df_age[,c('BU_CODE', 'P_15_24_JR', 'P_25_44_JR', 'P_45_64_JR', 'P_65_EO_JR')]
df_age <- df_age %>% st_drop_geometry()
df_age <- df_age %>%
  mutate_at("BU_CODE", str_replace, "BU", "") %>%
  mutate_at("BU_CODE", str_remove, "^0+")
names(df_age)[names(df_age) == 'P_15_24_JR'] <- 'Perc_15_24_yr'
names(df_age)[names(df_age) == 'P_25_44_JR'] <- 'Perc_25_44_yr'
names(df_age)[names(df_age) == 'P_45_64_JR'] <- 'Perc_45_64_yr'
names(df_age)[names(df_age) == 'P_65_EO_JR'] <- 'Perc_65_EO_yr'

#self employed
df_self_employed <- my_spdf_buurt[,c("BU_CODE", "P_ARB_ZS")]
df_self_employed[df_self_employed$P_ARB_ZS == -99999999,2] <- NA
df_self_employed <- df_self_employed %>% st_drop_geometry()
df_self_employed <- df_self_employed %>%
  mutate_at("BU_CODE", str_replace, "BU", "") %>%
  mutate_at("BU_CODE", str_remove, "^0+")
names(df_self_employed)[names(df_self_employed) == 'P_ARB_ZS'] <- 'Perc_self_employed'

#housing type
df_house_type <- my_spdf_buurt[,c("BU_CODE", "P_MGEZW")]
df_house_type[df_house_type$P_MGEZW == -99999999,3] <- NA
df_house_type <- df_house_type[,c("BU_CODE","P_MGEZW")]
df_house_type <- df_house_type %>% st_drop_geometry()
df_house_type <- df_house_type %>%
  mutate_at("BU_CODE", str_replace, "BU", "") %>%
  mutate_at("BU_CODE", str_remove, "^0+")
names(df_house_type)[names(df_house_type) == 'P_MGEZW'] <- 'Perc_multi_house'

#housing ownership
df_house_own <- my_spdf_buurt[,c("BU_CODE", "P_HUURWON")]
df_house_own[df_house_own$P_HUURWON == -99999999,2] <- NA
df_house_own <- df_house_own[,c("BU_CODE","P_HUURWON")]
df_house_own <- df_house_own %>% st_drop_geometry()
df_house_own <- df_house_own %>%
  mutate_at("BU_CODE", str_replace, "BU", "") %>%
  mutate_at("BU_CODE", str_remove, "^0+")
names(df_house_own)[names(df_house_own) == 'P_HUURWON'] <- 'Perc_rental_house'

#density
df_density <- my_spdf_buurt[,c("BU_CODE", "BEV_DICHTH")]
df_density[df_density$BEV_DICHTH == -99999999,2] <- NA
df_density <- df_density %>% st_drop_geometry()
df_density <- df_density %>%
  mutate_at("BU_CODE", str_replace, "BU", "") %>%
  mutate_at("BU_CODE", str_remove, "^0+")
names(df_density)[names(df_density) == 'BEV_DICHTH'] <- 'Population_density'


#household_size
df_household_size <- my_spdf_buurt[,c("BU_CODE", "GEM_HH_GR", "P_HH_M_K")]
df_household_size[df_household_size$GEM_HH_GR == -99999999,2] <- NA
df_household_size[df_household_size$P_HH_M_K == -99999999,3] <- NA
df_household_size <- df_household_size %>% st_drop_geometry()
df_household_size <- df_household_size %>%
  mutate_at("BU_CODE", str_replace, "BU", "") %>%
  mutate_at("BU_CODE", str_remove, "^0+")
names(df_household_size)[names(df_household_size) == 'GEM_HH_GR'] <- 'Avg_household_size'
names(df_household_size)[names(df_household_size) == 'P_HH_M_K'] <- 'Perc_household_child'

#shops
df_shops <- my_spdf_buurt[,c("BU_CODE", "AF_APOTH", "AF_SUPERM", "AF_WARENH", "AF_RESTAU")]
df_shops[df_shops$AF_APOTH == -99999999,2] <- NA
df_shops[df_shops$AF_SUPERM == -99999999,3] <- NA
df_shops[df_shops$AF_WARENH == -99999999,4] <- NA
df_shops[df_shops$AF_RESTAU == -99999999,5] <- NA
df_shops$shop_distance <- (df_shops$AF_APOTH + df_shops$AF_SUPERM + df_shops$AF_WARENH + df_shops$AF_RESTAU)/4
df_shops <- df_shops %>% st_drop_geometry()
df_shops <- df_shops %>%
  mutate_at("BU_CODE", str_replace, "BU", "") %>%
  mutate_at("BU_CODE", str_remove, "^0+")
df_shops <- df_shops[,c('BU_CODE', 'shop_distance')]
names(df_shops)[names(df_shops) == 'shop_distance'] <- 'Dist_shops'

#public points
df_public_point <- my_spdf_buurt[,c("BU_CODE", "AF_ATTRAC", "AF_MUSEUM", "AF_ZWEMB")]
df_public_point[df_public_point$AF_ATTRAC == -99999999,2] <- NA
df_public_point[df_public_point$AF_MUSEUM == -99999999,3] <- NA
df_public_point[df_public_point$AF_ZWEMB == -99999999,4] <- NA
df_public_point <- transform(df_public_point, public_distance = pmin(AF_ATTRAC, AF_MUSEUM, AF_ZWEMB))
df_public_point$has_public_point <- as.numeric(cut(df_public_point$public_distance, 27))
df_public_point[is.na(df_public_point$has_public_point),'has_public_point'] <- 0
df_public_point[df_public_point$has_public_point >1, 'has_public_point'] <- 0
df_public_point <- df_public_point %>% st_drop_geometry()
df_public_point <- df_public_point[,c('BU_CODE', 'has_public_point')]
df_public_point <- df_public_point %>%
  mutate_at("BU_CODE", str_replace, "BU", "") %>%
  mutate_at("BU_CODE", str_remove, "^0+")
names(df_public_point)[names(df_public_point) == 'has_public_point'] <- 'Has_public_point'

#hospitals
df_hospital <- my_spdf_buurt[,c("BU_CODE", "AF_ZIEK_E")]
df_hospital[df_hospital$AF_ZIEK_E == -99999999,2] <- NA
df_hospital$has_hospital <- as.numeric(cut(df_hospital$AF_ZIEK_E, 80))
df_hospital[is.na(df_hospital$AF_ZIEK_E),'has_hospital'] <- 0
df_hospital[df_hospital$has_hospital >1, 'has_hospital'] <- 0
df_hospital <- df_hospital %>% st_drop_geometry()
df_hospital <- df_hospital %>%
  mutate_at("BU_CODE", str_replace, "BU", "") %>%
  mutate_at("BU_CODE", str_remove, "^0+")
names(df_hospital)[names(df_hospital) == 'has_hospital'] <- 'Has_hospital'
names(df_hospital)[names(df_hospital) == 'AF_ZIEK_E'] <- 'Dist_hospital'

#schools
df_school <- my_spdf_buurt[,c("BU_CODE", "AF_ONDBAS")]
df_school[df_school$AF_ONDBAS == -99999999,2] <- NA
df_school$has_school <- as.numeric(cut(df_school$AF_ONDBAS, 10))
df_school[is.na(df_school$AF_ONDBAS),'has_school'] <- 0
df_school[df_school$has_school >1, 'has_school'] <- 0
df_school <- df_school %>% st_drop_geometry()
df_school <- df_school %>%
  mutate_at("BU_CODE", str_replace, "BU", "") %>%
  mutate_at("BU_CODE", str_remove, "^0+")
names(df_school)[names(df_school) == 'AF_ONDBAS'] <- 'Dist_school'
names(df_school)[names(df_school) == 'has_school'] <- 'Has_school'

#cars density
df_cars <- read.csv("Upload_data_sets/df_cars.csv")
df_cars <- df_cars[,c('BU_CODE', 'G_PAU_KM')]
temp <- my_spdf_buurt[,c("BU_CODE", "AANT_INW")]
df_cars <- left_join(df_cars, temp)
df_cars <- df_cars %>%
  mutate_at("BU_CODE", str_replace, "BU", "") %>%
  mutate_at("BU_CODE", str_remove, "^0+")
df_cars$car_density <- df_cars$G_PAU_KM/df_cars$AANT_INW
df_cars <- df_cars[,c('BU_CODE','car_density')]
names(df_cars)[names(df_cars) == 'car_density'] <- 'Car_density'

#prepare solar panel data
#read in data from csv
df_solar_panels <- read.csv("Upload_data_sets/Zonnestroom.csv", sep =";")
#consider only neighborhoods
df_solar_panels <- df_solar_panels[grep("BU", df_solar_panels$WijkenEnBuurten), ]
#select relevant columns and change names
df_solar_panels <- df_solar_panels[,c("WijkenEnBuurten", "OpgesteldVermogenVanZonnepanelen_6")]
names(df_solar_panels)[names(df_solar_panels) == 'WijkenEnBuurten'] <- 'BU_CODE'
names(df_solar_panels)[names(df_solar_panels) == 'OpgesteldVermogenVanZonnepanelen_6'] <- 'Solar_power_kw'
#set all NaN values to 0
df_solar_panels[is.na(df_solar_panels)] <- 0
#strip the first parth of the Buurt code so it matches the other data
df_solar_panels <- df_solar_panels %>%
  mutate_at("BU_CODE", str_replace, "BU", "") %>%
  mutate_at("BU_CODE", str_remove, "^0+")
df_solar_panels$BU_CODE = as.character(df_solar_panels$BU_CODE)
names(df_solar_panels)[names(df_solar_panels) == 'Solar_power_kw'] <- 'Solar_power'

#construct dataframe
basis_df <- neighborhoodsMRA[, c('BU_CODE')]
basis_df$BU_CODE = as.character(basis_df$BU_CODE)

#add all the dataframes together
df_independent <- purrr::reduce(list(basis_df, df_solar_panels, df_cars, df_income, df_school, df_public_point,
                                     df_house_own, df_house_type, df_household_size, df_hospital,
                                     df_green_votes, df_education, df_density, df_shops, 
                                     df_self_employed, df_city_centre, df_age), dplyr::left_join, by = 'BU_CODE')

#drop the geometry and write to file
df_independent <- df_independent %>% st_drop_geometry()
write_xlsx(df_independent,'independent_variables.xlsx')

