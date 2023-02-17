#prepare solar panel data
#read in data from csv
df_solar_panels <- read.csv("Upload_data_sets/Zonnestroom__wijken_en_buurten__2019_14122022_103834.csv", sep =";")
#consider only neighborhoods
df_solar_panels <- df_solar_panels[grep("BU", df_solar_panels$Regioaanduiding.Codering..code.), ]
#select relevant columns and change names
df_solar_panels <- df_solar_panels[,c("Regioaanduiding.Codering..code.", "Opgesteld.vermogen.van.zonnepanelen..kW.")]
names(df_solar_panels)[names(df_solar_panels) == 'Regioaanduiding.Codering..code.'] <- 'BU_CODE'
names(df_solar_panels)[names(df_solar_panels) == 'Opgesteld.vermogen.van.zonnepanelen..kW.'] <- 'Solar_power_kw'
#set all NaN values to 0
df_solar_panels[is.na(df_solar_panels)] <- 0
#strip the first parth of the Buurt code so it matches the other data
df_solar_panels <- df_solar_panels %>%
  mutate_at("BU_CODE", str_replace, "BU", "") %>%
  mutate_at("BU_CODE", str_remove, "^0+")
df_solar_panels$BU_CODE = as.numeric(as.character(df_solar_panels$BU_CODE))
