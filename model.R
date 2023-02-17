#import dependencies
library(performanceEstimation)
library(corrplot)
library(readxl)
library(AER)
library(pscl)
library(mpath)
library("olsrr")
library(boot)

#construct modelling dataframe
independent_variables <- read_excel("independent_variables.xlsx")
neighborhoodsMRA$BU_CODE = as.character(neighborhoodsMRA$BU_CODE)
total_df <- left_join(neighborhoodsMRA, independent_variables, by = 'BU_CODE')

#make plot of which neighborhoods to delete, is done later in the code for total_df
total_df_delete_plot <- total_df
total_df_delete_plot$delete <- 0
total_df_delete_plot[(rowSums(is.na(total_df_delete_plot)) > ncol(total_df_delete_plot)*.25) & 
                       (total_df_delete_plot$Valid == 1),'delete'] <- 1

tm_shape(total_df_delete_plot) +
  tm_fill(col = 'delete', breaks = c(0,1,1)) +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.5,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 0)


#select only valid neighborhoods and delete columns
total_df <- total_df[total_df$Valid == 1,]
total_df <- total_df[,!names(total_df) %in% c("Valid")]

#cast columns
total_df$car_density <- as.numeric(total_df$car_density)
total_df$charging_points <- as.numeric(total_df$charging_points)

#delete rows with more than 25% na values
delete_rows <- total_df[rowSums(is.na(total_df)) > ncol(total_df)*.25,]
total_df <- total_df[!rowSums(is.na(total_df)) > ncol(total_df)*.25,]

#get new percentage of NA values
colMeans(is.na(total_df))

#for the green votes impute by the mean of the neighborhoods that touch a neighborhood with na value
index <- st_touches(total_df, total_df)
total_df <- total_df %>% 
  mutate(Green_votes_perc = ifelse(is.na(Green_votes_perc),
                                   apply(index, 1, function(i){mean(.$Green_votes_perc[i])}),
                                   Green_votes_perc))

#drop the geometry column
total_df <- total_df %>% st_drop_geometry()

#impute the other values using knn with 10 neighbors
total_df$BU_CODE <- as.numeric(total_df$BU_CODE)
total_df <- knnImp(total_df, k = 10)
colMeans(is.na(total_df))

#create a copy for later use and delete BU_CODE column
final_df <- total_df
total_df <- total_df[,!names(total_df) %in% c("BU_CODE")]

#############################################################################################
#ev users

#create own dataframe
home_chargers_model_df <- total_df[,!names(total_df) %in% c("occupancy_rate")]

#test on over dispersion under simple poisson regression
fmp <- glm(ev_users ~ Solar_power_kw , data = home_chargers_model_df, family=poisson)
dispersiontest(fmp)
#small p value so over dispersion -> therefore negative binomial model

#print all the columns in the dataframe
colnames(home_chargers_model_df)

#the following model was selected after trial and error using step down method
model_ev_users <- zeroinfl(ev_users ~ Solar_power+Dist_school+Perc_rental_house+Dist_hospital+
                             Perc_green_votes+Perc_low_educated+Dist_shops+Perc_self_employed+
                             Perc_65_EO_yr | Solar_power+Perc_rental_house+Avg_household_size+
                             Dist_hospital+Perc_green_votes+Dist_shops+Perc_self_employed+
                             Perc_45_64_yr+Perc_65_EO_yr, data = home_chargers_model_df, dist = "negbin")

#create mcfadden pseudo r squared value
pR2(model_ev_users)
summary(model_ev_users)

#check whether it is statistical better than zero model
InterceptModel <- update(model_ev_users, . ~ 1)
logLik(InterceptModel)
pchisq(2 * (logLik(model_ev_users) - logLik(InterceptModel)), df = 3, lower.tail=FALSE)

#create start values for bootstrap method
dput(coef(model_ev_users, "count"))
dput(coef(model_ev_users, "zero"))

#perform bootstrap experiments
f <- function(data, i) {
  require(pscl)
  m <- zeroinfl(ev_users ~ Solar_power_kw+AF_ONDBAS+P_HUURWON+AF_ZIEK_E+
                  Green_votes_perc+P_OPL_LG+shop_distance+P_ARB_ZS+P_65_EO_JR | Solar_power_kw+     
                  P_HUURWON +GEM_HH_GR+AF_ZIEK_E+Green_votes_perc+shop_distance+P_ARB_ZS+P_45_64_JR+P_65_EO_JR, data = data[i, ], dist="negbin",
                start = list(count = c(1.368, 0.001, -0.244, 0.007, -0.048, 0.035, -2.553, -0.103, 0.011, -0.26), zero = c(-23.262, 0.002, 0.118, 5.006, -0.266, -0.250, 0.910, 0.076, 0.147, 0.090)))
  print(as.vector(t(do.call(rbind, coef(summary(m)))[, 1:2])))
}
set.seed(10)
res <- boot(home_chargers_model_df, f, R = 3000, parallel = "snow", ncpus = 1)

#create bca confidence intervals for coefficients
parms <- t(sapply(c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33 ,35, 37, 39, 41), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "bca"))
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              bcaLL = bca[4], bcaUL = bca[5]))
}))

#construct matrix
var_names <- names(coef(model_ev_users))
var_names <- append(var_names, "count_theta", 10)
row.names(parms) <- var_names
parms

#now do the same for the transformed coefficients; h = exp
expparms <- t(sapply(c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33 ,35, 37, 39, 41), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("bca"), h = exp)
  with(out, c(Est = t0, bcaLL = bca[4], bcaUL = bca[5]))
}))

## add row names
row.names(expparms) <- var_names
expparms

#create boxplot of the confidence intervals
par(mar=c(11,8,1,1))
boxplot <- boxplot(t(head(expparms,11)), las =2, outline = TRUE)

#########################################################################################
#occupancy rate model

#construct modelling dataframe
occupancy_model_df <- total_df
names(occupancy_model_df)[names(occupancy_model_df) == 'AANT_INW'] <- 'Num_residents'
occupancy_model_df <- total_df[,!names(total_df) %in% c("home_chargers","NumberChargingPoints")]
colnames(occupancy_model_df)
occupancy_model_df$occupancy_rate<- occupancy_model_df$avg_hours/730.48
occupancy_model_df <- occupancy_model_df[,!names(occupancy_model_df) %in% c("avg_hours")]

#linearity check
pairs(~occupancy_rate + Num_residents + Solar_power  + Car_density + Perc_high_income + Perc_low_income + Dist_school
      + Has_school + Perc_rental_house, data = occupancy_model_df)

pairs(~occupancy_rate +Has_public_point + Avg_household_size + Perc_household_child + Dist_hospital + 
        Has_hospital + Perc_high_educated + Perc_low_educated + Population_density, data = occupancy_model_df)

pairs(~occupancy_rate  + Dist_shops + Perc_self_employed + Dist_train_station + Has_city_centre + Perc_15_24_yr +
        Perc_25_44_yr + Perc_45_64_yr + Perc_65_EO_yr, data = occupancy_model_df)

#correlations check
cor_mat <- cor(occupancy_model_df)
cor_mat[cor_mat > 0.8]
cor_mat[cor_mat < -0.8]
corrplot(cor_mat, method="number",tl.cex=0.5, number.cex=0.5)
occupancy_model_df <- occupancy_model_df[,!names(occupancy_model_df) %in% c("P_HH_M_K", "P_LAAG_INK_H")]

#with the step down method the following model was constructed
model_occupancy <- lm(occupancy_rate ~ Num_residents+Solar_power+
                        Perc_high_income+Has_school+
                        Avg_household_size+Perc_green_votes + Perc_high_educated+Population_density+      
                        Perc_self_employed+Perc_45_64_yr+Perc_65_EO_yr, data = occupancy_model_df)
summary(model_occupancy)

#check no autoregression
acf(model_occupancy$residuals)
pacf(model_occupancy$residuals)

#test first lag autocorrelation
dwtest(model_occupancy)
# -> there is no significant autocorrelation in the model

#check normality of error terms
plot(model_occupancy)
ols_test_normality(model_occupancy)

#check residuals
mean(model_occupancy$residuals)
plot(model_occupancy$residuals)

#bootstrap experiments
dput(coef(model_occupancy))
res_occupancy <- Boot(model_occupancy, R=3000)

#create confidence intervalls
conf_occupancy <- confint(res_occupancy)
conf_occupancy <- cbind(coef(model_occupancy), conf_occupancy)
colnames(conf_occupancy) <- c("Est", "bcaLL", "bcaUL")
conf_occupancy

#create boxplot of the confidence intervals
par(mar=c(8,5,1,1))
boxplot(t(conf_occupancy), las =2)