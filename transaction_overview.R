#analysis for appendix on fact_chargersessions

correct_records <- function(chargingData){
  #clear rows with NA in critical columns
  chargingData <- chargingData[!is.na(chargingData$RFID_skey),]
  chargingData <- chargingData[!is.na(chargingData$UseType),]
  chargingData <- chargingData[!is.na(chargingData$kWh),]
  chargingData <- chargingData[!is.na(chargingData$IsValid),]
  chargingData <- chargingData[!is.na(chargingData$StartConnectionDateTime),]
  chargingData <- chargingData[!is.na(chargingData$EndConnectionDateTime),]
  chargingData <- chargingData[!is.na(chargingData$ChargePoint_skey),]
  
  #clear rows with values that are not correct
  chargingData <- chargingData[chargingData$UseType == 'regulier',]#only regulier usetype
  chargingData <- chargingData[chargingData$RFID_skey >= 0,]
  chargingData <- chargingData[chargingData$kWh > 0,]
  chargingData <- chargingData[chargingData$IsValid == 1,]
  chargingData <- chargingData[chargingData$ChargePoint_skey %in% relevant_chargers,]#relevant_chargers as in Relevant_charging_points script
  
  DIM_LOCATION <- DIM_LOCATION(username, password)
  
  #add location data to chargindata
  charingPointsData <- left_join(chargingData, DIM_LOCATION)
  #delete sessions where no postalcode is known
  charingPointsData <- charingPointsData[!is.na(charingPointsData$PostalCode),]
  charingPointsData <- charingPointsData[!is.na(charingPointsData$Location_skey),]
  return(charingPointsData)
}

correct_records_occupancy <- function(chargingData){
  #clear rows with NA in critical columns
  chargingData <- chargingData[!is.na(chargingData$RFID_skey),]
  chargingData <- chargingData[!is.na(chargingData$kWh),]
  chargingData <- chargingData[!is.na(chargingData$IsValid),]
  chargingData <- chargingData[!is.na(chargingData$StartConnectionDateTime),]
  chargingData <- chargingData[!is.na(chargingData$EndConnectionDateTime),]
  chargingData <- chargingData[!is.na(chargingData$ChargePoint_skey),]
  
  #clear rows with values that are not correct
  chargingData <- chargingData[chargingData$RFID_skey >= 0,]
  chargingData <- chargingData[chargingData$kWh > 0,]
  chargingData <- chargingData[chargingData$IsValid == 1,]
  chargingData <- chargingData[chargingData$ChargePoint_skey %in% relevant_chargers,]#relevant_chargers as in Relevant_charging_points script
  
  DIM_LOCATION <- DIM_LOCATION(username, password)
  
  #add location data to chargindata
  charingPointsData <- left_join(chargingData, DIM_LOCATION)
  #delete sessions where no postalcode is known
  charingPointsData <- charingPointsData[!is.na(charingPointsData$PostalCode),]
  charingPointsData <- charingPointsData[!is.na(charingPointsData$Location_skey),]
  return(charingPointsData)
}


#januari
data_jan <- FACT_CHARGESESSION(username,password,country = "'NLD'" ,
                               startDateView = "'2022-01-01'", endDateView = "'2022-01-31'")
nrow(data_jan)
#191749
correct_data_jan <- correct_records(data_jan)
nrow(correct_data_jan)
#176610
correct_data_jan_occupancy <- correct_records_occupancy(data_jan)
nrow(correct_data_jan_occupancy)
#186686

#februari
data_feb <- FACT_CHARGESESSION(username,password,country = "'NLD'" ,
                               startDateView = "'2022-02-01'", endDateView = "'2022-02-28'")
nrow(data_feb)
#191015
correct_data_feb <- correct_records(data_feb)
nrow(correct_data_feb)
#175178
correct_data_feb_occupancy <- correct_records_occupancy(data_feb)
nrow(correct_data_feb_occupancy)
#185082


#march
data_mar <- FACT_CHARGESESSION(username,password,country = "'NLD'" ,
                               startDateView = "'2022-03-01'", endDateView = "'2022-03-31'")
nrow(data_mar)
#247207
correct_data_mar <- correct_records(data_mar)
nrow(correct_data_mar)
#231595
correct_data_mar_occupancy <- correct_records_occupancy(data_mar)
nrow(correct_data_mar_occupancy)
#232040



#april
data_apr <- FACT_CHARGESESSION(username,password,country = "'NLD'" ,
                               startDateView = "'2022-04-01'", endDateView = "'2022-04-30'")
nrow(data_apr)
#241266
correct_data_apr <- correct_records(data_apr)
nrow(correct_data_apr)
#226307
correct_data_apr_occupancy <- correct_records_occupancy(data_apr)
nrow(correct_data_apr_occupancy)
#226805

#may
data_may <- FACT_CHARGESESSION(username,password,country = "'NLD'" ,
                               startDateView = "'2022-05-01'", endDateView = "'2022-05-31'")
nrow(data_may)
#220645
correct_data_may <- correct_records(data_may)
nrow(correct_data_may)
#207432
correct_data_may_occupancy <- correct_records_occupancy(data_may)
nrow(correct_data_may_occupancy)
#207854

#june
data_jun <- FACT_CHARGESESSION(username,password,country = "'NLD'" ,
                               startDateView = "'2022-06-01'", endDateView = "'2022-06-30'")
nrow(data_jun)
#238243
correct_data_jun <- correct_records(data_jun)
nrow(correct_data_jun)
#222687
correct_data_jun_occupancy <- correct_records_occupancy(data_jun)
nrow(correct_data_jun_occupancy)
#223138

#july
data_jul <- FACT_CHARGESESSION(username,password,country = "'NLD'" ,
                               startDateView = "'2022-07-01'", endDateView = "'2022-07-31'")
nrow(data_jul)
#236643
correct_data_jul <- correct_records(data_jul)
nrow(correct_data_jul)
#222496
correct_data_jul_occupancy <- correct_records_occupancy(data_jul)
nrow(correct_data_jul_occupancy)
#222911

#august
data_aug <- FACT_CHARGESESSION(username,password,country = "'NLD'" ,
                               startDateView = "'2022-08-01'", endDateView = "'2022-08-31'")
nrow(data_aug)
#229407
correct_data_aug <- correct_records(data_aug)
nrow(correct_data_aug)
#215816
correct_data_aug_occupancy <- correct_records_occupancy(data_aug)
nrow(correct_data_aug_occupancy)
#216189

#september
data_sep <- FACT_CHARGESESSION(username,password,country = "'NLD'" ,
                               startDateView = "'2022-09-01'", endDateView = "'2022-09-30'")
nrow(data_sep)
#269567
correct_data_sep <- correct_records(data_sep)
nrow(correct_data_sep)
#252450
correct_data_sep_occupancy <- correct_records_occupancy(data_sep)
nrow(correct_data_sep_occupancy)
#252824

#october
data_oct <- FACT_CHARGESESSION(username,password,country = "'NLD'" ,
                               startDateView = "'2022-10-01'", endDateView = "'2022-10-31'")
nrow(data_oct)
#291390
correct_data_oct <- correct_records(data_oct)
nrow(correct_data_oct)
#273126
correct_data_oct_occupancy <- correct_records_occupancy(data_oct)
nrow(correct_data_oct_occupancy)
#273518

#november
data_nov <- FACT_CHARGESESSION(username,password,country = "'NLD'" ,
                               startDateView = "'2022-11-01'", endDateView = "'2022-11-30'")
nrow(data_nov)
#316076
correct_data_nov <- correct_records(data_nov)
nrow(correct_data_nov)
#296060
correct_data_nov_occupancy <- correct_records_occupancy(data_nov)
nrow(correct_data_nov_occupancy)
#296491

#december
data_dec <- FACT_CHARGESESSION(username,password,country = "'NLD'" ,
                               startDateView = "'2022-12-01'", endDateView = "'2022-12-31'")
nrow(data_dec)
#342804
correct_data_dec <- correct_records(data_dec)
nrow(correct_data_dec)
#318718
correct_data_dec_occupancy <- correct_records_occupancy(data_dec)
nrow(correct_data_dec_occupancy)
#319165