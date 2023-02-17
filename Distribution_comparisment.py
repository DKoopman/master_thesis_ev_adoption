#import dependencies
import pandas as pd
import geopandas as gpd
from scipy.stats import ks_2samp
import scipy.stats as stats
import matplotlib.pyplot as plt
import numpy as np

#read in the data of the shapefile
imd_shp = 'WijkBuurtkaart_2020_v3\\buurt_2020_v3.shp'
imd = gpd.read_file(imd_shp)

#process car_density variable
df_cars = pd.read_csv('df_cars.csv')
imd  = imd.merge(df_cars, on='BU_CODE', how='left')
imd['Car_density'] = imd['G_PAU_KM']/imd['AANT_INW']
imd.replace([np.inf, -np.inf], np.nan, inplace=True)

#process Green_votes_perc
df_green_votes = pd.read_csv('greenVotes.csv')
df_green_votes = df_green_votes[['BU_CODE', 'Green_votes_perc']]
imd  = imd.merge(df_green_votes, on='BU_CODE', how='left')

#process Solar_power_kw
df_solar_panels = pd.read_csv('solarpanels.csv')
df_solar_panels = df_solar_panels[['BU_CODE', 'Solar_power_kw']]
imd  = imd.merge(df_solar_panels, on='BU_CODE', how='left')

#reconstruct shop_distance
imd['shop_distance'] = (imd['AF_APOTH'] + imd['AF_SUPERM'] + imd['AF_WARENH'] + imd['AF_RESTAU'])/4
imd['P_OPL_HG'] = imd['A_OPL_HG']/ imd['AANT_INW']
imd['P_OPL_LG'] = imd['A_OPL_LG']/ imd['AANT_INW']

df = pd.read_excel('Gemeenten alfabetisch 2022.xlsx')
df = df[['GemeentecodeGM', 'Provincienaam']]
df_total = pd.merge(imd, df, left_on='GM_CODE', right_on='GemeentecodeGM')
print(df_total.shape)

#select the MRA region
df_selection = df_total[df_total['Provincienaam'].isin(['Flevoland', 'Noord-Holland', 'Utrecht'])]
df_selection = df_selection[~df_selection['GM_NAAM'].isin(['Amsterdam', 'Utrecht'])]

#select the rest of the Netherlands
df_selection2 = df_total[~df_total['Provincienaam'].isin(['Flevoland', 'Noord-Holland', 'Utrecht'])]

#select columns to check
columns_to_check = ['AF_TREINST',
'AF_ZIEK_E',
'AF_ONDBAS',
'P_LAAGINKP',
'P_HOOGINKP',
'P_OPL_HG',
'P_OPL_LG',
'P_15_24_JR',
'P_25_44_JR',
'P_45_64_JR',
'P_65_EO_JR',
'P_ARB_ZS',
'P_HH_M_K',
'P_HUURWON',
'BEV_DICHTH',
'GEM_HH_GR',
'AANT_INW',
'Car_density',
'Dist_shops',
'Solar_power_kw',
'Green_votes_perc']

#create mapping for variable names
columns_to_check_mapping = ['Dist_train_station',
'Dist_hospital',
'Dist_school',
'Perc_low_income',
'Perc_high_income',
'Perc_high_educated',
'Perc_low_educated',
'Perc_15_24_yr',
'Perc_25_44_yr',
'Perc_45_64_yr',
'Perc_65_EO_yr',
'Perc_self_employed',
'Perc_household_child',
'Perc_rental_house',
'Population_density',
'Avg_household_size',
'Num_residents',
'Car_density',
'Dist_shops',
'Solar_power',
'Perc_green_votes']

#make the figures
for index,column in enumerate(columns_to_check):
    fig, ax = plt.subplots(figsize = (8,4))
    df_selection = df_selection[df_selection[column] >= 0]
    ax = df_selection[column].plot.kde(label = f'MRA {columns_to_check_mapping[index]}', legend = True)
    df_selection2 = df_selection2[df_selection2[column] >= 0]
    ax = df_selection2[column].plot.kde(label = f'NL {columns_to_check_mapping[index]}', legend = True)
    
    #no assumptions on distribution
    ax.plot([], [], ' ', label=f"P-value KS-test: {round(ks_2samp(df_selection[column], df_selection2[column])[1],5)}")
    
    #test on normality
    if stats.shapiro(df_selection[column])[0] < 0.05 :
        #not normally distributed
        ax.plot([], [], ' ', label=f"P-value Wilcoxon-test: {round(stats.ranksums(x=df_selection[column], y=df_selection2[column])[1],5)}")
    else:
        #normal distribution
        ax.plot([], [], ' ', label=f"P-value T-test: {round(stats.ttest_ind(a=df_selection[column], b=df_selection2[column], equal_var=True)[1],5)}")
    plt.legend()
    fig.savefig(f'figures/{columns_to_check_mapping[index]}.png')