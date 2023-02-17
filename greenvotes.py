import pandas as pd
import json

#read in data from geo.json file
with open('tk2021.geo.json') as data_file:    
    data = json.load(data_file)  
df = pd.json_normalize(data, 'features')

#make selection and drop na values
df = df.groupby('properties.Adres').sum(numeric_only=True)
df = df[['properties.Geldige stemmen', 'properties.SP (Socialistische Partij)', 'properties.GROENLINKS', 'properties.Partij voor de Dieren', 'properties.Partij van de Arbeid (P.v.d.A.)']]
df = df.dropna()
df = df.apply(pd.to_numeric)
df.reset_index(inplace = True)

#load mathcing from postcode to neighborhood
match_postcode = pd.read_csv('pc6-gwb2020.csv', sep = ';')
match_postcode = match_postcode[['PC6', 'Buurt2020']]
match_postcode['Buurt2020'] = match_postcode['Buurt2020'].astype(str)
match_postcode['Buurt2020'] = match_postcode['Buurt2020'].str.zfill(8)
match_postcode['Buurt2020'] = "BU" + match_postcode['Buurt2020']

#group by neighborhood and determine percentage green votes
df = df.merge(match_postcode, left_on='properties.Adres', right_on='PC6')
df = df.groupby('Buurt2020').sum(numeric_only=True)
df['Green_votes'] = df['properties.Partij voor de Dieren'] + df['properties.GROENLINKS'] + df['properties.SP (Socialistische Partij)'] + df['properties.Partij van de Arbeid (P.v.d.A.)']
df['Green_votes_perc'] = (df['Green_votes']/df['properties.Geldige stemmen'])*100
df.reset_index(inplace=True)
df = df[['Buurt2020', 'Green_votes_perc']]
df.rename({'Buurt2020': 'BU_CODE'}, axis=1, inplace=True)
df.to_csv('greenVotes.csv')
