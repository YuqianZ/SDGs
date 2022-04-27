# -*- coding: utf-8 -*-
"""
Created on Tue Apr 26 23:06:12 2022

@author: Yuqian Zhang
"""


import pandas as pd

N_SDG_14_comb_00_20 = pd.read_csv('G:/My Drive/MSU/Study/Research/My PhD/SDGs/SDG14&15/SDG/data/SDSN2/N_SDG_14_comb_00_20.csv', encoding='latin-1')

df1 = pd.DataFrame(N_SDG_14_comb_00_20)
df1

df1['Year'] = df1['Year'].astype(str)

df1['Key'] = df1['Code']+df1['SDG']+df1['Year']
df1

dict1 = dict(zip(df1['Key'],df1['Value']))
dict1

N_empty_full_country_sdg_year = pd.read_csv('G:/My Drive/MSU/Study/Research/My PhD/SDGs/SDG14&15/SDG/data/SDSN2/empty_full_country_sdg_year.csv')
df2 = pd.DataFrame(N_empty_full_country_sdg_year)
df2

df2['Year'] = df2['Year'].astype(str)
df2['Key'] = df2['Code']+df2['SDG']+df2['Year']
df2

df2['Value_true'] = df2['Key'].map(dict1)
df2

df2.to_csv('G:/My Drive/MSU/Study/Research/My PhD/SDGs/SDG14&15/SDG/data/SDSN2/update_full_country_sdg_year.csv')
