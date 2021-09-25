# -*- coding: utf-8 -*-
# ---
# jupyter:
#   jupytext:
#     formats: ipynb,py:light
#     text_representation:
#       extension: .py
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.6.0
#   kernelspec:
#     display_name: Python 3
#     language: python
#     name: python3
# ---

import pandas as pd
import numpy as np
import re
from datetime import datetime
import pickle

# # After Calculations in R 

date_parser = lambda c: pd.to_datetime(c, format='%Y-%m-%d', errors='coerce')

# + [markdown] heading_collapsed=true
# ## V1 

# + hidden=true
n_split = 1
n_subsplit = 10

# + hidden=true
# Define parsing for pd.read_csv()
lst_drop = []
lst_panel = []
file_ending = [str(i) + '_' + str(j) for i in range(1,n_split+1) for j in range(1,n_subsplit+1)]
parse_dates=['rechdat', 'grddat', 'exitdat']
dtype = {'crefo': 'Int64',
        'jahr': 'Int64',
        'treat': 'Int64',
        'update': 'Int64',
        'updays': 'Int64',
        'gryear': 'Int64',
        'bonitaet': 'Int64',
        'ch_bonitaet': 'Int64',
        'exit': 'Int64',
        'anzma': 'Int64',
        'ch_anzma': 'Int64',
        'umsatz': 'float64',
        'ch_umsatz': 'float64',
        'auftrag': 'Int64',
        'p_auftrag': 'Int64',
        'entwick': 'Int64',
        'p_entwick': 'Int64',
        'wz': 'Int64',
        'kreis': 'Int64',
        'refo': 'Int64',
        'wz08': 'Int64',
        'stamm': 'Int64',
        'kurteil': 'Int64',
        'zweise': 'Int64',
        'hkredit': 'Int64',
        'bilwelle': 'Int64',
        'faulig': 'Int64',
        'giftig': 'Int64',
        'tot': 'Int64'
        }

# + hidden=true
# Loop over local files and append these
for k in file_ending:
    df_temp = pd.read_csv(r'Q:\Meine Bibliotheken\Research\SME_Corona\02_Data\01_PanelData\02_Chunks\df_panel' + k + r'.txt', 
                          sep='\t',
                          encoding='latin',
                          index_col='crefo',
                          na_values=['TRUE', 'FALSE'],
                          keep_default_na=True,
                          parse_dates=parse_dates,
                          #infer_datetime_format=True,
                          date_parser=date_parser,
                          dtype=dtype
                         )
    lst_drop.append(df_temp.ch_bonitaet.isna().shape[0])
    df_temp = df_temp.loc[df_temp.ch_bonitaet.notna(),:]
    lst_panel.append(df_temp)
    
    if '_10' in k:
        print(k)

# + hidden=true
# Create one df
df_panel = pd.concat(lst_panel)

# + hidden=true
df_panel.head(3)

# + hidden=true
df_panel.shape

# + hidden=true
# Number of dropped observations
sum(lst_drop)

# + hidden=true
# Number of observations after dropping all panel-observations prior to 2018
df_panel.shape[0] + sum(lst_drop)

# + hidden=true
df_panel.to_csv(r"Q:\Meine Bibliotheken\Research\SME_Corona\02_Data\01_PanelData\df_panel_2018_2020_clean.txt", 
                sep = '\t',
                na_rep='NA')
# -

# ## V2

n_split = 50
n_subsplit = 10

# Define parsing for pd.read_csv()
lst_drop = []
lst_panel = []
file_ending = [str(i) + '_' + str(j) for i in range(1,n_split+1) for j in range(1,n_subsplit+1)]
parse_dates=['rechdat', 'grddat']
dtype = {'crefo': 'Int64',
        'jahr': 'Int64',
        'update': 'Int64',
        'updays': 'Int64',
        'gryear': 'Int64',
        'p_bonitaet': 'Int64', 
        'bonitaet': 'Int64',
        'ch_bonitaet': 'Int64',
        'anzma': 'Int64',
        'ch_anzma': 'Int64',
        'umsatz': 'float64',
        'ch_umsatz': 'float64',
        'auftrag': 'Int64',
        'entwick': 'Int64',
        'wz': 'Int64',
        'kreis': 'Int64',
        'refo': 'Int64',
        'wz08': 'Int64',
        'stamm': 'Int64',
        'kurteil': 'Int64',
        'zweise': 'Int64',
        'hkredit': 'Int64',
        'bilwelle': 'Int64',
        'faulig': 'Int64',
        'giftig': 'Int64',
        'tot': 'Int64'
        }

# Loop over local files and append these
for k in file_ending:
    df_temp = pd.read_csv(r'H:\Large_Datasets\SME_Corona\02_Chunks\df_panel' + k + r'.txt', 
                          sep='\t',
                          encoding='latin',
                          index_col='crefo',
                          na_values=['TRUE', 'FALSE'],
                          keep_default_na=True,
                          parse_dates=parse_dates,
                          #infer_datetime_format=True,
                          date_parser=date_parser,
                          dtype=dtype
                         )
    
    lst_drop.append(df_temp.ch_bonitaet.isna().shape[0])
    df_temp = df_temp.loc[df_temp.ch_bonitaet.notna(),:]
    lst_panel.append(df_temp)
    
    if '_10' in k:
        print(k)

# Create one df
df_panel = pd.concat(lst_panel)

df_panel.head(3)

df_panel.shape

# Distribution of observations over years
df_panel.jahr.value_counts(dropna=False)

# Number of dropped observations
sum(lst_drop)

df_panel.to_csv(r"Q:\Meine Bibliotheken\Research\SME_Corona\02_Data\01_PanelData\df_panel_2016_2020_clean.txt", 
                sep = '\t',
                na_rep='NA')



# # Conduct Calculations in Python 

w_y_map = {
    58 : 2020,
    56 : 2019,
    54 : 2018,
    52 : 2017,
    50 : 2016,
    48 : 2015,
    46 : 2014,
    44 : 2013,
    42 : 2012,
    40 : 2011,
    38 : 2010
           }


# ## Dates Files 

# + [markdown] heading_collapsed=true
# ### Cleaning

# + hidden=true
def clean_dat(column):
    
    # If date is read as float there are explicit missings convert these to 0 and coerce column to int then proceed normal
    if column.dtypes == 'float64':
        column = column.fillna(0).astype(int)
    
    # Define date column as string
    column = column.astype(str)
    
    # Replace month and day by 01-01 if not given
    column = column.apply(lambda x: re.sub(r'0000$', '0101', x))
    
    # Replace day by 01 if not given
    column = column.apply(lambda x: re.sub(r'00$', '01', x))
    
    # Clean format of dates
    #dates = [datetime.strptime(i, '%Y%m%d').date().strftime("%Y-%m-%d") if len(i)==8 else np.nan for i in column.values]
    column = pd.to_datetime(column, errors='coerce').dt.date
    
    # Create Series
    #res = pd.Series(index=column.index, data = dates, name = column.name)
    
    return(column)


# + hidden=true
file_ending = [str(i) for i in range(58,46,-2)]

# + hidden=true
file_ending

# + hidden=true
# Loop over local files and clean these
for k in file_ending:
    
    # Read file
    df_dat = pd.read_stata(
        r'K:\\MUP\\Paneldaten\\Daten\\Original\\daten_w' + k + r'.dta', 
        preserve_dtypes=False, 
        convert_dates=True, 
        index_col='crefo'
                         )
    
    df_dat.index = df_dat.index.astype(np.int64)
    df_dat['jahr'] = df_dat.welle.map(w_y_map)
    df_dat['grddat'] = clean_dat(df_dat.gruenddat)
    df_dat.erstdat = clean_dat(df_dat.erstdat)
    df_dat.rechdat = clean_dat(df_dat.rechdat)
    
    # Select relevant columns
    df_dat.drop(columns= ['welle', 'gruenddat', 'archivdat', 'hreintragdat', 'ziehdat'], inplace=True)
    
    with open(r'H:\Large_Datasets\SME_Corona\df_dat' + k + '.pkl', 'wb') as f:
        pickle.dump(obj=df_dat, file=f)

    print(k)

# + [markdown] heading_collapsed=true
# ### Appending

# + hidden=true
# Create empty df
df_dat = pd.DataFrame(columns=['jahr', 'rechdat'])

# Loop over local files and append these
for k in file_ending:
    
    # Read file
    df_temp = pd.read_csv(
        r'H:\Large_Datasets\SME_Corona\df_dat' + k + r'.csv', 
        sep = '\t',
        #nrows=1000,
        index_col='Unnamed: 0',
        usecols = ['Unnamed: 0', 'jahr', 'rechdat']
                         )
    
    df_dat = df_dat.append(df_temp)

    print(k)

# + hidden=true
df_dat.shape

# + hidden=true
df_dat.head(3)

# + hidden=true
df_dat.index.name = 'crefo'

# + hidden=true
df_dat.to_csv(r'H:\Large_Datasets\SME_Corona\df_dat_all.csv', sep = '\t', encoding='utf-8')


# + [markdown] heading_collapsed=true
# ## Clean Exit Files

# + hidden=true
def clean_exit(x):
    
    len_x = len(x)
    
    if len_x == 5:
        if x[0] == '2':
            res = x[0] + '0' + x[1:5] + '01'
        elif x[0] == '9':
            res = '1' + x[0:5] + '01'
        else: 
            res = np.datetime64('NaT')
    
    elif len_x == 4:
        res = '19' + x[0:4] + '01'
    
    elif len_x == 3:
        res = np.datetime64('NaT')
        
    elif len_x == 6:
        res = x[0:6] + '01'
        
    else:
        res = np.datetime64('NaT')
        
    return(res)


# + hidden=true
exit_map = {'existiert sicher': 0,
            'existiert nicht': 1,
            'Konkurs': 2,
            '(zurzeit) nicht wirtschaftsaktiv': 3,
            'nicht ziehen': 4,
            'Übernahme': 5}

# + hidden=true
file_ending = [str(i) for i in range(58,52,-2)]

# + hidden=true
file_ending

# + hidden=true
# Loop over local files and append these
for k in file_ending:
    
    # Read file
    df_temp = pd.read_stata(
        r"K:\MUP\Paneldaten\Exitmerkmal\exitmerkmal_W" + k + ".dta", 
        preserve_dtypes=False, 
        convert_dates=True, 
        index_col='crefo'
                         )
    
    df_temp = df_temp.loc[df_temp.index.notnull(),:].copy()
    df_temp.index = df_temp.index.astype(np.int64)
    df_temp.loc[:,'jahr'] = w_y_map[int(k)]
    df_temp.exit_datum = df_temp.exit_datum.fillna(0).astype(int).astype(str)
    df_temp.exit_datum = df_temp.exit_datum.apply(lambda x: clean_exit(x))
    df_temp['exitdat'] = pd.to_datetime(df_temp.exit_datum, errors='coerce').dt.date
    df_temp.exit = df_temp.exit.map(exit_map)

    # Select relevant columns
    df_temp = df_temp[['jahr', 'exitdat', 'exit']]
    
#     with open(r'Q:\Meine Bibliotheken\Research\SME_Corona\02_Data\01_PanelData\04_Exit\df_exit' + k + '.pkl', 'wb') as f:
#         pickle.dump(obj=df_temp, file=f)

    df_temp.to_csv(r'H:\Large_Datasets\SME_Corona\df_temp' + k + '.csv', sep = '\t', encoding = 'utf-8')

    print(k)

# + [markdown] heading_collapsed=true
# ## Within Chunk Calculations 

# + [markdown] hidden=true
# Migrated to R!

# + hidden=true
date_parser = lambda c: pd.to_datetime(c, format='%Y-%m-%d', errors='coerce')

# + hidden=true
n_split = 50

# + hidden=true
# Define parsing for pd.read_csv()
lst_drop = []
lst_panel = []
#file_ending = [str(i) + '_' + str(j) for i in range(1,n_split+1) for j in range(1,n_subsplit+1)]
parse_dates=['rechdat', 'grddat']
dtype = {'crefo': 'Int64',
        'jahr': 'Int64',
        #'treat': 'Int64',
        #'update': 'Int64',
        #'updays': 'Int64',
        'gryear': 'Int64',
        'bonitaet': 'Int64',
        #'ch_bonitaet': 'Int64',
        #'exit': 'Int64',
        'anzma': 'Int64',
        #'ch_anzma': 'Int64',
        'umsatz': 'float64',
        #'ch_umsatz': 'float64',
        'auftrag': 'Int64',
        #'p_auftrag': 'Int64',
        'entwick': 'Int64',
        #'p_entwick': 'Int64',
        'wz': 'Int64',
        'kreis': 'Int64',
        'refo': 'Int64',
        'wz08': 'Int64',
        'stamm': 'Int64',
        'kurteil': 'Int64',
        'zweise': 'Int64',
        'hkredit': 'Int64',
        'bilwelle': 'Int64',
        'faulig': 'Int64',
        'giftig': 'Int64',
        'tot': 'Int64'
        }

# + hidden=true
df_panel = pd.read_csv(r"Q:\Meine Bibliotheken\Research\SME_Corona\02_Data\01_PanelData\02_Chunks\01_Raw\df_panel1.txt", 
                          sep='\t',
                          encoding='latin',
                          index_col='crefo',
                          #na_values=['TRUE', 'FALSE'],
                          #keep_default_na=True,
                          parse_dates=parse_dates,
                          #infer_datetime_format=True,
                          date_parser=date_parser,
                          #dtype=dtype
                         )

# + hidden=true
df_panel.reset_index(inplace=True)

# + hidden=true
df_panel.set_index(['crefo', 'jahr'], inplace=True)

# + hidden=true
df_panel.shape

# + hidden=true
df_temp = None

# + hidden=true
df_temp

# + hidden=true
df_temp = pd.read_csv(r"H:\Large_Datasets\SME_Corona\df_dat58.csv", sep='\t', encoding='utf-8')

# + hidden=true
df_temp.dtypes

# + hidden=true
df_temp.loc[df_temp['Unnamed: 0']==2011037623,:]

# + hidden=true
df_temp.index.name = 'crefo'

# + hidden=true
df_temp.reset_index(inplace=True)

# + hidden=true
df_temp.set_index(['crefo', 'jahr'], inplace=True)

# + hidden=true
df_temp.drop(columns=['grddat'], inplace=True)

# + hidden=true
df_panel = df_panel.merge(df_temp, how='left', left_index=True, right_index=True)

# + hidden=true
df_panel.columns

# + hidden=true
df_panel['rechdat'] = df_panel.rechdat_x.combine_first(df_panel.rechdat_y)
df_panel.drop(columns = ['rechdat_x', 'rechdat_y'], inplace=True)

# + hidden=true
df_panel
