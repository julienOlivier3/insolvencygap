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
import pyreadr

df_mup_panel = pyreadr.read_r(r'Q:\\Meine Bibliotheken\\Research\\SME_Corona\\02_Data\\01_PanelData\\df_panel2010_2020.R')

df_mup_panel = pd.read_csv(r'Q:\\Meine Bibliotheken\\Research\\SME_Corona\\02_Data\\01_PanelData\\df_panel2010_2019.txt', sep = '\t', encoding='utf-8')

df_mup_panel.loc[df_mup_panel.crefo==2012252973, :]


