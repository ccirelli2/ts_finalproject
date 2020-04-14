## DOCUMENTATION _________________________________________________
''' Descr:  Main script for the final project


    Ref: https://towardsdatascience.com/forecasting-in-python-with-facebook-prophet-29810eb57e66

'''


## Import Libraries ----------------------------------------------
import pandas as pd
pd.set_option('display.max_columns', None)
import numpy as np

## Directories --------------------------------------------------
dir_data    = r'/home/cc2/Desktop/repositories/Time_Series/final_project/data'

## Import Data ---------------------------------------------------
afile   = 'COBRA-2009-2019.csv'
data    = pd.read_csv(dir_data + '/' + afile)


## Data Exploration ---------------------------------------------
df_grouped = data.groupby('UCR Literal')['UCR Literal'].count()
print(df_grouped)




