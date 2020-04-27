import matplotlib.pyplot as plt
import pandas as pd
import statsmodels.api as sm
from patsy import dmatrices
pd.set_option('display.max_columns', None)


# Documentation ---------------------------------------------------
''' Descr:  Investigate the relationship between the claims counts
            across all independent variables.
'''

# Directories -----------------------------------------------------
dir_data = r'/home/cc2/Desktop/repositories/ts_finalproject/data'

# Import Data -----------------------------------------------------
data = pd.read_csv(dir_data + '/' + 'data.csv', header=0, infer_datetime_format=True)


# Process Data ----------------------------------------------------
data['occur_date'] = pd.to_datetime(data['Occur Date'], format="%Y-%m-%d")
data['occur_year'] = [str(x)[0:4] for x in data.loc[:, 'occur_date']]
data['occur_month'] = [str(x)[5:7] for x in data.loc[:, 'occur_date']]
data['occur_day'] = [str(x)[8:10] for x in data.loc[:, 'occur_date']]

df = data.loc[:, ['occur_year', 'occur_month', 'occur_day', 'Beat', 'Shift Occurence', 'UCR Literal', 'Neighborhood']]



# Instantiate Poisson Regression Model
