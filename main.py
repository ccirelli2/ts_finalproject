# Documentation ________________________________________
''' Desc: Script for fitting and generating predictions using
    the facebook prophet model.
    Ref:https://facebook.github.io/prophet/docs/quick_start.html#python-api
'''


# Import Modules ----------------------------------------
import pandas as pd
import matplotlib.pyplot as plt
from fbprophet import Prophet

## Define Directories -----------------------------------
dir_data = r'/home/cc2/Desktop/repositories/ts_finalproject/data'


## Import Data ------------------------------------------
data = pd.read_csv(dir_data + '/' + 'data.csv')


## Inspect Data -----------------------------------------
#print(data.head())


## Get Count By Date ------------------------------------
df_grouped = data.groupby('Report Date')['Report Date'].count()
df_final = pd.DataFrame({})
df_final['ds'] = df_grouped.index.astype('datetime64[ns]')
df_final['y'] = df_grouped.values


## Split Train Test
sample = round(0.7 * len(df_final['y']))
train = df_final.iloc[:sample, :]
test = df_final.iloc[sample :, :]

## Instantiate & Fit Prophet Model ----------------------
m = Prophet()
m.fit(train)

## Make a Prediction ------------------------------------
future = m.make_future_dataframe(periods = len(test.loc[:, 'y']))
pred = m.predict(future)


# Create DataFrame with Actual and Predicted Values -----
df_pred = pd.DataFrame({})
df_pred['Predicted'] = pred.loc[sample :, 'yhat']
df_pred['Actual'] = test.loc[:, 'y']


# Plot Prediction vs Actual
df_pred.plot()
plt.title('Actual vs Predicted')
plt.xlabel('Time Series')
plt.ylabel('Daily Count of Auto Thefts')
plt.show()

mse = sum((df_pred.loc[:, 'Predicted'] - df_pred.loc[:, 'Actual'])**2) / len(df_pred.iloc[:, 1])
print('MSE => {}'.format(mse))
print(len(df_pred.iloc[:, 0]))
