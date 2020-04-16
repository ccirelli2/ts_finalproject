## DOCUMENTATION __________________________________________________
' Descr:	Main R script for project
          Objective is to predict the count of Auto Thefts over time.
          Time will be measured using the Occurance Date and Occurance Time
	Ref: https://www.analyticsvidhya.com/blog/2018/05/generate-accurate-forecasts-facebook-prophet-python-r/
  Ref: https://www.statmethods.net/advstats/timeseries.html 
       - Seasonal decomposition
'

## Clear Namespace --------------------------------------------------
rm(list=ls())

## Load Libraries ---------------------------------------------------
source('/home/cc2/Desktop/repositories/ts_finalproject/data_inspection.R')
library(dplyr)
library(ggplot2)
library(astsa)
library(forecast)
library(Hmisc)
library(tseries)


## Load Data --------------------------------------------------------
' data source:  http://opendataportal.azurewebsites.us/
  description:  Atlanta Crime Data'
data <- read.csv('/home/cc2/Desktop/repositories/ts_finalproject/data/data.csv')


## Data Inspection --------------------------------------------------
'  Observations:  Data set contains temporal, continuous, ordinar and categoricla data.
                  Missing data:  Apartment Offic Prefix looks like it has missing values. 
                  
                  Target:        UCR.Literal
'
data_dict <- get_data_dict()
col_names = names(data)
col_names
head(data)


# Inspection Auto Thefts
AutoTheft   <- data$UCR.Literal=='AUTO THEFT'
data.at     <- data[AutoTheft, c('Occur.Date', 'Occur.Time', 'Beat', 'Shift.Occurence', 'Location.Type', 
                        'Neighborhood', 'Latitude', 'Longitude')]
summary(data.at)

# Transform Columns 
' Observations:  The minimum of the occurance date is 1916, which does not comport with the
                 study which should have been from 2009 at the earliest
  Null/Missing   Neighborhood = 1941

'
data.at$Occur.Date <- as.Date(data.at$Occur.Date, '%Y-%m-%d')
data.at$Year       <- format(as.Date(data.at$Occur.Date, '%Y-%m-%d'), "%Y")
data.at$Month      <- format(as.Date(data.at$Occur.Date, '%Y-%m-%d'), "%m")
data.at$Day        <- format(as.Date(data.at$Occur.Date, '%Y-%m-%d'), "%d")
data.at$Hour       <- substr(data.at$Occur.Time, 1,2)
data.at$Count      <- rep(1, length(data.at$Year))
data.at$Occur.Time <- NULL
col_names <- names(data.at)
min.yr             <- data.at$Year > 2008
data.at            <- data.at[min.yr, names(data.at)]
min(data.at$Year)
summary(data.at)




# Visualize Data -----------------------------------------------------------------------------------------
' Observations:  Vizualizing the monthly plot reveals what appears to be a cycle that repeats every 
                 6 months.  From February to July auto thefts show a constant increase until a pique in 
                 July.  From July to January shows a stretched out parabola shape (elongated u shape). 
'
date.cnt  <- data.at %>% group_by(Occur.Date) %>% tally()
yr.cnt    <- data.at %>% group_by(Year) %>% tally()
month.cnt <- data.at %>% group_by(Month) %>% tally()
day.cnt   <- data.at %>% group_by(Day) %>% tally()
hour.cnt  <- data.at %>% group_by(Hour) %>% tally()
plt.date  <- ggplot(date.cnt, aes(x=Occur.Date, y=n)) + geom_bar(stat='identity') + ggtitle("Count of Auto Thefts By Date")
plt.yr    <- ggplot(yr.cnt, aes(x=Year, y=n)) + geom_bar(stat='identity') + ggtitle("Count of Auto Thefts By Year")
plt.month <- ggplot(month.cnt, aes(x=Month, y=n)) + geom_bar(stat='identity') + ggtitle("Count of Auto Thefts By Month")
plt.day   <- ggplot(day.cnt, aes(x=Day, y=n)) + geom_bar(stat='identity') + ggtitle("Count of Auto Thefts By Day")
plt.hour  <- ggplot(hour.cnt, aes(x=Hour, y=n)) + geom_bar(stat='identity') + ggtitle("Count of Auto Thefts By Hour")

# Plots that show 6 month cycle
plt.month
plot(x=seq(1,length(month.cnt$n)), y=month.cnt$n, 'l')

# Stationarity --------------------------------------------------------------------------------------------
ts.12 <- ts(date.cnt$n, start=c(2009, 01 ,01), end=c(2019, 12,31), frequency=12)
monthplot(ts.12, main='Stationarity Test - Monthly Plot of Mean Count - Frequency =12')
ts.6 <- ts(date.cnt$n, start=c(2009, 01 ,01), end=c(2019, 12,31), frequency=6)
monthplot(ts.6, main='Stationarity Test - Monthly Plot of Mean Count - Frequency =6')

#****** Take Log of data to see difference in mean. 
x         <- seq(1, length(ts.12))
ts.12.log <- log(ts.12)
ts.6.log  <- log(ts.6)

par(mfrow=c(2,1))
plot(x=x, y=ts.12, col='green', type='l')
plot(x=x, y=ts.12.log, col='red', type='l')

x         <- seq(1, length(ts.6))
plot(x=x, y=ts.6, col='green', type='l')
plot(x=x, y=ts.6.log, col='red', type='l')

# Check if variance is constant
par(mfrow=c(1,1))
h1 = ts.6[1: round(length(ts.6 *.5))]
h2 = ts.6[36: length(ts.6)]
v1 = var(h1)
v2 = var(h2)
?barplot
barplot(height=c(v1, v2), names.arg=c('v1', 'v2'), main='Compare Varianc e- 1st Half vs 2nd Half')


# Take Log and Check Variance Again
h1.log <- log(h1)
h2.log <- log(h2)
v1.log <- var(h1.log)
v2.log <- var(h2.log)
barplot(height=c(v1.log, v2.log), names.arg=c('v1', 'v2'), main='Compare Varianc - Log 1st Half vs 2nd Half')


# Seasonality ---------------------------------------------------------------------------------------------
' Observations:  The cycles are hard to vizualize for each series.   
                 For some months there seems to be a cyclicality for a 6 month period that looks like a 
                 parabola
'
viz_season_v1(date.cnt, 6)


# Take Difference to remove trend -------------------------------------------------------------------------
' Observations:  No material differences in plot.  Still difficult to vizualize. 
'
viz_season_v2(date.cnt, 31)


# Apply Smoothing to Differenced Time Series --------------------------------------------------------------
' Observations:  Mean & Variance are clearly not constant
                 Sligthly easier to vizualize the cyclicality of the data.   
'
viz_diff_smoothed(date.cnt, 1, 12)

# Decomposition of Time Series ----------------------------------------------------------------------------
' Observations:  
  i.) Seaonality The frequency of the time series that provides for the cleanest visualization of the
                 seasonality appears to be 6 months.  All other options between 7-12 months have 
                 more eratic and less consistent seaonality plots. 
  ii.) Trend     The trend seems to be less pronounced at 6 months than at 12.  This is likely due to the 
                 fact that the trend occurs over annual periods and not bi-annually, which is what we 
                 visualized in the earlier plots. 
                 

'
# Decomposition (Observed, Trend, Seasonal, Random)
plot(decomp(date.cnt, 6))

# Seaonality Component
gen_seasonal_lots(decomp, date.cnt, 6, 9)





# Stationarity - ACF & PACF Plots -------------------------------------------------------------------------

# Create Time Series 
ts.6 <- ts(date.cnt$n, start=c(2009, 01 ,01), end=c(2019, 12,31), frequency=6)
ts.12  <- ts(date.cnt$n, start=c(2009, 01 ,01), end=c(2019, 12,31), frequency=12)



# Dickey Fuller Test
' Observations:
    Unclearn why the DF test says that the data is stationary when we know
    that it is not. 
    
  Output:
    Augmented Dickey-Fuller Test
    data:  date.cnt$n
    Dickey-Fuller = -48.161, Lag order = 0, p-value = 0.01
    alternative hypothesis: stationary
'
adf.test(ts.6, k=0)

# ACF & PACF Plot 
' Observations: The plots indicate that this is possibly a 032-130-6 process. 
'
par(mfrow=c(1,1))
ts.6.diff <- diff(ts.6, 3)
acf(ts.6.diff)
pacf(ts.6.diff)


# Utilize Auto Arima to verify best arima order -----------------------------------------------------------------

# Fit Auto Arima Model
' auto.arima.6  = Best model: ARIMA(0,0,0) with non-zero mean 
  auto.arima.12 = Best model: ARIMA(0,0,1) with non-zero mean' 
auto.arima.6  <- auto.arima(ts.6, trace=TRUE, ic='bic')
auto.arima.12 <- auto.arima(ts.12, trace=TRUE, ic='bic')


# Fit Auto Arima - Use Log of 
' auto.arima.6  = Best model: ARIMA(0,1,1)   
  auto.arima.12 = Best model: ARIMA(0,0,0)  
'
ts.6.log     <- log(ts.6)
ts.12.log    <- log(ts.12)
auto.arima.6 <- auto.arima(ts.6.log, trace=TRUE, ic='bic')
auto.arima.12 <- auto.arima(ts.12.log, trace=TRUE, ic='bic')


# Manually Fit SARIMA Model ---------------------------------------------------------------------------------------
' Approach:      Try multiple fits
  Observation:   sarima (0,1,2)(2,2,1)x6 provided the lowest aic score. 
                 the prediction verus the plot results are pretty good. 
'

# Train Test Split  
train.sample <- round(0.7 * length(ts.6.log))
test.sample  <- length(ts.6.log) - train.sample

ts.train <-  ts.6[1: train.sample]
ts.test  <-  ts.6[51: length(ts.6)]

# Train Sarima Model (Order = 032 130 6)
sarima.6.fit  <- arima(ts.train, order=c(0,1,2), seasonal = list(order=c(2,2,1), period=6))
summary(sarima.6.fit)
sarima.6.pred <- predict(sarima.6.fit, length(ts.test))

# Plot Prediction Vs Actual
x = seq(1, test.sample)
plot(x=x, y=sarima.6.pred$pred, col='blue', type='l', xlab='Time', ylab='Count Auto Theft', main='SARIMA (0,1,2)(2,2,1)x6 - Prediction vs Actual')
lines(x=x, y=ts.test, col='red')
legend(1, 5, legend=c('Actual', 'Predicted'), col=c('Blue', 'Red'), lty=1:2, cex=0.8)

# Calculate RMSE
sarima.6.mse <- sum((sarima.6.pred$pred - ts.test)**2) / length(sarima.6.pred$pred)
sarima.6.mse
summary(sarima.6.fit)

sum((sarima.6.pred$pred - ts.test)**2) / length(ts.test)
