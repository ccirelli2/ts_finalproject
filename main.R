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
source('/home/cc2/Desktop/repositories/Time_Series/final_project/data/data_inspection.R')
library(dplyr)
library(ggplot2)
library(astsa)
library(forecast)

## Load Data --------------------------------------------------------
' data source:  http://opendataportal.azurewebsites.us/
  description:  Atlanta Crime Data'
data <- read.csv('/home/cc2/Desktop/repositories/Time_Series/final_project/data/data.csv')


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


# Plot Data
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


# Seasonality ---------------------------------------------------------------------------------------------
ts.date.cnt <- ts(date.cnt$n, start=c(2009, 01 ,01), end=c(2019, 12,31), frequency=31)
ggseasonplot(ts.date.cnt, main='Seasonal Plot - AutoTheft - Frequency =31')

# Take Difference to remove trend -------------------------------------------------------------------------
date.cnt.diff <- diff(date.cnt$n, 1)
ts.date.cnt.diff <- ts(date.cnt.diff, start=c(2009, 01 ,01), end=c(2019, 12,31), frequency=12)
ggseasonplot(ts.date.cnt.diff, main='Seasonal Plot - AutoTheft - Frequency =31')
plot(y=date.cnt.diff, x=seq(1, length(date.cnt.diff)), 'l', main='Plot of Daily Time Series - Differencing of 1')

# Apply Smoothing to Differenced Time Series
' Mean & Variance are clearly not constant'
date.cnt.diff <- diff(date.cnt$n, 1)
ts.date.cnt.diff.smooth <- ts(date.cnt.diff.smoothed, start=c(2009, 01 ,01), end=c(2019, 12,31), frequency=31)
ggseasonplot(ts.date.cnt.diff.smooth, main='Seasonal Plot | AutoTheft | Frequency =12')
plot(x=seq(1,length(month.cnt$n)), y=month.cnt$n, 'l')

# Average Monthly Count By All Years
mu_cnt.month <- month.cnt$n/31
plot(x=seq(1,12), y=(mu_cnt.month))

# Average Daily Count By All Years
mu_cnt.day <- day.cnt$n/31
plot(x=seq(1,31), y=(mu_cnt.day))

# Observations 
' Trend
  Seasonality
  Cyclicality
  Holidays

'


# Stationarity - ACF & PACF Plots -------------------------------------------------------------------------

acf(date.cnt$n)
pacf(date.cnt$n)
acf2(date.cnt$n)

acf(date.cnt.diff)
acf2(date.cnt.diff)


# Create Time Series
#ts.date.cnt <- ts(date.cnt$n, start=c(2009, 01 ,01), end=c(2019, 12,31))








