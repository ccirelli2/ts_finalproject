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



# Seasonality ---------------------------------------------------------------------------------------------
' Observations:  The cycles are hard to vizualize for each series.   
                 For some months there seems to be a cyclicality for a 6 month period that looks like a 
                 parabola
'
viz_season_v1(date.cnt, 6)


# Take Difference to remove trend -------------------------------------------------------------------------
' Observations:  No material differences in plot.  Still difficult to vizualize. 
'
viz_season_v2(date.cnt.diff, 31)


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

# Dickey Fuller Test
'	Augmented Dickey-Fuller Test
  data:  date.cnt$n
  Dickey-Fuller = -48.161, Lag order = 0, p-value = 0.01
  alternative hypothesis: stationary
'
adf.test(date.cnt$n, k=0)


# ACF & PACF Plot 
' ACF:  
'
ts <- ts(date.cnt$n, start=c(2009, 01 ,01), end=c(2019, 12,31), frequency=12)
ts.diff <- diff(ts, 1)

acf(ts.diff)
pacf(ts.diff)

plot(decompose(ts))






