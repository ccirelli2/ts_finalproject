# DOCUMENTATION ---------------------------------------
'	Desc:	Module for the data inspection 
'

# Import Libraries ------------------------------------
library(hash)

get_data_dict <- function(){
  data_dict <- hash()
  data_dict[["Report.Date"]]              <- c('INT', '########',	'Unique record ID')
  data_dict[['Occur.Date']]               <- c('DATE', 'YY-MM-DD')
  data_dict[['Occur.Time']]               <- c('DATETIME', 'HHMM')
  data_dict[['Possible.Date']]            <- c('DATE', 'YY-MM-DD')
  data_dict[['Possible.Time']]            <- c('DATEIME', 'HHMM')
  data_dict[['Beat']]                     <- c('INT', '###', 'Location type data')
  data_dict[['Appartment.Office.Prefix']] <- c('TEXT')			
	data_dict[['Appartment.Number']]        <- c('INT', '###', 'Number of the apt.') 
	data_dict[['Location']]                 <- c('VARCHAR', '###"""', 'Address of location')
  data_dict[['Shift.Occurence']]          <- c('VARCHAR', '"""', 'Categorical time of day') 
	data_dict[['Location.Type']]            <- c('INT', '##', 'Unclear', 'Need the key') 
	data_dict[['UCR.Literal']]					    <- c('TEXT', '"""', 'Major category of crime') 
  data_dict[['UCR..']]						        <- c('INT', '###', 'Minor category of crime') 
  data_dict[['IBR.Code']]					        <- c('VARCHAR', '###', 'Another minor code of crime') 
  data_dict[['Neighborhood']]				      <- c('TEXT', '"""', 'Name of neighborbood/area') 
	data_dict[['NPU']]							        <- c('TEXT', '"', 'Neighborhood planning units') 
  data_dict[['Latitude']]					        <- c('FLOAT', '##.#####', 'Latitude location of crime') 
  data_dict[['Longitude']]					      <- c('FLOAT', '##.####', 'Longitude location of crime')
  return(data_dict)
}
  

viz_season_v1 <- function(data, frequency){
  ts.date.cnt <- ts(data$n, start=c(2009, 01 ,01), end=c(2019, 12,31), frequency=frequency)
  plt <- ggseasonplot(ts.date.cnt, main=paste('Seasonal Plot - AutoTheft - Frequency =', frequency))
  return(plt)
}

viz_season_v2 <- function(data, frequency){
  diff_        <- diff(data$n, 2)
  ts.date.cnt <- ts(diff_, start=c(2009, 01 ,01), end=c(2019, 12,31), frequency=frequency)
  plt         <- ggseasonplot(ts.date.cnt, main=paste('Seasonal Plot - AutoTheft - Frequency =', frequency))
  return(plt)
}


viz_diff_smoothed <- function(data, difference, frequency){
  diff_     <- diff(data$n, difference)
  smoothed <- smooth(diff_, endrule='Tukey') 
  ts       <- ts(smoothed, start=c(2009, 01 ,01), end=c(2019, 12,31), frequency=frequency)
  plt      <- plot(ts, main='Time Series Plot - Diff=2 | Smoothed=Median')
  return(plt)
}
  

decomp <- function(data, frequency){
  'Desc:   Decompose a time series
   data:   data.frame that includes count of observations by feature variable. Ex date.cnt
   return: decomposition of time series. '
  ts  <- ts(data$n, start=c(2009, 01 ,01), end=c(2019, 12,31), frequency=frequency)
  dcp <- decompose(ts)
  return(dcp)
}


gen_seasonal_lots <- function(decomp, data, min, max){
  ' Desc:  Generate 4 plots of the seasonality of a time series
    decomp decomposition function defined in data_inspection script
    min:   Minimum number of frequency
    max:   Max number of frequency
    Note:  Max - Min must not > 4
  '
  # Create 4 Plots on Single Pallet
  par(mfrow=c(4,1))
  # Iterate sequency of frequencies
  for (i in seq(min, max)){
    # Get results
    result <- decomp(data, i)
    # Plot
    plot(result$seasonal, main=paste('TS - Plot of Seasonality At Frequency =', i))
  } 
  return()
}





