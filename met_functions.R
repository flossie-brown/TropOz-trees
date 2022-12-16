#------------------------------------------------------------------------
# function returns merged ozone and met dataframe
# subtracts 3 because some files are weird
#------------------------------------------------------------------------
merge_ozone_met <- function(met, ozone, time_format){

	# format the same
	ozone$datetime <- as.POSIXct(strptime(ozone$date, format=time_format, tz="Australia/Brisbane"))
	met$datetime <- as.POSIXct(strptime(met$datetime, format=time_format, tz="Australia/Brisbane"))

	# some ozone and met outputs are different by 3 mins, subtract 3 from ozone file so they can be merged
	# print in case number is not 3
	time_dif <- ozone$datetime[1] - met$datetime[1]
	print(time_dif)

	met$datetime <- met$datetime + time_dif

	full <- merge(ozone, met, by = 'datetime')
    
    return(full)
}

#------------------------------------------------------------------------
# function extracts hours months and days from datetime
# append back to dataframe
#------------------------------------------------------------------------
add_month_day_hour_DOY <- function(dataframe, start_date){

	#break up datetime into sections
	strtime <- lapply(dataframe$datetime, FUN=function(x) toString(x))

	fulltime <- lapply(strtime, FUN=function(x) strsplit(x, ' ')[[1]][2])
    	hours <- lapply(fulltime, FUN=function(x) strsplit(x, ':')[[1]][1])

    	fulldate <- lapply(strtime, FUN=function(x) strsplit(x, ' ')[[1]][1])
    	months <- lapply(fulldate, FUN=function(x) strsplit(x, '-')[[1]][2])
    	days <- lapply(fulldate, FUN=function(x) strsplit(x, '-')[[1]][3])

	# convert to DOY
	Date2 <-  lapply(fulldate, FUN=function(x) as.Date(x,"%Y-%m-%d"))
    	dif <- lapply(Date2, FUN=function(x) difftime(x , as.Date(start_date, "%Y-%m-%d"), units="days"))
	
	# replace nans with 0 because it gets confused at 00:00
	hour <- rapply(hours , f=function(x) ifelse(is.na(x),0,x), how="replace" )

	dataframe$Month <- as.numeric(months)
	dataframe$Day <- as.numeric(days)
	dataframe$Hour <- as.numeric(hour)
	dataframe$Day_of_year <- as.numeric(unlist(dif))

	numeric <- lapply(dataframe, as.numeric)
	full_data <- data.frame(numeric)

	return(full_data)
    }

#------------------------------------------------------------------------
# function calculates day of year
# append back to dataframe
#------------------------------------------------------------------------
save_file <- function(filename, chamber_input){

	full_input <- cbind(csv_input, hourly_average[chamber_input])
	write.csv(na.omit(full_input), filename, row.names=FALSE)

	return(print(filename))

    }