#------------------------------------------------------------------------
# select abaxial or adaxial data to calculate gmax
# compute RSC from conductance an gmax
#------------------------------------------------------------------------
RSC <- function(dataframe, position){

	#select ab or ad
	position_df <- dataframe[dataframe$Position == position,]

	#select 95th percentile
	gmax <- quantile(position_df$Conductance, probs = 0.95)[[1]]

	position_df$RSC <- position_df$Conductance / gmax

	return(position_df)
	}

#------------------------------------------------------------------------
# select abaxial or adaxial data and calculate gmax
#------------------------------------------------------------------------
gmax <- function(dataframe){

	#select 95th percentile
	gmax <- quantile(dataframe$Conductance, probs = 0.95)[[1]]

	return(gmax)
	}

#------------------------------------------------------------------------
# Divide variable into 10 equal sections
#------------------------------------------------------------------------

 variable_limits <- function(variable){

	#make a sequence of equally spaced values between the recorded limits
    max <- max(variable, na.rm = TRUE)
    min <- min(variable, na.rm = TRUE)
    limits <- seq(min, max, length.out = 12)

    return(limits)
	}

#------------------------------------------------------------------------
# Return 90th percentile for each section
#------------------------------------------------------------------------

divide_10 <- function(variable, column){

    limits <- variable_limits(variable)

	#select data between those limits and find the 90th percentile for each group
    centile <- list()
    for(i in 1:11) {
        segment <- Porometer[(variable >= limits[i]) & (variable < limits[i+1]), column]
        quant <- quantile(segment, probs = 0.9, na.rm = TRUE)
        centile <- c(centile, quant)
    }

    T90 <- list()
    for (i in 1:11) {
        T90 <- c(T90, centile[[i]])
    }
    return(T90)
}

#------------------------------------------------------------------------
# Select the data above the 90th percentile in each group
#------------------------------------------------------------------------

top_90 <- function(variable){

    limits <- variable_limits(variable)

	#make a dataframe of data above the 90th percentile
    top <- data.frame()
    for(i in 1:11) {
        segment <- Porometer[(variable >= limits[i]) & (variable < limits[i+1]),]
        quant <- quantile(segment$RSC, probs = 0.9, na.rm = TRUE)
        data <- segment[segment$RSC > quant[1],]
        top <- rbind(top, data)
    }
    return(top)
}

#------------------------------------------------------------------------
# Find the center value of each group for plotting
#------------------------------------------------------------------------

variable_center <- function(variable){

    limits <- variable_limits(variable)
    center <- list()
    for(i in 1:11) {
        total <- limits[i] +limits[i+1]
        middle <- total/2
        center <- c(center, middle)
        }
    return(center)
    }

#------------------------------------------------------------------------
# Fit a polynomial of order 2
#------------------------------------------------------------------------

fit_poly <- function(dataframe){

	polynomial <- lm(RSC ~ poly(AirTC_Avg, 2, raw = TRUE), data = dataframe)
	coefs <- coef(polynomial)

	return(coefs)
	}

#------------------------------------------------------------------------
# Calculate temperatures from Jarvis parameters
#------------------------------------------------------------------------

temp_function <- function(temp, coefs){
    temps = coefs[[1]] + ((temp)*coefs[[2]]) + (coefs[[3]] * ((temp)**2))
    max <- ifelse(temps < 0, 0, temps)
    return(max)
}

RH_function <- function(RH, coefs){
    RHs = coefs[[1]] + (coefs[[2]] * (RH))
    min <- ifelse(RHs > 1, 1, RHs)
    return(min)
}

PAR_function <- function(PAR, coefs){
    PARs = 1 - exp(-coefs * PAR)
    min <- ifelse(PARs > 1, 1, PARs)
    return(min)
}

#------------------------------------------------------------------------
# function extracts hours months and days from datetime
# append back to dataframe
#------------------------------------------------------------------------
add_day <- function(dataframe){

	#break up datetime into sections
	strtime <- lapply(dataframe$datetime, FUN=function(x) toString(x))

	fulltime <- lapply(strtime, FUN=function(x) strsplit(x, ' ')[[1]][2])

    	fulldate <- lapply(strtime, FUN=function(x) strsplit(x, ' ')[[1]][1])
    	days <- lapply(fulldate, FUN=function(x) strsplit(x, '-')[[1]][3])

	dataframe$Time <- fulltime
	dataframe$Day <- as.numeric(days)

	return(dataframe)
    }

#------------------------------------------------------------------------
# function extracts days from datetime
# append back to dataframe
#------------------------------------------------------------------------
add_day2 <- function(dataframe){

	day <- list()
	Time <- list()
	df_len <- nrow(dataframe)

	for (i in 1:df_len){
    		times <- dataframe$datetime[[i]]
    		strtime <- toString(times)
		fulltime <- strsplit(strtime, ' ')[[1]][2]
    		fulldate <- strsplit(strtime, ' ')[[1]][1]
    		days <- strsplit(fulldate, '-')[[1]][3]

    		day <- c(day, days)
		Time <- c(Time, fulltime)
		}

	dataframe$Time <- Time
	dataframe$Day <- as.numeric(day)


	return(dataframe)
    }