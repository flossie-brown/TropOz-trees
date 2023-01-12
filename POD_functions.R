#--------------------------------------------------
#  make list of files and load all POD values
#--------------------------------------------------

fileload <- function(folder_name, file_suffix){

        filepaths = list()
        for (i in 1:9){
                filename <- c(gsub(" ", "", paste(f, '/', folder_name, '/output', i, file_suffix, '.csv')))
                filepaths <- c(filepaths, filename)
            }

        file_list <- lapply(filepaths, function(x){read.csv(x)})
    }

#--------------------------------------------------
#  extract POD values
#--------------------------------------------------

POD_extract <- function(df_list){

	POD_list <- data.frame(Cham_ID = 1)

        for (i in 1:9){
            	PODY_values <- lapply(df_list[[i]], FUN = max)

            	POD_df <- data.frame(Cham_ID = i)
          	POD_df$PODy = PODY_values$PODY..mmol.m.2.PLA.
		POD_df$POD0 = PODY_values$POD0..mmol.m.2.PLA.
		POD_df$AOT40 = PODY_values$AOT40..ppm.

            	POD_list <- Reduce(function(x, y) merge(x, y, all=TRUE), list(POD_list, POD_df))
            	}   
        
        return(POD_list)
    }

#--------------------------------------------------
#  make a dataframe with yield relationships
#--------------------------------------------------

yield_features <- function(dataframe, POD_variable, name){
    
    biomass_relationship <- lm(Biomass ~ POD_variable, data = dataframe)
    Intercept <- summary(biomass_relationship)$coefficients[1]
    Slope <-  summary(biomass_relationship)$coefficients[2]

    yield_relationship <- lm(Biomass/Intercept ~ POD_variable, data = dataframe)
    r.squared <- summary(yield_relationship)$r.squared
    Yield_relationship <- summary(yield_relationship)$coefficients[2]
    
    Yield_dataframe <- data.frame(r.squared = r.squared,
                                  Intercept = Intercept, Slope = Slope, 
                                  Yield_relationship = Yield_relationship,
                                   POD_type = name)
    
    return(Yield_dataframe)
    
}