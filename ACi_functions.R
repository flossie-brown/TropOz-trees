#------------------------------------------------------------------------
# function selects certain variables from excel file for ACi calculations
# converts some intergers to factorsas appropriate
#------------------------------------------------------------------------
unify_excel_file <- function(dataframe){
    
    key_vars <- dataframe[c('Obs', 'HHMMSS', 'Exp', 'Pot', 'Leaf', 'Chamber', 'Photo', 'Tleaf', 'Ci', 'PARi')]
    
    key_vars$Leaf<-as.factor(key_vars$Leaf)
    key_vars$Pot<-as.factor(key_vars$Pot)
    
    return(key_vars)
}

#------------------------------------------------------------------------
# function applies fitacis from plantecophys package using dataframe variables
# group by pot to fit an aci curve for each pot
#------------------------------------------------------------------------

fitACIs <- function(dataframe, group_var){
    
    df <- as.data.frame(dataframe)
    fits <- fitacis(df, varnames = list(ALEAF = "Photo", Tleaf = "Tleaf", Ci = "Ci", PPFD = "PARi"), group = group_var)
    
    return(fits)
}

#------------------------------------------------------------------------
# function summarises ACi curva data to give mean Vcmax Jmax
# Curve could be saved seperately for more detailed analysis
#------------------------------------------------------------------------

summary_table <- function(acifits){
    
    #establish data frame for Curve fitting outputs
    Curve<-coef(acifits)

    #Summary table of photosynthetic parameters
    Curve_Sum<- data.frame(Vcmax.mean=mean(Curve$Vcmax,na.rm=T),
                      Vcmax.sd=sd(Curve$Vcmax,na.rm=T),
                      Jmax.mean=mean(Curve$Jmax,na.rm=T),
                      Jmax.sd=sd(Curve$Jmax,na.rm=T))
    
    return(Curve_Sum)
}

#------------------------------------------------------------------------
# function gives intercepts and slope of fitted ACi data against measured ACi data
# Will have intercept 0 and slope 1 is perfect fit
#------------------------------------------------------------------------

fit_summary <- function(acifits){
    
    all_data <- acifits[[1]]$df
    no_of_curves <- length(acifits[])

    if (length(ACI_fits) > 1){
    
        for (i in 2:no_of_curves){
             all_data <-rbind(all_data, acifits[[i]]$df)
        }}
    
    corr <- lm(Ameas ~ Amodel, data = all_data)
    corr_corf <- coef(corr)
    
    return(corr_corf)
    }
