#------------------------------------------------------------------------
# function selects certain variables from excel file for gs calculations
# converts some intergers to factors as appropriate
#------------------------------------------------------------------------
unify_excel_file <- function(dataframe){

	#remove those data with negative conductance
	Licor_Track<-subset(dataframe)
	#remove unrealistic Ci values (<0 or >1000) these are due to transient conditions such as
	#pressure changes when the greenhouse door opens
	#Rapid change in conditions and unstable data
	#high CO2 conditions found when the greenhouse has been worked in and needs to be ventillated

	Licor_Track<-subset(Licor_Track,Ci>=0 & Ci<=1000)
	Licor_Track<-subset(Licor_Track,!(Photo<=0 & Cond>=0.1))
	Licor_Track<-subset(Licor_Track,StableF>=0.5)

        Licor_Track$Time<-as.POSIXct(strptime(Licor_Track$HHMMSS,format="%H:%M:%S",tz="Australia/Brisbane"))
    
        key_vars <- Licor_Track[c('Obs', 'HHMMSS', 'Exp', 'Leaf', 'Chamber','Pot',
	'Photo', 'Cond', 'VpdL', 'CO2S', 'RH_S', 'PARi', 'Hrs')]
    
        key_vars$Leaf<-as.factor(key_vars$Leaf)
        key_vars$Pot<-as.factor(key_vars$Pot)
	key_vars$Hrs <- as.numeric(key_vars$Hrs)
    
    return(key_vars)
}

#------------------------------------------------------------------------
# function compares predicted conductance with measured conductance and gives r.squared
# Other functons possible if return lmgpred
#------------------------------------------------------------------------

fit_accuracy <- function(gs_fit, preds, dataframe){
     
    preds_v_measured <- data.frame(subset(dataframe,PARi>=1)$Cond, preds)
    names(preds_v_measured)[[1]] <- 'measured'
    
    lmgpred <- lm(data = preds_v_measured, measured ~ preds)
    stats <- glance(lmgpred)
    
    return(stats)
    }

#------------------------------------------------------------------------
# function finds g0 as the average nighttime conductance in umol mol
# only select positive values of conductance
#------------------------------------------------------------------------

g0_calc<- function(dataframe){

	col_index <- which(colnames(night)=="Cond")
	average_cond <- mean(dataframe[dataframe$Cond >= 0 ,col_index])

	return(average_cond)
	}

#------------------------------------------------------------------------
# function saves g1 values and rsquare values
#------------------------------------------------------------------------

save_stats <- function(g1_BB, r_BB, g1_USO, r_USO, g0, filename){

	BallBerry <- c(g1_BB[[2]])
	r2_BB <- c(r_BB$r.squared)
	Medlyn <- c(g1_USO[[2]])
	r2_Medlyn <- c(r_USO$r.squared)
	g0_BB <- c(g0)

	df <- data.frame(BallBerry, r2_BB, g0_BB, Medlyn, r2_Medlyn)
	cat('\n\n Save DataFrame')
	print(colnames(df))

	write.csv( df, filename, row.names=FALSE)

	return(cat(paste('saved as: ', filename)))
	}