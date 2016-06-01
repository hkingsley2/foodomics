#########################################################    
######Script to Calculate Saturation Categories##########
#########################################################  

#Read .CSV file
dataframe<-read.csv(file="Z:/Notebooks_E/e15/Harris Kingsley/Foodomics Beta Testing/For Running/r practice.csv", header=TRUE, sep=",")

#Total Fat = `X204`
#Monounsaturated Fat=`X645`
#Polyunsaturated Fat=`X646`
#Saturated Fat=`X606`
#Trans Fat=`X605`


#creates new columns and says that if total fat is 0 on the food label then all the fats should be 0 
dataframe$calc_totalfat <- ifelse(!is.na(dataframe$Total_Fat_per_serving_g),  dataframe$Total_Fat_per_serving_g, dataframe$X204 )
dataframe$calc_saturatedfat <- ifelse(dataframe$Total_Fat_per_serving_g ==0, 0,dataframe$Saturated_Fat_per_serving_g)  
dataframe$calc_transfat <- ifelse(dataframe$Total_Fat_per_serving_g == 0,0,dataframe$Trans_Fat_per_serving_g)    
dataframe$calc_monofat <- ifelse(dataframe$Total_Fat_per_serving_g == 0,0,dataframe$Monounsaturated_Fat_per_serving_g)    
dataframe$calc_polyfat <- ifelse(dataframe$Total_Fat_per_serving_g == 0,0,dataframe$Polyunsaturated_Fat_per_serving_g)    

#creates a new column that counts the number of NA's 
dataframe$na_count <- apply(dataframe[,19:23],1,function(x) length(which(is.na(x))))

#if there is 1 NA this fills it in with the total fat - sum of other individual fats  
dataframe$calc_saturatedfat <- ifelse(dataframe$na_count ==1 & is.na(dataframe$calc_saturatedfat),dataframe$Total_Fat_per_serving_g- apply(dataframe[20:23],1, function(x) sum(x,na.rm=TRUE)),dataframe$calc_saturatedfat) 
dataframe$calc_transfat <- ifelse(dataframe$na_count ==1 & is.na(dataframe$calc_transfat),dataframe$Total_Fat_per_serving_g - apply(dataframe[20:23],1, function(x) sum(x,na.rm=TRUE)),dataframe$calc_transfat)
dataframe$calc_monofat <- ifelse(dataframe$na_count ==1 & is.na(dataframe$calc_monofat),dataframe$Total_Fat_per_serving_g - apply(dataframe[20:23],1, function(x) sum(x,na.rm=TRUE)),dataframe$calc_monofat)
dataframe$calc_polyfat <- ifelse(dataframe$na_count ==1 & is.na(dataframe$calc_polyfat),dataframe$Total_Fat_per_serving_g - apply(dataframe[20:23],1, function(x) sum(x,na.rm=TRUE)),dataframe$calc_polyfat)


#######by percent calculation I use the fat from the food database and devide it
#######by the sum of the 4 individual fats in the food databases and
######multiply it by the total fat on the food label 


#if there are 2 NA's this fills in with a percent calcuation 
dataframe$calc_saturatedfat <- ifelse(dataframe$na_count == 2 & is.na(dataframe$calc_saturatedfat),dataframe$X606/apply(dataframe[15:18],1,function(x) sum(x,na.rm=TRUE))*dataframe$calc_totalfat,dataframe$calc_saturatedfat)
dataframe$calc_transfat <- ifelse(dataframe$na_count == 2 & is.na(dataframe$calc_transfat),dataframe$X605/apply(dataframe[15:18],1,function(x) sum(x,na.rm=TRUE))*dataframe$calc_totalfat,dataframe$calc_transfat)
dataframe$calc_monofat <- ifelse(dataframe$na_count == 2 & is.na(dataframe$calc_monofat),dataframe$X645/apply(dataframe[15:18],1,function(x) sum(x,na.rm=TRUE))*dataframe$calc_totalfat,dataframe$calc_monofat)
dataframe$calc_polyfat <- ifelse(dataframe$na_count == 2 & is.na(dataframe$calc_polyfat),dataframe$X646/apply(dataframe[15:18],1,function(x) sum(x,na.rm=TRUE))*dataframe$calc_totalfat,dataframe$calc_polyfat)

#if there are 3 NA's this fills in the data with amother percent cacluation
dataframe$calc_saturatedfat <- ifelse(dataframe$na_count == 3 & is.na(dataframe$calc_saturatedfat), dataframe$X606/apply(dataframe[15:18],1,function(x) sum(x,na.rm=TRUE))*dataframe$calc_totalfat,dataframe$calc_saturatedfat)
dataframe$calc_transfat <- ifelse(dataframe$na_count == 3 & is.na(dataframe$calc_transfat), dataframe$X605/apply(dataframe[15:18],1,function(x) sum(x,na.rm=TRUE))*dataframe$calc_totalfat,dataframe$calc_transfat)
dataframe$calc_monofat <- ifelse(dataframe$na_count == 3 & is.na(dataframe$calc_monofat), dataframe$X645/apply(dataframe[15:18],1,function(x) sum(x,na.rm=TRUE))*dataframe$calc_totalfat,dataframe$calc_monofat)
dataframe$calc_polyfat <- ifelse(dataframe$na_count == 3 & is.na(dataframe$calc_polyfat), dataframe$X646/apply(dataframe[15:18],1,function(x) sum(x,na.rm=TRUE))*dataframe$calc_totalfat,dataframe$calc_polyfat)

#if there are 4 NA's (only total fat is avalaible) this fills in with percent calculation 
dataframe$calc_saturatedfat <- ifelse(dataframe$na_count == 4 & is.na(dataframe$calc_saturatedfat), dataframe$X606/apply(dataframe[15:18],1,function(x) sum(x,na.rm=TRUE))*dataframe$calc_totalfat,dataframe$calc_saturatedfat)
dataframe$calc_transfat <- ifelse(dataframe$na_count == 4 & is.na(dataframe$calc_transfat), dataframe$X605/apply(dataframe[15:18],1,function(x) sum(x,na.rm=TRUE))*dataframe$calc_totalfat,dataframe$calc_transfat)
dataframe$calc_monofat <- ifelse(dataframe$na_count == 4 & is.na(dataframe$calc_monofat), dataframe$X645/apply(dataframe[15:18],1,function(x) sum(x,na.rm=TRUE))*dataframe$calc_totalfat,dataframe$calc_monofat)
dataframe$calc_polyfat <- ifelse(dataframe$na_count == 4 & is.na(dataframe$calc_polyfat), dataframe$X646/apply(dataframe[15:18],1,function(x) sum(x,na.rm=TRUE))*dataframe$calc_totalfat,dataframe$calc_polyfat)

#if all 5 of the food label data is NA then just fill in with database data if possible
dataframe$calc_totalfat <- ifelse(dataframe$na_count == 5 & is.na(dataframe$calc_totalfat),dataframe$X204,dataframe$calc_totalfat)
dataframe$calc_saturatedfat <- ifelse(dataframe$na_count == 5 & is.na(dataframe$calc_saturatedfat), dataframe$X606,dataframe$calc_saturatedfat)
dataframe$calc_transfat <- ifelse(dataframe$na_count == 5 & is.na(dataframe$calc_transfat), dataframe$X605,dataframe$calc_transfat)
dataframe$calc_monofat <- ifelse(dataframe$na_count == 5 & is.na(dataframe$calc_monofat), dataframe$X645,dataframe$calc_monofat)
dataframe$calc_polyfat <- ifelse(dataframe$na_count == 5 & is.na(dataframe$calc_polyfat), dataframe$X646,dataframe$calc_polyfat)
