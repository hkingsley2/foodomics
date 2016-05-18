#######################################
#START OF MOOSHING PATIENT INTAKE DATA#
#######################################

#Get the patient's menus
setwd(patientfolder)
daily_menus<-read.csv(file='Menus.txt', header=TRUE, sep="\t", na.strings=c("","NA"))

#Convert menus intakes to lowercase ID values
library(dplyr)
daily_menus$PKT_Recipe_Number<-tolower(daily_menus$PKT_Recipe_Number)

#Number every row in the menus dataframe
setwd(patientfolder)
library(lubridate)
library(data.table)
dt <- data.table(daily_menus) #after this the date is mm/dd/yy 12:00 AM
test5<-dt[, number := 1:.N, by=c("MRNUMBER","PKT_Recipe_Number")] #this numbers every row in the dt by date
data<-as.data.frame(test5) #this converts the dt to a dataframe

#Make Menus in Wide Format
menus_ID<-data[,c("MRNUMBER","PKT_Recipe_Number","Ingredient_ID", "number")]
menus_wide_ID <- reshape(menus_ID, direction="wide", idvar = c("MRNUMBER","PKT_Recipe_Number"), timevar = "number")

#Working with the actual values
menus_Amt<-data[,c("MRNUMBER","PKT_Recipe_Number","PKT_Recipe_Ingredient_Amount", "number")]
menus_wide_amt <- reshape(menus_Amt, direction="wide", idvar = c("MRNUMBER","PKT_Recipe_Number"), timevar = "number")

#convert daily intakes to lowercase ID values
melted_daily_intakes$PKT_Recipe_Number<-tolower(melted_daily_intakes$PKT_Recipe_Number)

#Now we want to merge the wide menus with the daily intakes BY THE PKT RECIPE NUMBER
Foods_by_DI<-merge(melted_daily_intakes,menus_wide_ID, by=c("MRNUMBER","PKT_Recipe_Number"))
#write.csv(Foods_by_DI , file="Foods_by_DI.csv")

#Now make the foods in long format so that we can attribute a profile to them from the foodomics database
Manyfoods_DI <- melt(Foods_by_DI ,  id.vars = c("MRNUMBER","PKT_Recipe_Number","Date","Source","DAYT","DATT"), variable.name = 'PRODUCTNDID')
#Manyfoods_DI$PRODUCTNDID<-Manyfoods_DI$value
#write.csv(Manyfoods_DI , file="Manyfoods_DI.csv")

#the above function generates a lot of rows with blank data, so lets get rid of them
clean_Manyfoods_DI<-Manyfoods_DI[!is.na(Manyfoods_DI$value),]

#Now we want to merge in the values to use in scaling
Foods_by_DI_2<-merge(melted_daily_intakes,menus_wide_amt, by=c("MRNUMBER","PKT_Recipe_Number"))
#write.csv(Foods_by_DI_2, file="Foods_by_DI_2.csv")

#Now make the foods in long format so that we can attribute a profile to them from the foodomics database

Manyfoods_DI_2 <- melt(Foods_by_DI_2,  id.vars = c("MRNUMBER","PKT_Recipe_Number","Date","Source","DAYT","DATT"), variable.name = 'Intake')
#write.csv(Manyfoods_DI_2 , file="Manyfoods_DI_2.csv")

#the above function generates a lot of rows with blank data, so lets get rid of them

clean_Manyfoods_DI_2<-Manyfoods_DI_2[!is.na(Manyfoods_DI_2$value),]

####NOW COMBINE THE DATAFRAME WITH THE INGREDIENT NUMBERS AND AMOUNTS
clean_Manyfoods_DI$PRODUCTNDID<-clean_Manyfoods_DI$value
clean_Manyfoods_DI<-clean_Manyfoods_DI[,-c(8)]
clean_Manyfoods_DI$Intake<-clean_Manyfoods_DI_2$value
#write.csv(clean_Manyfoods_DI, file="clean_Manyfoods_DI_comb.csv")

#####################################
#END OF MOOSHING PATIENT INTAKE DATA#
#####################################

#intentionally blank spaces

#####################################
##START OF WORKING W FOODOMICS DATA##
#####################################

#Bring in the foodomics database
setwd("~/GitHub/foodomics/Foodomics Database Creation/Output Data")
foodomics<-readRDS(file="foodomics_DB_Apr_27_2016_08_29_06.rds")

#Cleaning up of the dates in the foodomics database so we can match appropriate profiles
foodomics$Date_Obtained<-as.numeric(foodomics$Date_Obtained)
foodomics$Date_Obtained<-as.Date(foodomics$Date_Obtained, origin = "1899-12-30")
foodomics$Date_Obtained[is.na(foodomics$Date_Obtained)] <- "2015-01-01"
foodomics$Date_Obtained<-as.POSIXlt(foodomics$Date_Obtained,format="%m/%d/%y")
foodomics$Month<-month(foodomics$Date_Obtained)
foodomics$Day<-day(foodomics$Date_Obtained)
foodomics$Year<-year(foodomics$Date_Obtained)
foodomics$MATCH_DATE<-paste(foodomics$Year, foodomics$Month, foodomics$Day, sep="-")
foodomics$MATCH_DATE<-as.Date(foodomics$MATCH_DATE)

#Cleaning up of the dates in the clean many foods sheet so we can match appropriate profiles
Manyfoods_DI$Date<-as.POSIXlt(Manyfoods_DI$Date,format="%m/%d/%y")
clean_Manyfoods_DI$Month<-month(clean_Manyfoods_DI$Date)
clean_Manyfoods_DI$Day<-day(clean_Manyfoods_DI$Date)
clean_Manyfoods_DI$Year<-year(clean_Manyfoods_DI$Date)
clean_Manyfoods_DI$MATCH_DATE<-paste(clean_Manyfoods_DI$Year, clean_Manyfoods_DI$Month, clean_Manyfoods_DI$Day, sep="-")
clean_Manyfoods_DI$MATCH_DATE<-as.Date(clean_Manyfoods_DI$MATCH_DATE)

#get list of NDIDs the patient ate, and create a database of just the NDIDs eaten by the patient
foodomics_eaten<- foodomics[foodomics$PRODUCTNDID %in% clean_Manyfoods_DI$PRODUCTNDID, ] 
#write.csv(foodomics , file="foodomicsTESTING.csv")

#condense the dataset into only the foodomic columns and not the columns in the NFD
foodomics_eaten<-foodomics_eaten[,c(1:179,581)]
memory.limit(size=20000)
check_dates <- abs(outer(clean_Manyfoods_DI$MATCH_DATE,foodomics_eaten$MATCH_DATE,"-")) #finds the closest date for every field pairing
check_ndid <- outer(clean_Manyfoods_DI$PRODUCTNDID,foodomics_eaten$PRODUCTNDID,"==") #determines if the NDID was the same for each field pairing
check_ndid[check_ndid==0]<-NA #changes 0 to NA
check_all <- check_dates*check_ndid #multiply the two arrays together to get a conditionally valid list of closeness
best_match <- apply(check_all, 1, function(i) which.min(i)) #find the smallest value in each conditionally valid list
best_match <- as.numeric(best_match)
result <- cbind(clean_Manyfoods_DI, foodomics_eaten[best_match,]) #merge the two arrays based on the result of the best_match array
result2_cleaned<-result[,c(1,3,4,5,6,2,7,15:24,8,26:64,69:191)]
write.csv(result2_cleaned, file="result_not_summed_not_scaled.csv")

#Calculations for scaled data but not summed
#Make values numeric
result2_cleaned[,-c(1:18)] <- sapply(result2_cleaned[,-c(1:18)], as.numeric)

#Multiply by intake
result2_cleaned[,-c(1:18)]=as.data.frame(apply( result2_cleaned[,-c(1:18)], 2, function(x) x *  result2_cleaned$Intake / 100))
write.csv(result2_cleaned, file="result_scaled_not_summed_for_DB.csv")

#Columns to use in the sum for daily summary
result_daily_summed <- aggregate(x = result2_cleaned[,-c(1:18)],
                                 FUN = sum,
                                 by = list(Group_date = result2_cleaned$Date), na.rm=TRUE)

write.csv(result_daily_summed, file="result_daily_summed.csv")