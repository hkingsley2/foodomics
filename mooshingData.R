mooshIntake.R


melted_daily_intakes



setwd("Z:/MySQL Database/Diet/Raw_Data/Dec2015/Dec2015Data_deid/Test_0196")
daily_menus<-read.csv(file='KG0196_menus.txt', header=TRUE, sep="\t", na.strings=c("","NA"))


setwd("Z:/MySQL Database/Diet/Reference_Tables")
foodomics<-readRDS(file="foodomics_DB_Feb_18_2016_13_38_34.rds")



What we want to do is moosh the mes with the daily intakes


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





#Now we want to merge the wide menus with the daily intakes BY THE PKT RECIPE NUMBER

Foods_by_DI<-merge(melted_daily_intakes,menus_wide_ID, by=c("MRNUMBER","PKT_Recipe_Number"))
write.csv(Foods_by_DI , file="Foods_by_DI.csv")
#Now make the foods in long format so that we can attribute a profile to them from the foodomics database

Manyfoods_DI <- melt(Foods_by_DI ,  id.vars = c("MRNUMBER","PKT_Recipe_Number","Date","variable"), variable.name = 'PRODUCTNDID')
write.csv(Manyfoods_DI , file="Manyfoods_DI.csv")

#the above function generates a lot of rows with blank data, so lets get rid of them
clean_Manyfoods_DI<-Manyfoods_DI[!is.na(Manyfoods_DI$Intake),]

#Now we want to merge in the values to use in scaling
Foods_by_DI_2<-merge(melted_daily_intakes,menus_wide_amt, by=c("MRNUMBER","PKT_Recipe_Number"))
write.csv(Foods_by_DI_2, file="Foods_by_DI_2.csv")

#Now make the foods in long format so that we can attribute a profile to them from the foodomics database

Manyfoods_DI_2 <- melt(Foods_by_DI_2,  id.vars = c("MRNUMBER","PKT_Recipe_Number","Date","variable"), variable.name = 'Intake')
write.csv(Manyfoods_DI_2 , file="Manyfoods_DI_2.csv")

#the above function generates a lot of rows with blank data, so lets get rid of them

clean_Manyfoods_DI_2<-Manyfoods_DI_2[!is.na(Manyfoods_DI_2$value),]

####NOW COMBINE THE DATAFRAME WITH THE INGREDIENT NUMBERS AND AMOUNTS


############
############
############
############
############
#WINNER#####
############
############
############
############
clean_Manyfoods_DI$Intake<-clean_Manyfoods_DI_2$value
write.csv(clean_Manyfoods_DI, file="clean_Manyfoods_DI_comb.csv")
############
############
############
############
############
#WINNER#####
############
############
############
############

#fix dates so we can match appropriate profiles
foodomics$Date_Obtained<-as.numeric(foodomics$Date_Obtained)
foodomics$Date_Obtained<-as.Date(foodomics$Date_Obtained, origin = "1899-12-30")
foodomics$Date_Obtained[is.na(foodomics$Date_Obtained)] <- "2015-01-01"
foodomics$Date_Obtained<-as.POSIXlt(foodomics$Date_Obtained,format="%m/%d/%y")

Manyfoods_DI$Date<-as.POSIXlt(Manyfoods_DI$Date,format="%m/%d/%y")

attributes(Manyfoods_DI$MATCH_DATE)
attributes(foodomics$MATCH_DATE)


clean_Manyfoods_DI$Month<-month(clean_Manyfoods_DI$Date)
clean_Manyfoods_DI$Day<-day(clean_Manyfoods_DI$Date)
clean_Manyfoods_DI$Year<-year(clean_Manyfoods_DI$Date)
clean_Manyfoods_DI$MATCH_DATE<-paste(clean_Manyfoods_DI$Year, clean_Manyfoods_DI$Month, clean_Manyfoods_DI$Day, sep="-")
clean_Manyfoods_DI$MATCH_DATE<-as.Date(clean_Manyfoods_DI$MATCH_DATE)

foodomics$Month<-month(foodomics$Date_Obtained)
foodomics$Day<-day(foodomics$Date_Obtained)
foodomics$Year<-year(foodomics$Date_Obtained)
foodomics$MATCH_DATE<-paste(foodomics$Year, foodomics$Month, foodomics$Day, sep="-")
foodomics$MATCH_DATE<-as.Date(foodomics$MATCH_DATE)

#get list of NDIDs he ate
foodomics_eaten<- foodomics[foodomics$PRODUCTNDID %in% clean_Manyfoods_DI$value, ] 

clean_Manyfoods_DI222<-clean_Manyfoods_DI[,-c(3:5,8:10)]
clean_Manyfoods_DI222$PRODUCTNDID<-clean_Manyfoods_DI222$value
temp <- abs(outer(clean_Manyfoods_DI222$MATCH_DATE,foodomics_eaten$MATCH_DATE,"-"))
ind <- apply(temp, 1, function(i) which.min(i))
ind<-as.numeric(ind)
result<-merge(clean_Manyfoods_DI222, foodomics_eaten[ind,], by=c("PRODUCTNDID"))
#result <- cbind(clean_Manyfoods_DI222, foodomics_eaten[ind,])
write.csv(result , file="result_not_summed.csv")

#Calculations
result2 <- result[,c(1:7,24:190,426:431,584,587)]
result2[,-c(1:6,182)] <- sapply(result2[,-c(1:6,182)], as.numeric)
result3=as.data.frame(apply( result2[,-c(1:6,182)], 2, function(x) x *  result2$Intake / 100))
result3$MATCH_DATE<-result2$MATCH_DATE
write.csv(result3, file="result_scaled.csv")


#Columns to use in the sum for daily summary
result4<-result3[,-c(1)]
#result_daily[,c(-175)] <- sapply(result_daily[,c(-175)], as.numeric)
result_daily_summed <- aggregate(x = result4[,c(-175)],
                     FUN = sum,
                     by = list(Group.date = result4$MATCH_DATE), na.rm=TRUE)

#result4 %>% group_by(MATCH_DATE) %>% summarise_each(funs(sum))
#notworking<-by(result4, result4$MATCH_DATE, function(x) colSums(result4[,-c(175)]))

write.csv(result_daily_summed, file="result_daily_summed.csv")