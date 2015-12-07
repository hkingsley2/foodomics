######################################################
#####Creating Time Series in R for Foodomics Data#####
######################################################

#Required Packages (these will install if you don't have them installed)
if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr) #for formatting structure of data
}
if(!require(data.table)){
  install.packages("data.table")
  library(data.table) #for creating values for some variables
}

if(!require(zoo)){
  install.packages("zoo")
  library(zoo) #for creating time series
}

if(!require(stringr)){
  install.packages("stringr")
  library(stringr) #for creating time series
}

#Required Docs (all in same folder)
#----Daily intakes saved as .csv file with date formatted as mm/dd/yyyy hh:mmm
#----New Data Tracking for obtaining diet changes (up to date)
#----Demographics table for obtaining the start date 
#(and for obtaining the stop date when needed, and for counting diet prescriptions)

#Required Formatting of Required Docs
#----for all sheets, right click date, select format values, 
#choose date, choose 03/12/2015 (for example)

#Explanation: The time series will be created by
#----1) using dates from new data tracking 
#(fill in list of dates from first date to last date available)
#----2) merge this time series with the time series from the daily intake data
#----3) prune the whole time series using the information from the demographics table

#Load foodomics dataset
foodomicsDB<-readRDS("foodomicsDB.RDS")

######################################################
##########Create the Full Length Time Series##########
######################################################

#Load the daily data
daily_intakes<-read.csv(file='thsc_diet_data.csv', header=TRUE, sep=",")

#Take first two columns that have Date and ME number information
daily_intakes<-daily_intakes[,c(1:2)]

#Rename these variables
daily_intakes<-rename(daily_intakes, Date=Date.of.Intake)
daily_intakes<-rename(daily_intakes, ME=ME.)

dt <- data.table(daily_intakes) #after this the date is mm/dd/yy 12:00 AM
test<-dt[, number := 1:.N, by = Date] #this numbers every row in the dt by date
data<-as.data.frame(test) #this converts the dt to a dataframe

data_wide <- reshape(data, direction="wide", idvar = "Date", timevar = "number")
#this reshape function casts the data frame into wide format, head(data_wide) to see
#this creates a column for each ME, meaning the max number of ME a patient has 
#is the number of columns that are created, different for each patient
#Removes any rows that are empty
data_wide<-data_wide[-which(data_wide == ""),]

data_wide$Date<-as.POSIXct(data_wide$Date,format="%m/%d/%y") # now date is formatted yyyy/mm/dd
data_wide.zoo<-zoo(data_wide[,-1],data_wide[,1]) #set date to Index in a zoo time series object
df2 <- merge(data_wide.zoo,zoo(,seq(start(data_wide.zoo),end(data_wide.zoo),by="DSTday")), all=TRUE) 
#fills in time series from the start of the first time series object to
#the end of the first time series object

final<-as.data.frame(df2) #make the zoo time series object a dataframe
#final2<-final[ !grepl( "23:00" , rownames(final) ) ,  ]
final$Date<-row.names(final)

######################################################
###########Bring in Data and Day Type Info############
######################################################

###############
###DATA TYPE###
###############
#Get the data and aggregate it so that there is only one DATT value per diet record
data_type<-read.csv(file='thsc_diet_data.csv', header=TRUE, sep=",")
Data.Type<-data_type[,c(1,3)]
Data.Type<-rename(Data.Type, Date=Date.of.Intake)
Data.Type<-rename(Data.Type, DATT=Data.Type)
Data.Type<-aggregate(Data.Type$DATT, by=list(Data.Type$Date), max)
Data.Type<-rename(Data.Type, Date=Group.1)
Data.Type<-rename(Data.Type, DATT=x)

#Fill in the time series
Data.Type$Date<-as.POSIXct(Data.Type$Date,format="%m/%d/%y")
Data.Type.zoo<-zoo(Data.Type[,-1],Data.Type[,1]) #set date to Index
df2 <- merge(Data.Type.zoo,zoo(,seq(start(Data.Type.zoo),end(Data.Type.zoo),by="DSTday")), all=TRUE)
Data.Type<-as.data.frame(df2)
Data.Type$Date<-row.names(Data.Type)
Data.Type<-rename(Data.Type, DATT=df2)

###############
####DAY TYPE###
###############

#Get the data and aggregate it so that there is only one value per date
day_type<-read.csv(file='thsc_diet_data.csv', header=TRUE, sep=",")
Day.Type<-day_type[,c(1,4)]
Day.Type<-rename(Day.Type, Date=Date.of.Intake)
Day.Type<-rename(Day.Type, DAYT=Day.Type)
Day.Type<-aggregate(Day.Type$DAYT, by=list(Day.Type$Date), max)
Day.Type<-rename(Day.Type, Date=Group.1)
Day.Type<-rename(Day.Type, DAYT=x)

#Fill in the time series
Day.Type$Date<-as.POSIXct(Day.Type$Date,format="%m/%d/%y")
Day.Type.zoo<-zoo(Day.Type[,-1],Day.Type[,1]) #set date to Index
df2 <- merge(Day.Type.zoo,zoo(,seq(start(Day.Type.zoo),end(Day.Type.zoo),by="DSTday")), all=TRUE)
Day.Type<-as.data.frame(df2)
Day.Type$Date<-row.names(Day.Type)
Day.Type<-rename(Day.Type, DAYT=df2)

#MERGE DAY TYPE AND DATA TYPE
types<-merge(Day.Type,Data.Type,by="Date")

###############
###MERGE ALL###
###############

#MERGE THE DAILY INTAKE DATA WITH THE QUALITY DATA
wide_daily_intakes<-merge(final,types,by="Date")
#wide_daily_intakes<-wide_daily_intakes[,c(1:7,9:10)] #remove unneeded date column

######################################################
##############Bring in Diet Change Info###############
######################################################

####Now, we need to bring in the data about the diet prescription changes.
diet_changes<-read.csv(file='thsc_ndt.csv', header=TRUE, sep=",")
diet_changes<-diet_changes[,c(1,4,5,6,8,9,10,12,13,14,15)]
diet_changes[is.na(diet_changes)]<- "0"
diet_changes<-diet_changes[!diet_changes$Diet.Date=="", ]
cols = c(2:11);    
diet_changes[,cols] = apply(diet_changes[,cols], 2, function(x) as.numeric(as.character(x)))
diet_changes$sum<-rowSums(diet_changes[, c(2:11)])

#Fill in the time series.
diet_changes$Diet.Date<-as.POSIXct(diet_changes$Diet.Date,format="%m/%d/%y")
diet_changes <- aggregate(diet_changes$sum, by=list(diet_changes$Diet.Date), max)
diet_changes.zoo<-zoo(diet_changes[,-1],diet_changes[,1]) #set date to Index
df2 <- merge(diet_changes.zoo,zoo(,seq(start(diet_changes.zoo),end(diet_changes.zoo),by="DSTday")), all=TRUE)
diet_changes<-as.data.frame(df2)
diet_changes$Date<-row.names(diet_changes)
diet_changes<-rename(diet_changes, sum=df2)
#diet_changes[is.na(diet_changes)]<- "0"
diet_changes$sum2<-na.locf(diet_changes$sum)

###############
###MERGE ALL###
###############

#merge the diet change data with the daily intake data
final_daily_intake<-merge(wide_daily_intakes,diet_changes,by="Date", all=TRUE)
#Now we have a dataframe that we can use to implement LCOF and NOCB methods that rely on the changes in the Reason.For.Change column
#We will need to prune this dataframe to the dates that the patient was actually on therapy for.

######################################################
##############FILL IN TIME SERIES DATA################
######################################################

#Now, I am going to combine the columns such that we have one 'intake' column for meal equivalents and days
#The code below will give the intake code a value of NA if the ME.1 is na, but it will give the intake code if there is an intake code.
final_daily_intake$intakecode<- as.factor(ifelse(is.na(final_daily_intake$ME.1), final_daily_intake$ME.1,
                                   paste(final_daily_intake$ME.1, 
                                      final_daily_intake$ME.2,
                                      final_daily_intake$ME.3,
                                      final_daily_intake$ME.4,
                                      final_daily_intake$ME.5,
                                      final_daily_intake$ME.6,
                                      final_daily_intake$DAYT,
                                      final_daily_intake$DATT,sep=",")))

setDT(final_daily_intake)
#TIME SERIES INPUT
      final_daily_intake[,intakecode:=na.locf(intakecode,na.rm=FALSE),
                         by=list(as.factor(sum2))] #LOCF
      final_daily_intake[,intakecode:=na.locf(intakecode,na.rm=FALSE,fromLast=TRUE),
                         by=list(as.factor(sum2))] #NOCB

head(final_daily_intake)

setDT(final_daily_intake)[, paste0("Column", 1:8) := tstrsplit(intakecode, ",")]


final_daily_intake<-str_split_fixed(final_daily_intake$intakecode, ",", 8)

head(final_daily_intake)