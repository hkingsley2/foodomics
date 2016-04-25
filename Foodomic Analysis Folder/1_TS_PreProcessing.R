#---Probably need to make this run through patients in a dataframe list, and may need to make a data\
#--frame list out of each patient for each diet prescription, and then run this code through that. 
#---need to add in the fact that days that are 5 should all get NA values, but I need a clean sheet first.

######################################################
#####Creating Time Series in R for Foodomics Data#####
######################################################



#SET PATIENT FOLDER

#if current
patientfolder<-"Z:/Data_D/D18/Clinic/Patient Folders/RoAr01456840"

#if non current
patientfolder<-"Z:/Data_D/D18/Clinic/Patient Folders/Non-Current Patients/HaZe1225262_AJ33KGN/Test"




#Required Packages (these will install if you don't have them installed)

if(!require(plyr)){
  install.packages("plyr")
  library(plyr) #for formatting structure of data
}
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

if(!require(reshape2)){ 
  install.packages("reshape2")
  library(reshape2) #for creating time series
}

if(!require(lubridate)){ 
  install.packages("lubridate")
  library(lubridate) #for creating time series
}

if(!require(ggplot2)){ 
  install.packages("ggplot2")
  library(ggplot2) #for creating time series
}

if(!require(data.table)){ 
  install.packages("data.table")
  library(data.table) #for creating time series
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
#foodomicsDB<-readRDS("foodomicsDB.RDS")

######################################################
##########Create the Full Length Time Series##########
######################################################

#Load the daily data
setwd(patientfolder)
daily_intakes<-read.csv(file='Daily_Intake.txt', header=TRUE, sep="\t", na.strings=c("","NA"))

#Take first two columns that have Date and ME number information
daily_intakes<-daily_intakes[,c(1:3)]

#Rename these variables
daily_intakes<-rename(daily_intakes, ME=PKT_Recipe_Number)

dt <- data.table(daily_intakes) #after this the date is mm/dd/yy 12:00 AM
test<-dt[, number := 1:.N, by = Date] #this numbers every row in the dt by date
data<-as.data.frame(test) #this converts the dt to a dataframe

data_wide <- reshape(data, direction="wide", idvar = c("MRNUMBER","Date"), timevar = "number")
#this reshape function casts the data frame into wide format, head(data_wide) to see
#this creates a column for each ME, meaning the max number of ME a patient has 
#is the number of columns that are created, different for each patient
#Removes any rows that are empty
#data_wide<-data_wide[-which(data_wide == ""),]

#This pastes all ME columns together
cols<-names(data_wide[, c(3:ncol(data_wide))])
data_wide$intakecode<- as.factor(ifelse(is.na(data_wide$ME.1), data_wide$ME.1,
                                        apply( data_wide[ , cols ] , 1 , paste , collapse = "," )))

########Maybe get rid of the other ME data
data_wide<-data_wide[,c("MRNUMBER","Date","intakecode")]

#################
###TIME SERIES###
#################

data_wide$Date<-as.POSIXct(data_wide$Date,format="%m/%d/%y") # now date is formatted yyyy/mm/dd
data_wide.zoo<-zoo(data_wide[,],data_wide[,c("Date")]) #set date to Index in a zoo time series object
df2 <- merge(data_wide.zoo,zoo(,seq(start(data_wide.zoo),end(data_wide.zoo),by="DSTday")), all=TRUE) 
#fills in time series from the start of the first time series object to
#the end of the first time series object

final<-as.data.frame(df2) #make the zoo time series object a dataframe
#final2<-final[ !grepl( "23:00" , rownames(final) ) ,  ]
final$Date<-row.names(final)

######################################################
###########Bring in Data and Day Type Info############
######################################################

##################
###DATA QUALITY###
##################
#Get the data and aggregate it so that there is only one DATT value per diet record
setwd(patientfolder)
data_type<-read.csv(file='Daily_Intake.txt', header=TRUE, sep="\t", na.strings=c("","NA"))

Data.Type<-data_type[,c(1,2,4)]
#Data.Type<-rename(Data.Type, Date=Date.of.Intake)
Data.Type<-rename(Data.Type, DATT=Data_Quality)
Data.Type<-aggregate(Data.Type$DATT, by=list(Data.Type$Date,Data.Type$MRNUMBER), max)
Data.Type<-rename(Data.Type, Date=Group.1)
Data.Type<-rename(Data.Type, MRNUMBER=Group.2)
Data.Type<-rename(Data.Type, DATT=x)

#Fill in the time series
Data.Type$Date<-as.POSIXct(Data.Type$Date,format="%m/%d/%y")
Data.Type.zoo<-zoo(Data.Type[,-1],Data.Type[,c("Date")]) #set date to Index
df2 <- merge(Data.Type.zoo,zoo(,seq(start(Data.Type.zoo),end(Data.Type.zoo),by="DSTday")), all=TRUE)
Data.Type<-as.data.frame(df2)
Data.Type$Date<-row.names(Data.Type)
#Data.Type<-rename(Data.Type, DATT=df2)

##################
####DAY QUALITY###
##################

#Get the data and aggregate it so that there is only one value per date
setwd(patientfolder)
day_type<-read.csv(file='Daily_Intake.txt', header=TRUE, sep="\t", na.strings=c("","NA"))
Day.Type<-day_type[,c(1,2,5)]
#Day.Type<-rename(Day.Type, Date=Date.of.Intake)
Day.Type<-rename(Day.Type, DAYT=Day_Quality)
Day.Type<-aggregate(Day.Type$DAYT, by=list(Day.Type$Date,Day.Type$MRNUMBER), max)
Day.Type<-rename(Day.Type, Date=Group.1)
Day.Type<-rename(Day.Type, MRNUMBER=Group.2)
Day.Type<-rename(Day.Type, DAYT=x)

#Fill in the time series
Day.Type$Date<-as.POSIXct(Day.Type$Date,format="%m/%d/%y")
Day.Type.zoo<-zoo(Day.Type[,-1],Day.Type[,c("Date")]) #set date to Index
df2 <- merge(Day.Type.zoo,zoo(,seq(start(Day.Type.zoo),end(Day.Type.zoo),by="DSTday")), all=TRUE)
Day.Type<-as.data.frame(df2)
Day.Type$Date<-row.names(Day.Type)
#Day.Type<-rename(Day.Type, DAYT=df2)

#MERGE DAY TYPE AND DATA TYPE
types<-merge(Day.Type,Data.Type,by=c("MRNUMBER","Date"))

###############
###MERGE ALL###
###############
#MERGE THE DAILY INTAKE DATA WITH THE QUALITY DATA
wide_daily_intakes<-merge(final,types,by=c("MRNUMBER","Date"))
#wide_daily_intakes<-wide_daily_intakes[,c(1:7,9:10)] #remove unneeded date column

######################################################
##############Bring in Diet Change Info###############
######################################################

####Now, we need to bring in the data about the diet prescription changes.
setwd(patientfolder)

diet_changes<-read.csv(file='Diet_Rx.txt', header=TRUE, sep="\t") #NEED TO ADD MRNUMBER TO THIS FILE
diet_changes<-diet_changes[!diet_changes$Date_of_Change=="", ] #gets rid of any blank rows in the file
diet_changes[is.na(diet_changes)]<- "0" #gives a 0 to any blank cell for calculation purposes

cols = c(4:13);     #these are the columns that actually have diet prescription information in them
diet_changes[,cols] = apply(diet_changes[,cols], 2, function(x) as.numeric(as.character(x)))
diet_changes$sum<-rowSums(diet_changes[, cols])

#Fill in the time series.  #########I may want to imrpove this later because really each unique line here is simply the diet prescription identifier
diet_changes$Date_of_Change<-as.POSIXct(diet_changes$Date_of_Change,format="%m/%d/%y")
diet_changes <- aggregate(diet_changes$sum, by=list(diet_changes$Date_of_Change), max)
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
final_daily_intake<-merge(wide_daily_intakes,diet_changes[, c("Date", "sum2")],by="Date", all=TRUE)
#Now we have a dataframe that we can use to implement LCOF and NOCB methods that rely on the changes in the Reason.For.Change column
#We will need to prune this dataframe to the dates that the patient was actually on therapy for.

####################################################################
setDT(final_daily_intake)
final_daily_intake2<-final_daily_intake[,intakecode:=na.locf(intakecode,na.rm=TRUE,fromLast=FALSE),
                                        by=as.factor(sum2)]

final_daily_intake2<-final_daily_intake2[,intakecode:=na.locf(intakecode,na.rm=TRUE,fromLast=TRUE)]

##TEXT TO COLUMNS FOR INTAKE CODE DATA, probably not ready to go yet
setDT(final_daily_intake2)[, paste0("Column", 1:2) := tstrsplit(intakecode, ",")]




#Melt the data so that we can being to import the profile information
final_daily_intake2<-as.data.frame(final_daily_intake2)
final_daily_intake3 <- final_daily_intake2[,colSums(is.na(final_daily_intake2))<nrow(final_daily_intake2)]
pruned<-final_daily_intake3[,c(1:2,7:15)]
melted_daily_intakes<-melt(pruned, id.vars = c("Date","MRNUMBER"))
melted_daily_intakes<-melted_daily_intakes[!melted_daily_intakes$value=="NA",]
#Rename these variables
melted_daily_intakes<-rename(melted_daily_intakes, PKT_Recipe_Number=value)
write.csv(melted_daily_intakes , file="melted_daily_intakes.csv")


#test2[,c(paste0("ME_", 1:9))]<-str_split_fixed(test2$intakecode, ",", 9)