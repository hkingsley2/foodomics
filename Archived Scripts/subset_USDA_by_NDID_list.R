########################################################
#####Subset USDA Nutrient Database by a List of IDs#####
########################################################

#Import required files
#----1) List of IDs (where ID column is named NDID) (a dataframe named 'LIST')
#----2) Compiled or current NFD (where ID column is named NDID and USDA ID is NDB_No)
#----3) USDA database (profile data) (where ID column is NDB_No)

##########
###USDA###
##########
          NFL<- read.csv(file = "Data/WebsiteNFD2.csv", header = TRUE,
                         na.strings=c("NA","NaN", " "), encoding = 'ASCI', fileEncoding='UTF-8')
          
          USDA<-read.csv(file="TEST_SR28_PROFILE_DATA.csv", header=TRUE) #import USDA profile data
          #  need to import blanks as NA
          USDAdef<-read.csv(file="NUTR_DEF.csv", header=FALSE) #import USDA profile definitions
          USDAdef$V1<-gsub("~", "", USDAdef$V1)
          USDAdef$V2<-gsub("~", "", USDAdef$V2)
          USDAdef$V3<-gsub("~", "", USDAdef$V3)
          
##########
####NFL###
##########
          NFL<-read.csv(file="TEST_NFL.txt", sep="\t", header = TRUE,
                        na.strings=c("NA","NaN", " "), encoding = 'ASCI', fileEncoding='UTF-8') #import nutrition facts label database
          #rename a column
          names(NFL)[names(NFL)=="USDA_No"] <- "NDB_No"
          NFL[,5:60] <- sapply(NFL[, 5:60], as.numeric)
          NFL[NFL==""] <- NA
          
          #Subet NFL to list of IDs provided
          NFLmod = NFL[NFL$NDID %in% LIST$NDID, ] 
          
###########################
####Combine NFL AND USDA###
###########################
          #Subset USDA database to foods in the NFL database
          USDAmod = USDA[USDA$NDB_No %in% NFLmod$NDB_No, ] 
          #Merge data
          NFL_USDA = merge(x = USDAmod, y = NFLmod, by = "NDB_No", all = TRUE)
          #Save data as a .csv file
          write.csv(NFL_USDA, file="NFL_USDA.csv")
          
