#Read in the USDAweightedNFD
USDAweightedNFD <- read.csv(file="USDAweightedNFD.txt", header=TRUE, sep="\t")

#Subset the USDAweighted NFD by foods in eatenNDIDs (note that there is a 1:many relationship between eatenNDIDs to USDAweightedNFD, because a patient can eat a food that has multiple USDA base products)
condensedUSDAweightedNFD = USDAweightedNFD[USDAweightedNFD$New.NDID.Number %in% eatenNDIDs, ] 

#We only need a few columns of this
condensedUSDAweightedNFD =condensedUSDAweightedNFD[,c(1:4,11)]

#Read in the USDA database
USDA<-read.csv(file="TEST_SR28_PROFILE_DATA.csv", header=TRUE) #import USDA profile data

#Subset USDA profile database to list of IDS in condensedUSDAweightedNFD
USDAmod = USDA[USDA$NDB_No %in% condensedUSDAweightedNFD$USDA.New.DB.Number, ] 

library(plyr)
condensedUSDAweightedNFD<-rename(condensedUSDAweightedNFD, c("USDA.New.DB.Number"="NDB_No"))

#Merge this database back with the condensed database to have everything in one sheet
preSCALING <- merge(condensedUSDAweightedNFD,USDAmod, by="NDB_No")

preSCALING[,c(7:156)]<-preSCALING[,c(7:156)] * preSCALING[,c(5)]

