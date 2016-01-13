#Note: Not all NDIDs have base products, therefore we will need to prune the eaten NDIDs list some (eatenNDIDs list comes from getNDIDs.R)

#Read in the USDAweightedNFD
USDAweightedNFD <- read.csv(file="USDA BASE PRODUCTS.txt", header=TRUE, sep="\t")

#Subset the USDAweighted NFD by foods in eatenNDIDs (note that there is a 1:many relationship between eatenNDIDs to USDAweightedNFD, because a patient can eat a food that has multiple USDA base products)
condensedUSDAweightedNFD = USDAweightedNFD[USDAweightedNFD$PRODUCTNDID %in% eatenNDIDs, ] 

#We only need a few columns of this
#condensedUSDAweightedNFD =condensedUSDAweightedNFD[,c(1:4,11)]

#Read in the USDA database
USDA<-read.csv(file="SR28_PROFILE_DATA.csv", header=TRUE) #import USDA profile data
source("getADJUSDA.R")

#Subset USDA profile database to list of IDS in condensedUSDAweightedNFD
USDAmod = USDA[USDA$NDB_No %in% condensedUSDAweightedNFD$USDAID, ] 

library(plyr)
condensedUSDAweightedNFD<-rename(condensedUSDAweightedNFD, c("USDAID"="NDB_No"))

#Merge this database back with the condensed database to have everything in one sheet
preSCALING <- merge(condensedUSDAweightedNFD,USDAmod, by="NDB_No")

#multiply values by weighting factor
preSCALING[,c(7:156)]<-preSCALING[,c(7:156)] * preSCALING[,c(5)]

