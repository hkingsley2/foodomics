#####################
#####IMPORT FILES####
#####################
              
              #(1)
              #Read in the USDA database
              setwd("Z:/MySQL Database/Diet/Reference_Tables/1_FoodomicsPreProcessing")
              USDA<-read.csv(file="SR28_PROFILE_DATA.csv", header=TRUE) #import USDA profile data
              #Update/complete profiles with information that is available on macronutrients
              setwd("~/GitHub/foodomics")
              source("getADJUSDA.R")
              
              #(2)
              #Read in the USDAweightedNFD
              setwd("Z:/MySQL Database/Diet/Reference_Tables/1_FoodomicsPreProcessing")
              USDAweightedNFD <- read.csv(file="USDA BASE PRODUCTS.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
              #Change product (weighting factor) to a numeric value
              USDAweightedNFD$X.OFPRODUCT<-as.numeric(USDAweightedNFD$X.OFPRODUCT)
              
              #Make sure that each of the NDIDs in the sheet add up to 1 (view results of ADD)
              ADD<- aggregate(USDAweightedNFD$X.OFPRODUCT, by=list(Category=USDAweightedNFD$PRODUCTNDID), FUN=sum)
              write.csv(ADD, file="CHECKING_BASE_PRODUCTS.csv")

##################################
#####OBTAIN NDIDS AND PROFILES####
##################################
              
              #(3)
              #Obtain NDIDs from the USDAweightedNFD sheet
              eatenNDIDs <- unique(USDAweightedNFD$PRODUCTNDID)
              
              #(4)
              #Subset the USDAweightedNFD by foods in eatenNDIDs (note that there is a 1:many relationship between eatenNDIDs to USDAweightedNFD, because a patient can eat a food that has multiple USDA base products)
              condensedUSDAweightedNFD = USDAweightedNFD[USDAweightedNFD$PRODUCTNDID %in% eatenNDIDs, ] 
              #Note: in some cases, the list of eatenNDIDs will just be the NDIDs in the food database being analyzed
              #In that case, you need to obtain the list of NDIDs for the food database you want to analyze
              
              #condensedUSDAweightedNFD =condensedUSDAweightedNFD[,c(1:4,11)]
              
              #(5)
              #Subset USDA profile database to list of IDS in condensedUSDAweightedNFD
              USDAmod = USDA[USDA$NDB_No %in% condensedUSDAweightedNFD$USDA.ID, ] 
              
              library(plyr)
              #rename the USDAID column to NDB_No because that is what it is called in the USDAmod sheet
              condensedUSDAweightedNFD$NDB_No<-condensedUSDAweightedNFD$USDA.ID
              condensedUSDAweightedNFD<-condensedUSDAweightedNFD[,c(-2)]
              
              #Merge this database back with the condensed database to have everything in one sheet
              preSCALING <- merge(condensedUSDAweightedNFD,USDAmod, by="NDB_No")

###############################
#####SCALE AND SUM PROFILES####
###############################
              
              #(6)
              #multiply values by weighting factor
              preSCALING[,c(7:160)]<-preSCALING[,c(7:160)] * preSCALING[,c(5)]
              
              #(7) 
              #Now, we need to sum the data for the representative products for each NDID that they belong to
              #Sum the data in preSCALING PER PRODUCT except where values are NA then just use value
              #First get rid of columns that we don't need
              preSCALING2<-preSCALING[,c(2,7:156,160)]
              library(magrittr)
              library(dplyr)
              referenceBASEP<- preSCALING2 %>% group_by(PRODUCTNDID) %>% summarise_each(funs(sum))

#####################
#####SAVE RESULTS####
#####################
              
              saveRDS(referenceBASEP, file="reference_BASEP_based_on_02022016_compiledDB.rds")
              
              write.csv(referenceBASEP, file="reference_BASEP_based_on_02022016_compiledDB.csv")

#(8)
#We are finished. Now we have a database of base products that are 
#found as representative products in our food database (our current 
#food database, or historical food database, but this could also work 
#for our patient data)
