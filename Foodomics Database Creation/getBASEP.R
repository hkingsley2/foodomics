#####################
#####IMPORT FILES####
#####################
              
              #(1)
              #Read in the USDA database
              setwd("~/GitHub/foodomics/Foodomics Database Creation/Source Data")
              USDA<-read.csv(file="SR28_PROFILE_DATA.csv", header=TRUE) #import USDA profile data
              
              #Update/complete profiles with information that is available on macronutrients
              setwd("~/GitHub/foodomics/Foodomics Database Creation")
            
              source("getADJUSDA.R")
              
              #(2)
              #Read in the USDAweightedNFD
              setwd("~/GitHub/foodomics/Foodomics Database Creation/Source Data")
              USDAweightedNFD <- read.csv(file="USDA BASE PRODUCTS.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
              #Change product (weighting factor) to a numeric value
              USDAweightedNFD$X.OFPRODUCT<-as.numeric(USDAweightedNFD$X.OFPRODUCT)
              
              #Make sure that each of the NDIDs in the sheet add up to 1 (view results of ADD)
              setwd("~/GitHub/foodomics/Foodomics Database Creation/Source Data")
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
              preSCALING[,c(8:161)]<-preSCALING[,c(8:161)] * preSCALING[,c(5)]
              
              #(7) 
              #Now, we need to sum the data for the representative products for each NDID that they belong to
              #Sum the data in preSCALING PER PRODUCT except where values are NA then just use value
              #First get rid of columns that we don't need
              preSCALING2<-preSCALING[,c(2,8:157,160:161)]
              library(magrittr)
              library(dplyr)
              referenceBASEP<- preSCALING2 %>% group_by(PRODUCTNDID) %>% summarise_each(funs(sum))

######################################
#####CONVERT VITAMIN MINERAL UNITS####
######################################
              
              referenceBASEP$`X313`<-referenceBASEP$`X313`*0.000001
              referenceBASEP$`X317`<-referenceBASEP$`X317`*0.000001
              referenceBASEP$`X319`<-referenceBASEP$`X319`*0.000001
              referenceBASEP$`X320`<-referenceBASEP$`X320`*0.000001
              referenceBASEP$`X321`<-referenceBASEP$`X321`*0.000001
              referenceBASEP$`X322`<-referenceBASEP$`X322`*0.000001
              referenceBASEP$`X325`<-referenceBASEP$`X325`*0.000001
              referenceBASEP$`X326`<-referenceBASEP$`X326`*0.000001
              referenceBASEP$`X328`<-referenceBASEP$`X328`*0.000001
              referenceBASEP$`X334`<-referenceBASEP$`X334`*0.000001
              referenceBASEP$`X337`<-referenceBASEP$`X337`*0.000001
              referenceBASEP$`X338`<-referenceBASEP$`X338`*0.000001
              referenceBASEP$`X417`<-referenceBASEP$`X417`*0.000001
              referenceBASEP$`X418`<-referenceBASEP$`X418`*0.000001
              referenceBASEP$`X428`<-referenceBASEP$`X428`*0.000001
              referenceBASEP$`X429`<-referenceBASEP$`X429`*0.000001
              referenceBASEP$`X430`<-referenceBASEP$`X430`*0.000001
              referenceBASEP$`X431`<-referenceBASEP$`X431`*0.000001
              referenceBASEP$`X432`<-referenceBASEP$`X432`*0.000001
              referenceBASEP$`X435`<-referenceBASEP$`X435`*0.000001
              referenceBASEP$`X578`<-referenceBASEP$`X578`*0.000001
              referenceBASEP$`X318`<-referenceBASEP$`X318`*0.025*0.001
              referenceBASEP$`X324`<-referenceBASEP$`X324`* 0.3*0.001
              referenceBASEP$`X262`<-referenceBASEP$`X262`*0.001
              referenceBASEP$`X263`<-referenceBASEP$`X263`*0.001
              referenceBASEP$`X301`<-referenceBASEP$`X301`*0.001
              referenceBASEP$`X303`<-referenceBASEP$`X303`*0.001
              referenceBASEP$`X304`<-referenceBASEP$`X304`*0.001
              referenceBASEP$`X305`<-referenceBASEP$`X305`*0.001
              referenceBASEP$`X306`<-referenceBASEP$`X306`*0.001
              referenceBASEP$`X307`<-referenceBASEP$`X307`*0.001
              referenceBASEP$`X309`<-referenceBASEP$`X309`*0.001
              referenceBASEP$`X312`<-referenceBASEP$`X312`*0.001
              referenceBASEP$`X315`<-referenceBASEP$`X315`*0.001
              referenceBASEP$`X323`<-referenceBASEP$`X323`*0.001
              referenceBASEP$`X341`<-referenceBASEP$`X341`*0.001
              referenceBASEP$`X342`<-referenceBASEP$`X342`*0.001
              referenceBASEP$`X343`<-referenceBASEP$`X343`*0.001
              referenceBASEP$`X344`<-referenceBASEP$`X344`*0.001
              referenceBASEP$`X345`<-referenceBASEP$`X345`*0.001
              referenceBASEP$`X346`<-referenceBASEP$`X346`*0.001
              referenceBASEP$`X347`<-referenceBASEP$`X347`*0.001
              referenceBASEP$`X401`<-referenceBASEP$`X401`*0.001
              referenceBASEP$`X404`<-referenceBASEP$`X404`*0.001
              referenceBASEP$`X405`<-referenceBASEP$`X405`*0.001
              referenceBASEP$`X406`<-referenceBASEP$`X406`*0.001
              referenceBASEP$`X410`<-referenceBASEP$`X410`*0.001
              referenceBASEP$`X415`<-referenceBASEP$`X415`*0.001
              referenceBASEP$`X421`<-referenceBASEP$`X421`*0.001
              referenceBASEP$`X454`<-referenceBASEP$`X454`*0.001
              referenceBASEP$`X573`<-referenceBASEP$`X573`*0.001
              referenceBASEP$`X601`<-referenceBASEP$`X601`*0.001
              referenceBASEP$`X636`<-referenceBASEP$`X636`*0.001
              referenceBASEP$`X638`<-referenceBASEP$`X638`*0.001
              referenceBASEP$`X639`<-referenceBASEP$`X639`*0.001
              referenceBASEP$`X641`<-referenceBASEP$`X641`*0.001
              
              
              
#####################
#####SAVE RESULTS####
#####################
              
              setwd("~/GitHub/foodomics/Foodomics Database Creation/Output Data")
              saveRDS(referenceBASEP, file="reference_BASEP.rds")
              
              write.csv(referenceBASEP, file="reference_BASEP.csv")

#(8)
#We are finished. Now we have a database of base products that are 
#found as representative products in our food database (our current 
#food database, or historical food database, but this could also work 
#for our patient data)
