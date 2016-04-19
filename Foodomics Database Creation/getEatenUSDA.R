#Get USDA products eaten


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
USDAweightedNFD$PRODUCTNDID<-USDAweightedNFD$NDID.FOR.USDA.PRODUCTS
USDAweightedNFD$NDB_No<-USDAweightedNFD$USDA.ID
neededUSDA<-USDAweightedNFD[,c(1,6)]

#(3) Merge USDA DB with the weighted part
eatenUSDAfoods <- unique(merge(neededUSDA, USDA, by="NDB_No"))


#The result is a formatted USDA database with NDID labels as PRODUCT NDID that can be concatenated

#We just need to add the right coulmn headers
########################################################
#####Create a USDA dataframe with the FNA column headers
########################################################


###Macronutrient Rule: if NFL data are non-NA, use NFL data, else, use eatenUSDAfoods data
eatenUSDAfoods$Product_Name<-eatenUSDAfoods$Shrt_Desc
eatenUSDAfoods$FNA_PROCNT<-eatenUSDAfoods$`X203`
eatenUSDAfoods$FNA_FAT<-eatenUSDAfoods$`X204`
eatenUSDAfoods$FNA_CHOCDF<-eatenUSDAfoods$CHOdec2
eatenUSDAfoods$FNA_ENERC_KCAL<-eatenUSDAfoods$`X208`

###Macronutrient Sub-Class Rule: if NFL data for sub-class are non-NA, use NFL sub-class data, else, use NFL macronutrient data factor for sub-class

################
#CHO CONVERSIONS
################

eatenUSDAfoods$FNA_ALC<-eatenUSDAfoods$`X221`
eatenUSDAfoods$FNA_FIBTG<-eatenUSDAfoods$`X291`
eatenUSDAfoods$FNA_LACS<-eatenUSDAfoods$`X213`
eatenUSDAfoods$FNA_MALS<-eatenUSDAfoods$`X214`
eatenUSDAfoods$FNA_FRUS<-eatenUSDAfoods$`X212`
eatenUSDAfoods$FNA_GALS<-eatenUSDAfoods$`X287`
eatenUSDAfoods$FNA_GLUS<-eatenUSDAfoods$`X211`
eatenUSDAfoods$FNA_STARCH<-eatenUSDAfoods$`X209`
eatenUSDAfoods$FNA_SUCS<-eatenUSDAfoods$`X210`
eatenUSDAfoods$FNA_SUGAR<-eatenUSDAfoods$`X269`

################
#PRO CONVERSIONS
################

eatenUSDAfoods$FNA_ALA_G<-eatenUSDAfoods$`X513`
eatenUSDAfoods$FNA_ARG_G<-eatenUSDAfoods$`X511`
eatenUSDAfoods$FNA_ASP_G<-eatenUSDAfoods$`X514`
eatenUSDAfoods$FNA_CYS_G<-eatenUSDAfoods$`X507`
eatenUSDAfoods$FNA_GLU_G<-eatenUSDAfoods$`X515`
eatenUSDAfoods$FNA_GLY_G<-eatenUSDAfoods$`X516`
eatenUSDAfoods$FNA_HISTN_G<-eatenUSDAfoods$`X512`
eatenUSDAfoods$FNA_HYP<-eatenUSDAfoods$`X521`
eatenUSDAfoods$FNA_ILE_G<-eatenUSDAfoods$`X503`
eatenUSDAfoods$FNA_LEU_G<-eatenUSDAfoods$`X504`
eatenUSDAfoods$FNA_LYS_G<-eatenUSDAfoods$`X505`
eatenUSDAfoods$FNA_MET_G<-eatenUSDAfoods$`X506`
eatenUSDAfoods$FNA_PHE_G<-eatenUSDAfoods$`X508`
eatenUSDAfoods$FNA_PRO_G<-eatenUSDAfoods$`X517`
eatenUSDAfoods$FNA_SER_G<-eatenUSDAfoods$`X518`
eatenUSDAfoods$FNA_THR_G<-eatenUSDAfoods$`X502`
eatenUSDAfoods$FNA_TRP_G<-eatenUSDAfoods$`X501`
eatenUSDAfoods$FNA_TYR_G<-eatenUSDAfoods$`X509`
eatenUSDAfoods$FNA_VAL_G<-eatenUSDAfoods$`X510`

################
#FAT CONVERSIONS
################

#If NFL DATA FOR A FAT SUB-CLASS ARE AVAILABLE
#Do we want to adjust their TOTAL MUFA by taking the sum of MUFAs or their total mufa category
eatenUSDAfoods$FNA_FAMS<-eatenUSDAfoods$`X645`
eatenUSDAfoods$FNA_FAPU<-eatenUSDAfoods$`X646`
eatenUSDAfoods$FNA_FASAT<-eatenUSDAfoods$`X606`
eatenUSDAfoods$FNA_FATRN<-eatenUSDAfoods$`X605`

######SFA
eatenUSDAfoods$FNA_F4D0<-eatenUSDAfoods$`X607`
eatenUSDAfoods$FNA_F6D0<-eatenUSDAfoods$`X608`
eatenUSDAfoods$FNA_F8D0<-eatenUSDAfoods$`X609`
eatenUSDAfoods$FNA_F10D0<-eatenUSDAfoods$`X610`
eatenUSDAfoods$FNA_F12D0<-eatenUSDAfoods$`X611`
eatenUSDAfoods$FNA_F13D0<-eatenUSDAfoods$`X696`
eatenUSDAfoods$FNA_F14D0<-eatenUSDAfoods$`X612`
eatenUSDAfoods$FNA_F15D0<-eatenUSDAfoods$`X652`
eatenUSDAfoods$FNA_F16D0<-eatenUSDAfoods$`X613`
eatenUSDAfoods$FNA_F17D0<-eatenUSDAfoods$`X653`
eatenUSDAfoods$FNA_F18D0<-eatenUSDAfoods$`X614`
eatenUSDAfoods$FNA_F20D0<-eatenUSDAfoods$`X615`
eatenUSDAfoods$FNA_F22D0<-eatenUSDAfoods$`X624`
eatenUSDAfoods$FNA_F24D0<-eatenUSDAfoods$`X654`

######MUFA
eatenUSDAfoods$FNA_F14D1<-eatenUSDAfoods$`X625`
eatenUSDAfoods$FNA_F15D1<-eatenUSDAfoods$`X697`
eatenUSDAfoods$FNA_F17D1<-eatenUSDAfoods$`X687`
eatenUSDAfoods$FNA_F20D1<-eatenUSDAfoods$`X628`
eatenUSDAfoods$FNA_F16D1C<-eatenUSDAfoods$`X673`
eatenUSDAfoods$FNA_F16D1<-eatenUSDAfoods$`X626`
eatenUSDAfoods$FNA_F18D1C<-eatenUSDAfoods$`X674`
eatenUSDAfoods$FNA_F18D1<-eatenUSDAfoods$`X617`
eatenUSDAfoods$FNA_F22D1C<-eatenUSDAfoods$`X676`
eatenUSDAfoods$FNA_F22D1<-eatenUSDAfoods$`X630`
eatenUSDAfoods$FNA_F24D1C<-eatenUSDAfoods$`X671`

######PUFA
eatenUSDAfoods$FNA_F18D4<-eatenUSDAfoods$`X627`
eatenUSDAfoods$FNA_F21D5<-eatenUSDAfoods$`X857`
eatenUSDAfoods$FNA_F22D4<-eatenUSDAfoods$`X858`
eatenUSDAfoods$FNA_F18D2<-eatenUSDAfoods$`X618`
eatenUSDAfoods$FNA_F18D3CN3<-eatenUSDAfoods$`X851`
eatenUSDAfoods$FNA_F18D3CN6<-eatenUSDAfoods$`X685`
eatenUSDAfoods$FNA_F18D3<-eatenUSDAfoods$`X619`
eatenUSDAfoods$FNA_F20D2CN6<-eatenUSDAfoods$`X672`
eatenUSDAfoods$FNA_F20D3N3<-eatenUSDAfoods$`X852`
eatenUSDAfoods$FNA_F20D3N6<-eatenUSDAfoods$`X853`
eatenUSDAfoods$FNA_F20D3<-eatenUSDAfoods$`X689`
eatenUSDAfoods$FNA_F20D4N6<-eatenUSDAfoods$`X855`
eatenUSDAfoods$FNA_F20D4<-eatenUSDAfoods$`X620`
eatenUSDAfoods$FNA_F20D5<-eatenUSDAfoods$`X629`
eatenUSDAfoods$FNA_F22D5<-eatenUSDAfoods$`X631`
eatenUSDAfoods$FNA_F22D6<-eatenUSDAfoods$`X621`

eatenUSDAfoods$FNA_F183I<-eatenUSDAfoods$`X856`
eatenUSDAfoods$FNA_F182I<-eatenUSDAfoods$`X666`
eatenUSDAfoods$FNA_F18D2CN6<-eatenUSDAfoods$`X675`
eatenUSDAfoods$FNA_F18D2CLA<-eatenUSDAfoods$`X670`

######TRANS
eatenUSDAfoods$FNA_F16D1T<-eatenUSDAfoods$`X662`
eatenUSDAfoods$FNA_F18D1T<-eatenUSDAfoods$`X663`
eatenUSDAfoods$FNA_F18D1TN7<-eatenUSDAfoods$`X859`
eatenUSDAfoods$FNA_F18D2X<-eatenUSDAfoods$`X665`
eatenUSDAfoods$FNA_F18D2TT<-eatenUSDAfoods$`X669`
eatenUSDAfoods$FNA_F22D1T<-eatenUSDAfoods$`X664`
eatenUSDAfoods$FNA_FATRNM<-eatenUSDAfoods$`X693`
eatenUSDAfoods$FNA_FATRNP<-eatenUSDAfoods$`X695`

###EVERYTHING ELSE
eatenUSDAfoods$FNA_CARTA<-eatenUSDAfoods$`X322`
eatenUSDAfoods$FNA_CARTB<-eatenUSDAfoods$`X321`
eatenUSDAfoods$FNA_CRYPX<-eatenUSDAfoods$`X334`
eatenUSDAfoods$FNA_VITK1D<-eatenUSDAfoods$`X429`
eatenUSDAfoods$FNA_FOLDFE<-eatenUSDAfoods$`X435`
eatenUSDAfoods$FNA_FOLFD<-eatenUSDAfoods$`X432`
eatenUSDAfoods$FNA_MK4<-eatenUSDAfoods$`X428`
eatenUSDAfoods$FNA_FOL<-eatenUSDAfoods$`X417`
eatenUSDAfoods$FNA_VITD<-eatenUSDAfoods$`X328`
eatenUSDAfoods$FNA_TOCPHA<-eatenUSDAfoods$`X323`
eatenUSDAfoods$FNA_RETOL<-eatenUSDAfoods$`X319`
eatenUSDAfoods$FNA_TOCPHB<-eatenUSDAfoods$`X341`
eatenUSDAfoods$FNA_TOCPHD<-eatenUSDAfoods$`X343`
eatenUSDAfoods$FNA_TOCPHG<-eatenUSDAfoods$`X342`
eatenUSDAfoods$FNA_TOCTRA<-eatenUSDAfoods$`X344`
eatenUSDAfoods$FNA_TOCTRB<-eatenUSDAfoods$`X345`
eatenUSDAfoods$FNA_TOCTRD<-eatenUSDAfoods$`X347`
eatenUSDAfoods$FNA_TOCTRG<-eatenUSDAfoods$`X346`
eatenUSDAfoods$FNA_VITA_RAE<-eatenUSDAfoods$`X320`
eatenUSDAfoods$FNA_VITD<-eatenUSDAfoods$`X324`
eatenUSDAfoods$FNA_BETN<-eatenUSDAfoods$`X454`
eatenUSDAfoods$FNA_CAFFN<-eatenUSDAfoods$`X262`
eatenUSDAfoods$FNA_CA<-eatenUSDAfoods$`X301`
eatenUSDAfoods$FNA_CHOLE<-eatenUSDAfoods$`X601`
eatenUSDAfoods$FNA_CHOLN<-eatenUSDAfoods$`X421`
eatenUSDAfoods$FNA_CU<-eatenUSDAfoods$`X312`
eatenUSDAfoods$FNA_FLD<-eatenUSDAfoods$`X313`
eatenUSDAfoods$FNA_FOLAC<-eatenUSDAfoods$`X431`
eatenUSDAfoods$FNA_FE<-eatenUSDAfoods$`X303`
eatenUSDAfoods$FNA_LUTZEA<-eatenUSDAfoods$`X338`
eatenUSDAfoods$FNA_LYCPN<-eatenUSDAfoods$`X337`
eatenUSDAfoods$FNA_MG<-eatenUSDAfoods$`X304`
eatenUSDAfoods$FNA_MN<-eatenUSDAfoods$`X315`
eatenUSDAfoods$FNA_PROadj<-eatenUSDAfoods$`X257`
eatenUSDAfoods$FNA_ENERC_KJ<-eatenUSDAfoods$`X268`
eatenUSDAfoods$FNA_VITEadd<-eatenUSDAfoods$`X573`
eatenUSDAfoods$FNA_VITBadd<-eatenUSDAfoods$`X578`
eatenUSDAfoods$FNA_ERGCAL<-eatenUSDAfoods$`X325`
eatenUSDAfoods$FNA_CHOCAL<-eatenUSDAfoods$`X326`
eatenUSDAfoods$FNA_NIA<-eatenUSDAfoods$`X406`
eatenUSDAfoods$FNA_PANTAC<-eatenUSDAfoods$`X410`
eatenUSDAfoods$FNA_P<-eatenUSDAfoods$`X305`
eatenUSDAfoods$FNA_K<-eatenUSDAfoods$`X306`
eatenUSDAfoods$FNA_RIBF<-eatenUSDAfoods$`X405`
eatenUSDAfoods$FNA_SE<-eatenUSDAfoods$`X317`
eatenUSDAfoods$FNA_NA<-eatenUSDAfoods$`X307`
eatenUSDAfoods$FNA_THEBRN<-eatenUSDAfoods$`X263`
eatenUSDAfoods$FNA_THIA<-eatenUSDAfoods$`X404`
eatenUSDAfoods$FNA_VITA_IU<-eatenUSDAfoods$`X318`
eatenUSDAfoods$FNA_VITB12<-eatenUSDAfoods$`X418`
eatenUSDAfoods$FNA_VITB6A<-eatenUSDAfoods$`X415`
eatenUSDAfoods$FNA_VITC<-eatenUSDAfoods$`X401`
eatenUSDAfoods$FNA_VITK1<-eatenUSDAfoods$`X430`
eatenUSDAfoods$FNA_ZN<-eatenUSDAfoods$`X309`
eatenUSDAfoods$FNA_SITSTR<-eatenUSDAfoods$`X641`
eatenUSDAfoods$FNA_CAMD5<-eatenUSDAfoods$`X639`
eatenUSDAfoods$FNA_PHYSTR<-eatenUSDAfoods$`X636`
eatenUSDAfoods$FNA_STID7<-eatenUSDAfoods$`X638`
eatenUSDAfoods$FNA_ASH<-eatenUSDAfoods$`X207`
eatenUSDAfoods$FNA_WATER<-eatenUSDAfoods$`X255`