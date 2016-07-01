#####################
#####IMPORT FILES####
#####################
              
              #GET HISTORICAL NUTRITION FACTS DATABASE
              setwd("~/GitHub/foodomics/Foodomics Database Creation/Source Data")
              compiledNFD<- read.csv(file = "TRANSLATING_BNFD_TO_R2.txt", header = TRUE, sep="\t", na.strings=c("NA","NaN", " ", "N/A", "NULL"),stringsAsFactors=FALSE)
                                          
              #GET REFERENCE BASE PRODUCT PROFILE DATABASE
              setwd("~/GitHub/foodomics/Foodomics Database Creation/Output Data")
              referenceBASEP<-readRDS(file = "reference_BASEP.rds")

###############################################
#####ALTER DATA TYPE AND REMOVE SUPPLEMENTS####
###############################################
              
              #Convert numeric columns to type numeric in compiled historical nutrition facts database
              compiledNFD[,c(12:96, 114:241)] <- sapply(compiledNFD[,c(12:96, 114:241)], as.numeric)
              
              #Store supplements separately
              supplements<-subset(compiledNFD, compiledNFD$Category_1=="Supplement")
              
              #Remove supplements from the NFD
              compiledNFD<-subset(compiledNFD, !compiledNFD$Category_1=="Supplement")
              
              #Calculate a factor that we can use to convert all profiles to per 100 grams
              compiledNFD$NFL_Factor= 100/(as.numeric(compiledNFD$Weight_per_serving_g))
              
              #Store baby foods separately
              baby_foods<-subset(compiledNFD, compiledNFD$Category_1=="Baby_Foods")
              
              #Remove baby foods from the NFD
              compiledNFD<-subset(compiledNFD, !compiledNFD$Category_1=="Baby_Foods")
              
              #Translate daily values
              setwd("~/GitHub/foodomics/Foodomics Database Creation/")
              source("transDV.R")

              #Put baby foods back into the main database
              compiledNFD<-rbind(compiledNFD,baby_foods)
              
              #Convert any units that are not in grams to gram units
              source("convert_NFD_to_g.R")
              
#########################
#####MAYBE GET RID OF####
#########################
              
              compiledNFD[,c(12:32,61:96, 116:241)]=as.numeric(apply( compiledNFD[,c(12:32,61:96, 116:241)], 2, function(x) x *  compiledNFD$NFL_Factor))
              # NFL[NFL==""] <- NA
              #Now it's all per 100 grams
              names(compiledNFD)[names(compiledNFD)=="NDID"] <- "PRODUCTNDID"
            
##########################################################################
#####MERGE REFERENCE PROFILES WITH NUTRITION FACTS INFORMATION BY NDID####
##########################################################################
              
              ####GRAND MERGED UNCALCULATED FOODOMICS UNREPLICATED
              unCALC_unREPL_foodomicsDB<-merge(compiledNFD, referenceBASEP, by="PRODUCTNDID", all.x=TRUE)  #this is dropping things that don't have matches??????
              #rename for simplicity
              UCURdb<-unCALC_unREPL_foodomicsDB
    
###############################################
#####NFL ADJUSTED USDA PROFILE CALCULATIONS####
###############################################
              
              #Calculate Adjusted Carbohydate, remembering that every serving is now 100 grams
              UCURdb$CHObyCAL <- ((((UCURdb$CALORIES_PER_SERVING_KCAL/100)* 100) - (((UCURdb$TOTAL_FAT_PER_SERVING_G/100)*100)*9) - (((UCURdb$PROTEIN_PER_SERVING_G/100)*100)*4))/4)
              UCURdb$CHObyWEIGHT <- ifelse(UCURdb$CARBOHYDRATE_PER_SERVING_G=="0" | UCURdb$Weight_per_serving=="0", 0, ((UCURdb$CARBOHYDRATE_PER_SERVING_G/100)*100))
              UCURdb$CHOdec<- ifelse(UCURdb$CHObyCAL>UCURdb$CHObyWEIGHT,UCURdb$CHObyCAL,UCURdb$CHObyWEIGHT)
              UCURdb$CHOdec<- ifelse(UCURdb$CHOdec>100,100,UCURdb$CHOdec)
              
              #DEFINE FACTOR (VALUE ON NFL DIVIDED BY VALUE IN USDA) --- may not need
              UCURdb$Fat_conv<-ifelse(UCURdb$TOTAL_FAT_PER_SERVING_G=="0",0,ifelse(!is.na(UCURdb$TOTAL_FAT_PER_SERVING_G/UCURdb$`X204`),UCURdb$TOTAL_FAT_PER_SERVING_G/UCURdb$`X204`,1))  #this should be fat from label
              UCURdb$PRO_conv<-ifelse(UCURdb$PROTEIN_PER_SERVING_G=="0",0,ifelse(!is.na(UCURdb$PROTEIN_PER_SERVING_G/UCURdb$`X203`),UCURdb$PROTEIN_PER_SERVING_G/UCURdb$`X203`,1))   #this should be pro frmo label
              UCURdb$CHO_conv<-ifelse(UCURdb$CHOdec=="0",0,ifelse(!is.na(UCURdb$CHOdec/UCURdb$CHOdec2), UCURdb$CHOdec/UCURdb$CHOdec2,1)) #this should be adjusted carbohydate
              
              ###Macronutrient Rule: if NFL data are non-NA, use NFL data, else, use USDA data
              UCURdb$FNA_PROCNT<-ifelse(!is.na(UCURdb$PROTEIN_PER_SERVING_G),UCURdb$PROTEIN_PER_SERVING_G,UCURdb$`X203`)
              UCURdb$FNA_FAT<-ifelse(!is.na(UCURdb$TOTAL_FAT_PER_SERVING_G),UCURdb$TOTAL_FAT_PER_SERVING_G,UCURdb$`X204`)
              UCURdb$FNA_CHOCDF<-ifelse(!is.na(UCURdb$CHOdec),UCURdb$CHOdec,UCURdb$CHOdec2)
              UCURdb$FNA_ENERC_KCAL<-ifelse(!is.na(UCURdb$CALORIES_PER_SERVING_KCAL),UCURdb$CALORIES_PER_SERVING_KCAL,UCURdb$`X208`)
              is.na(UCURdb) <- do.call(cbind,lapply(UCURdb, is.infinite))
              ###Macronutrient Sub-Class Rule: if NFL data for sub-class are non-NA, use NFL sub-class data, else, use NFL macronutrient data factor for sub-class
              
              ################
              #CHO CONVERSIONS
              ################
              
              UCURdb$FNA_ALC<-ifelse(!is.na(UCURdb$Alcohol_per_serving_g),UCURdb$Alcohol_per_serving_g,UCURdb$`X221`)
              UCURdb$FNA_FIBTG<-ifelse(UCURdb$CHOdec=="0",0,ifelse(!is.na(UCURdb$DIETARY_FIBER_PER_SERVING_G),UCURdb$DIETARY_FIBER_PER_SERVING_G,UCURdb$CHO_conv*UCURdb$`X291`))
              UCURdb$FNA_FINSOL<-ifelse(UCURdb$CHOdec=="0",0,ifelse(!is.na(UCURdb$INSOLUBLE_FIBER_PER_SERVING_G),UCURdb$INSOLUBLE_FIBER_PER_SERVING_G,"NA"))
              UCURdb$FNA_FSOL<-ifelse(UCURdb$CHOdec=="0",0,ifelse(!is.na(UCURdb$SOLUBLE_FIBER__PER_SERVING_G),UCURdb$SOLUBLE_FIBER__PER_SERVING_G,"NA"))
              UCURdb$FNA_LACS<-ifelse(UCURdb$CHOdec=="0",0,ifelse(!is.na(UCURdb$LACTOSE_PER_SERVING_G),UCURdb$LACTOSE_PER_SERVING_G,UCURdb$CHO_conv*UCURdb$`X213`))
              UCURdb$FNA_MALS<-ifelse(UCURdb$CHOdec=="0",0,ifelse(!is.na(UCURdb$MALTOSE_PER_SERVING_G),UCURdb$MALTOSE_PER_SERVING_G,UCURdb$CHO_conv*UCURdb$`X214`))
              UCURdb$FNA_FRUS<-ifelse(UCURdb$CHOdec=="0",0,ifelse(!is.na(UCURdb$FRUCTOSE_PER_SERVING_G),UCURdb$FRUCTOSE_PER_SERVING_G,UCURdb$CHO_conv*UCURdb$`X212`))
              UCURdb$FNA_GALS<-ifelse(UCURdb$CHOdec=="0",0,ifelse(!is.na(UCURdb$GaLACTOSE_PER_SERVING_G),UCURdb$GaLACTOSE_PER_SERVING_G,UCURdb$CHO_conv*UCURdb$`X287`))
              UCURdb$FNA_GLUS<-ifelse(UCURdb$CHOdec=="0",0,ifelse(!is.na(UCURdb$GLUCOSE_PER_SERVING_G),UCURdb$GLUCOSE_PER_SERVING_G,UCURdb$CHO_conv*UCURdb$`X211`))
              UCURdb$FNA_STARCH<-ifelse(UCURdb$CHOdec=="0",0,ifelse(!is.na(UCURdb$STARCH_PER_SERVING_G),UCURdb$STARCH_PER_SERVING_G,UCURdb$CHO_conv*UCURdb$`X209`))
              UCURdb$FNA_SUCS<-ifelse(UCURdb$CHOdec=="0",0,ifelse(!is.na(UCURdb$SUCROSE_PER_SERVING_G),UCURdb$SUCROSE_PER_SERVING_G,UCURdb$CHO_conv*UCURdb$`X210`))
              UCURdb$FNA_SUGAR<-ifelse(UCURdb$CHOdec=="0",0,ifelse(!is.na(UCURdb$SUGAR_PER_SERVING_G),UCURdb$SUGAR_PER_SERVING_G,UCURdb$CHO_conv*UCURdb$`X269`))
                      
              ################
              #PRO CONVERSIONS
              ################
              
              UCURdb$FNA_ALA_G<-ifelse(UCURdb$PROTEIN_PER_SERVING_G=="0",0,ifelse(!is.na(UCURdb$ALANINE_PER_SERVING_G),UCURdb$ALANINE_PER_SERVING_G,UCURdb$PRO_conv*UCURdb$`X513`))
              UCURdb$FNA_ARG_G<-ifelse(UCURdb$PROTEIN_PER_SERVING_G=="0",0,ifelse(!is.na(UCURdb$ARGININE_PER_SERVING_G),UCURdb$ARGININE_PER_SERVING_G,UCURdb$PRO_conv*UCURdb$`X511`))
              UCURdb$FNA_ASP_G<-ifelse(UCURdb$PROTEIN_PER_SERVING_G=="0",0,ifelse(!is.na(UCURdb$ASPARTIC_ACID_PER_SERVING_G),UCURdb$ASPARTIC_ACID_PER_SERVING_G,UCURdb$PRO_conv*UCURdb$`X514`))
              UCURdb$FNA_CYS_G<-ifelse(UCURdb$PROTEIN_PER_SERVING_G=="0",0,ifelse(!is.na(UCURdb$CYSTINE_PER_SERVING_G),UCURdb$CYSTINE_PER_SERVING_G,UCURdb$PRO_conv*UCURdb$`X507`))
              UCURdb$FNA_GLU_G<-ifelse(UCURdb$PROTEIN_PER_SERVING_G=="0",0,ifelse(!is.na(UCURdb$GLUTAMIC_ACID_PER_SERVING_G),UCURdb$GLUTAMIC_ACID_PER_SERVING_G,UCURdb$PRO_conv*UCURdb$`X515`))
              UCURdb$FNA_GLY_G<-ifelse(UCURdb$PROTEIN_PER_SERVING_G=="0",0,ifelse(!is.na(UCURdb$GLYCINE_PER_SERVING_G),UCURdb$GLYCINE_PER_SERVING_G,UCURdb$PRO_conv*UCURdb$`X516`))
              UCURdb$FNA_HISTN_G<-ifelse(UCURdb$PROTEIN_PER_SERVING_G=="0",0,ifelse(!is.na(UCURdb$HISTIDINE_PER_SERVING_G),UCURdb$HISTIDINE_PER_SERVING_G,UCURdb$PRO_conv*UCURdb$`X512`))
              UCURdb$FNA_HYP<-ifelse(UCURdb$PROTEIN_PER_SERVING_G=="0",0,ifelse(!is.na(UCURdb$HYDROXYPROLINE_PER_SERVING_G),UCURdb$HYDROXYPROLINE_PER_SERVING_G,UCURdb$PRO_conv*UCURdb$`X521`))
              UCURdb$FNA_ILE_G<-ifelse(UCURdb$PROTEIN_PER_SERVING_G=="0",0,ifelse(!is.na(UCURdb$ISOLEUCINE_PER_SERVING_G),UCURdb$ISOLEUCINE_PER_SERVING_G,UCURdb$PRO_conv*UCURdb$`X503`))
              UCURdb$FNA_LEU_G<-ifelse(UCURdb$PROTEIN_PER_SERVING_G=="0",0,ifelse(!is.na(UCURdb$LEUCINE_PER_SERVING_G),UCURdb$LEUCINE_PER_SERVING_G,UCURdb$PRO_conv*UCURdb$`X504`))
              UCURdb$FNA_LYS_G<-ifelse(UCURdb$PROTEIN_PER_SERVING_G=="0",0,ifelse(!is.na(UCURdb$LYSINE_PER_SERVING_G),UCURdb$LYSINE_PER_SERVING_G,UCURdb$PRO_conv*UCURdb$`X505`))
              UCURdb$FNA_MET_G<-ifelse(UCURdb$PROTEIN_PER_SERVING_G=="0",0,ifelse(!is.na(UCURdb$METHIONINE_PER_SERVING_G),UCURdb$METHIONINE_PER_SERVING_G,UCURdb$PRO_conv*UCURdb$`X506`))
              UCURdb$FNA_PHE_G<-ifelse(UCURdb$PROTEIN_PER_SERVING_G=="0",0,ifelse(!is.na(UCURdb$PHENYLALANINE_PER_SERVING_G),UCURdb$PHENYLALANINE_PER_SERVING_G,UCURdb$PRO_conv*UCURdb$`X508`))
              UCURdb$FNA_PRO_G<-ifelse(UCURdb$PROTEIN_PER_SERVING_G=="0",0,ifelse(!is.na(UCURdb$PROLINE_PER_SERVING_G),UCURdb$PROLINE_PER_SERVING_G,UCURdb$PRO_conv*UCURdb$`X517`))
              UCURdb$FNA_SER_G<-ifelse(UCURdb$PROTEIN_PER_SERVING_G=="0",0,ifelse(!is.na(UCURdb$SERINE_PER_SERVING_G),UCURdb$SERINE_PER_SERVING_G,UCURdb$PRO_conv*UCURdb$`X518`))
              UCURdb$FNA_THR_G<-ifelse(UCURdb$PROTEIN_PER_SERVING_G=="0",0,ifelse(!is.na(UCURdb$THREONINE_PER_SERVING_G),UCURdb$THREONINE_PER_SERVING_G,UCURdb$PRO_conv*UCURdb$`X502`))
              UCURdb$FNA_TRP_G<-ifelse(UCURdb$PROTEIN_PER_SERVING_G=="0",0,ifelse(!is.na(UCURdb$TRYPTOPHAN_PER_SERVING_G),UCURdb$TRYPTOPHAN_PER_SERVING_G,UCURdb$PRO_conv*UCURdb$`X501`))
              UCURdb$FNA_TYR_G<-ifelse(UCURdb$PROTEIN_PER_SERVING_G=="0",0,ifelse(!is.na(UCURdb$TYROSINE_PER_SERVING_G),UCURdb$TYROSINE_PER_SERVING_G,UCURdb$PRO_conv*UCURdb$`X509`))
              UCURdb$FNA_VAL_G<-ifelse(UCURdb$PROTEIN_PER_SERVING_G=="0",0,ifelse(!is.na(UCURdb$VALINE_PER_SERVING_G),UCURdb$VALINE_PER_SERVING_G,UCURdb$PRO_conv*UCURdb$`X510`))
            
              
              ################
              #FAT CONVERSIONS
              ################
              source("foodomics_recalc.r")
              
              #CHAIN LENGTH
              UCURdb$FNA_FAT_SHORT<-apply(cbind(UCURdb$FNA_F4D0,UCURdb$FNA_F6D0),1,sum,na.rm = TRUE)
              UCURdb$FNA_FAT_MEDIUM<-apply(cbind(UCURdb$FNA_F8D0,UCURdb$FNA_F10D0,UCURdb$FNA_F12D0,UCURdb$FNA_F13D0),1,sum,na.rm = TRUE)
              UCURdb$FNA_FAT_LONG<-apply(cbind(UCURdb$FNA_F14D0,
                                               UCURdb$FNA_F14D1,
                                               UCURdb$FNA_F15D0,
                                               UCURdb$FNA_F15D1,
                                               UCURdb$FNA_F16D0,
                                               UCURdb$FNA_F17D0,
                                               UCURdb$FNA_F17D1,
                                               UCURdb$FNA_F18D0,
                                               UCURdb$FNA_F18D4,
                                               UCURdb$FNA_F18D1TN7,
                                               UCURdb$FNA_F16D1C,
                                               UCURdb$FNA_F16D1T,
                                               UCURdb$FNA_F18D1C,
                                               UCURdb$FNA_F18D1T,
                                               UCURdb$FNA_F18D2CLA,
                                               UCURdb$FNA_F182I,
                                               UCURdb$FNA_F18D2CN6,
                                               UCURdb$FNA_F18D2X,
                                               UCURdb$FNA_F18D2TT,
                                               UCURdb$FNA_F18D3CN3,
                                               UCURdb$FNA_F18D3CN6,
                                               UCURdb$FNA_F183I,
                                               UCURdb$FNA_F16D1_OTHER,
                                               UCURdb$FNA_F18D1_OTHER,
                                               UCURdb$FNA_F18D2_OTHER,
                                               UCURdb$FNA_F18D3_OTHER),1,sum,na.rm = TRUE)
              
              UCURdb$FNA_FAT_VERY_LONG<-apply(cbind(UCURdb$FNA_F20D0,
                                                    UCURdb$FNA_F20D1,
                                                    UCURdb$FNA_F21D5,
                                                    UCURdb$FNA_F22D0,
                                                    UCURdb$FNA_F22D4,
                                                    UCURdb$FNA_F24D0,
                                                    UCURdb$FNA_F20D2CN6,
                                                    UCURdb$FNA_F20D5,
                                                    UCURdb$FNA_F22D5,
                                                    UCURdb$FNA_F22D6,
                                                    UCURdb$FNA_F24D1C,
                                                    UCURdb$FNA_F20D3N3,
                                                    UCURdb$FNA_F20D3N6,
                                                    UCURdb$FNA_F20D4N6,
                                                    UCURdb$FNA_F22D1C,
                                                    UCURdb$FNA_F22D1T,
                                                    UCURdb$FNA_F20D3_OTHER,
                                                    UCURdb$FNA_F20D4_OTHER,
                                                    UCURdb$FNA_F22D1_OTHER),1,sum,na.rm = TRUE)
              
                      
              
              
             ###EVERYTHING ELSE
             UCURdb$FNA_CARTA<-ifelse(!is.na(UCURdb$CAROTENE_ALPHA_PER_SERVING_MCG),UCURdb$CAROTENE_ALPHA_PER_SERVING_MCG,UCURdb$`X322`)
             UCURdb$FNA_CARTB<-ifelse(!is.na(UCURdb$CAROTENE_BETA_PER_SERVING_MCG),UCURdb$CAROTENE_BETA_PER_SERVING_MCG,UCURdb$`X321`)
             UCURdb$FNA_CRYPX<-ifelse(!is.na(UCURdb$CRYPTOXANTHIN_BETA_PER_SERVING_MCG),UCURdb$CRYPTOXANTHIN_BETA_PER_SERVING_MCG,UCURdb$`X334`)
             UCURdb$FNA_VITK1D<-ifelse(!is.na(UCURdb$DIHYDROPHYLLOQUINONE_PER_SERVING_MCG),UCURdb$DIHYDROPHYLLOQUINONE_PER_SERVING_MCG,UCURdb$`X429`)
             UCURdb$FNA_FOLDFE<-ifelse(!is.na(UCURdb$Folate_DFE_per_serving_mcg),UCURdb$Folate_DFE_per_serving_mcg,UCURdb$`X435`)
             UCURdb$FNA_FOLFD<-ifelse(!is.na(UCURdb$Folate_food_per_serving_mcg),UCURdb$Folate_food_per_serving_mcg,UCURdb$`X432`)
             UCURdb$FNA_MK4<-ifelse(!is.na(UCURdb$MENAQUINONE_4_PER_SERVING_MCG),UCURdb$MENAQUINONE_4_PER_SERVING_MCG,UCURdb$`X428`)
             UCURdb$FNA_FOL<-ifelse(!is.na(UCURdb$Folate_mcg),UCURdb$Folate_mcg,UCURdb$`X417`)
             UCURdb$FNA_VITD<-ifelse(!is.na(UCURdb$VITAMIN_D_MCG_PER_SERVING),UCURdb$VITAMIN_D_MCG_PER_SERVING,UCURdb$`X328`)
             UCURdb$FNA_TOCPHA<-ifelse(!is.na(UCURdb$VITAMIN_E_MG_PER_SERVING),UCURdb$VITAMIN_E_MG_PER_SERVING,UCURdb$`X323`)
             UCURdb$FNA_RETOL<-ifelse(!is.na(UCURdb$RETINOL_PER_SERVING_MCG),UCURdb$RETINOL_PER_SERVING_MCG,UCURdb$`X319`)
             UCURdb$FNA_TOCPHB<-ifelse(!is.na(UCURdb$TOCOPHEROL_BETA_PER_SERVING_MG),UCURdb$TOCOPHEROL_BETA_PER_SERVING_MG,UCURdb$`X341`)
             UCURdb$FNA_TOCPHD<-ifelse(!is.na(UCURdb$TOCOPHEROL_DELTA_PER_SERVING_MG),UCURdb$TOCOPHEROL_DELTA_PER_SERVING_MG,UCURdb$`X343`)
             UCURdb$FNA_TOCPHG<-ifelse(!is.na(UCURdb$TOCOPHEROL_GAMMA_PER_SERVING_MG),UCURdb$TOCOPHEROL_GAMMA_PER_SERVING_MG,UCURdb$`X342`)
             UCURdb$FNA_TOCTRA<-ifelse(!is.na(UCURdb$TOCOTRIENOL_ALPHA_PER_SERVING_MG),UCURdb$TOCOTRIENOL_ALPHA_PER_SERVING_MG,UCURdb$`X344`)
             UCURdb$FNA_TOCTRB<-ifelse(!is.na(UCURdb$TOCOTRIENOL_BETA_PER_SERVING_MG),UCURdb$TOCOTRIENOL_BETA_PER_SERVING_MG,UCURdb$`X345`)
             UCURdb$FNA_TOCTRD<-ifelse(!is.na(UCURdb$TOCOTRIENOL_DELTA_PER_SERVING_MG),UCURdb$TOCOTRIENOL_DELTA_PER_SERVING_MG,UCURdb$`X347`)
             UCURdb$FNA_TOCTRG<-ifelse(!is.na(UCURdb$TOCOTRIENOL_GAMMA_PER_SERVING_MG),UCURdb$TOCOTRIENOL_GAMMA_PER_SERVING_MG,UCURdb$`X346`)
             UCURdb$FNA_VITA_RAE<-ifelse(!is.na(UCURdb$VITAMIN_A_RAE_PER_SERVING_MCG),UCURdb$VITAMIN_A_RAE_PER_SERVING_MCG,UCURdb$`X320`)
             UCURdb$FNA_VITD<-ifelse(!is.na(UCURdb$VITAMIN_D_PER_SERVING_IU),UCURdb$VITAMIN_D_PER_SERVING_IU,UCURdb$`X324`)
             UCURdb$FNA_BETN<-ifelse(!is.na(UCURdb$BETAINE_PER_SERVING_MG),UCURdb$BETAINE_PER_SERVING_MG,UCURdb$`X454`)
             UCURdb$FNA_BIO<-ifelse(!is.na(UCURdb$BIOTIN_MCG),UCURdb$BIOTIN_MCG,"NA")
             UCURdb$FNA_BOR<-ifelse(!is.na(UCURdb$BORON_MG),UCURdb$BORON_MG,"NA")
             UCURdb$FNA_CAFFN<-ifelse(!is.na(UCURdb$CAFFEINE_PER_SERVING_MG),UCURdb$CAFFEINE_PER_SERVING_MG,UCURdb$`X262`)
             UCURdb$FNA_CA<-ifelse(!is.na(UCURdb$CALCIUM_MG),UCURdb$CALCIUM_MG,UCURdb$`X301`)
             UCURdb$FNA_CHL<-ifelse(!is.na(UCURdb$CHLORIDE_MG),UCURdb$CHLORIDE_MG,"NA")
             UCURdb$FNA_CHOLE<-ifelse(!is.na(UCURdb$CHOLESTEROL_PER_SERVING_MG),UCURdb$CHOLESTEROL_PER_SERVING_MG,UCURdb$`X601`)
             UCURdb$FNA_CHOLN<-ifelse(!is.na(UCURdb$CHOLINE_MG),UCURdb$CHOLINE_MG,UCURdb$`X421`)
             UCURdb$FNA_CHR<-ifelse(!is.na(UCURdb$CHROMIUM_MCG),UCURdb$CHROMIUM_MCG,"NA")
             UCURdb$FNA_CU<-ifelse(!is.na(UCURdb$COPPER_MCG),UCURdb$COPPER_MCG,UCURdb$`X312`)
             UCURdb$FNA_FLD<-ifelse(!is.na(UCURdb$FLUORIDE_MG),UCURdb$FLUORIDE_MG,UCURdb$`X313`)
             UCURdb$FNA_FOLAC<-ifelse(!is.na(UCURdb$FOLIC_ACID_PER_SERVING_MCG),UCURdb$FOLIC_ACID_PER_SERVING_MCG,UCURdb$`X431`)
             UCURdb$FNA_IOD<-ifelse(!is.na(UCURdb$IODINE_MCG),UCURdb$IODINE_MCG,"NA")
             UCURdb$FNA_FE<-ifelse(!is.na(UCURdb$IRON_MG),UCURdb$IRON_MG,UCURdb$`X303`)
             UCURdb$FNA_LCRN<-ifelse(!is.na(UCURdb$L_CARNITINE_MG),UCURdb$L_CARNITINE_MG,"NA")
             UCURdb$FNA_LUTZEA<-ifelse(!is.na(UCURdb$LUTEIN_ZEAXANTHIN_PER_SERVING_MCG),UCURdb$LUTEIN_ZEAXANTHIN_PER_SERVING_MCG,UCURdb$`X338`)
             UCURdb$FNA_LYCPN<-ifelse(!is.na(UCURdb$LYCOPENE_PER_SERVING_MCG),UCURdb$LYCOPENE_PER_SERVING_MCG,UCURdb$`X337`)
             UCURdb$FNA_MG<-ifelse(!is.na(UCURdb$Magnesium_mg),UCURdb$Magnesium_mg,UCURdb$`X304`)
             UCURdb$FNA_MN<-ifelse(!is.na(UCURdb$Manganese_mg),UCURdb$Manganese_mg,UCURdb$`X315`)
             UCURdb$FNA_MLY<-ifelse(!is.na(UCURdb$Molybdenum_mcg),UCURdb$Molybdenum_mcg,"NA")
             UCURdb$FNA_PROadj<-ifelse(!is.na(UCURdb$Adjusted_PROTEIN_PER_SERVING_G),UCURdb$Adjusted_PROTEIN_PER_SERVING_G,UCURdb$`X257`)
             UCURdb$FNA_ENERC_KJ<-ifelse(!is.na(UCURdb$Energy_per_serving_kJ),UCURdb$Energy_per_serving_kJ,UCURdb$`X268`)
             UCURdb$FNA_VITEadd<-ifelse(!is.na(UCURdb$Vitamin_E_added_per_serving_mg),UCURdb$Vitamin_E_added_per_serving_mg,UCURdb$`X573`)
             UCURdb$FNA_VITBadd<-ifelse(!is.na(UCURdb$Vitamin_B_12_added_per_serving_mcg),UCURdb$Vitamin_B_12_added_per_serving_mcg,UCURdb$`X578`)
             UCURdb$FNA_ERGCAL<-ifelse(!is.na(UCURdb$Vitamin_D2_ergocalciferol_per_serving_mcg),UCURdb$Vitamin_D2_ergocalciferol_per_serving_mcg,UCURdb$`X325`)
             UCURdb$FNA_CHOCAL<-ifelse(!is.na(UCURdb$Vitamin_D3_cholecalciferol_per_serving_mcg),UCURdb$Vitamin_D3_cholecalciferol_per_serving_mcg,UCURdb$`X326`)
             UCURdb$FNA_NAC<-ifelse(!is.na(UCURdb$N_acetyL_CARNITINE_MG),UCURdb$N_acetyL_CARNITINE_MG,"NA")
             UCURdb$FNA_NIA<-ifelse(!is.na(UCURdb$Niacin_mg),UCURdb$Niacin_mg,UCURdb$`X406`)
             UCURdb$FNA_NCK<-ifelse(!is.na(UCURdb$Nickel_mg),UCURdb$Nickel_mg,"NA")
             UCURdb$FNA_PANTAC<-ifelse(!is.na(UCURdb$Pantothenic_mg),UCURdb$Pantothenic_mg,UCURdb$`X410`)
             UCURdb$FNA_P<-ifelse(!is.na(UCURdb$Phosphorus_mg),UCURdb$Phosphorus_mg,UCURdb$`X305`)
             UCURdb$FNA_K<-ifelse(!is.na(UCURdb$Potassium_mg),UCURdb$Potassium_mg,UCURdb$`X306`)
             UCURdb$FNA_RIBF<-ifelse(!is.na(UCURdb$Riboflavin_mg),UCURdb$Riboflavin_mg,UCURdb$`X405`)
             UCURdb$FNA_SE<-ifelse(!is.na(UCURdb$Selenium_mcg),UCURdb$Selenium_mcg,UCURdb$`X317`)
             UCURdb$FNA_SIL<-ifelse(!is.na(UCURdb$Silicon_mcg),UCURdb$Silicon_mcg,"NA")
             UCURdb$FNA_NA<-ifelse(!is.na(UCURdb$Sodium_mg),UCURdb$Sodium_mg,UCURdb$`X307`)
             UCURdb$FNA_THEBRN<-ifelse(!is.na(UCURdb$Theobromine_per_serving_mg),UCURdb$Theobromine_per_serving_mg,UCURdb$`X263`)
             UCURdb$FNA_THIA<-ifelse(!is.na(UCURdb$Thiamin_mg),UCURdb$Thiamin_mg,UCURdb$`X404`)
             UCURdb$FNA_TIN<-ifelse(!is.na(UCURdb$Tin_mg),UCURdb$Tin_mg,"NA")
             UCURdb$FNA_VAN<-ifelse(!is.na(UCURdb$Vanadium_mg),UCURdb$Vanadium_mg,"NA")
             UCURdb$FNA_VITA_IU<-ifelse(!is.na(UCURdb$Vitamin_A_mcg),UCURdb$Vitamin_A_mcg,UCURdb$`X318`)
             UCURdb$FNA_VITB12<-ifelse(!is.na(UCURdb$Vitamin_B12_mcg),UCURdb$Vitamin_B12_mcg,UCURdb$`X418`)
             UCURdb$FNA_VITB6A<-ifelse(!is.na(UCURdb$Vitamin_B6_mg),UCURdb$Vitamin_B6_mg,UCURdb$`X415`)
             UCURdb$FNA_VITC<-ifelse(!is.na(UCURdb$Vitamin_C_mg),UCURdb$Vitamin_C_mg,UCURdb$`X401`)
             UCURdb$FNA_VITK1<-ifelse(!is.na(UCURdb$Vitamin_K_mcg),UCURdb$Vitamin_K_mcg,UCURdb$`X430`)
             UCURdb$FNA_ZN<-ifelse(!is.na(UCURdb$Zinc_mg),UCURdb$Zinc_mg,UCURdb$`X309`)
             UCURdb$FNA_SITSTR<-ifelse(!is.na(UCURdb$Beta_sitosterol_per_serving_mg),UCURdb$Beta_sitosterol_per_serving_mg,UCURdb$`X641`)
             UCURdb$FNA_CAMD5<-ifelse(!is.na(UCURdb$Campesterol_per_serving_mg),UCURdb$Campesterol_per_serving_mg,UCURdb$`X639`)
             UCURdb$FNA_PHYSTR<-ifelse(!is.na(UCURdb$Phytosterols_per_serving_mg),UCURdb$Phytosterols_per_serving_mg,UCURdb$`X636`)
             UCURdb$FNA_STID7<-ifelse(!is.na(UCURdb$Stigmasterol_per_serving_mg),UCURdb$Stigmasterol_per_serving_mg,UCURdb$`X638`)
             UCURdb$FNA_ASH<-ifelse(!is.na(UCURdb$Ash_per_serving_g),UCURdb$Ash_per_serving_g,UCURdb$`X207`)
             UCURdb$FNA_WATER<-ifelse(!is.na(UCURdb$Water_per_serving_g),UCURdb$Water_per_serving_g,UCURdb$`X255`)

             #convert supplements
             setwd("~/GitHub/foodomics/Foodomics Database Creation/")
             source("getSUPPLEMENTS.R")
             write.csv(UCURdb, file="UCURdb.csv")

              foodomics<-UCURdb[ , c(1:8,97:100, 400:562) ]
              
              #Put supplements back into the main database
              library(gtools)
              supplements$PRODUCTNDID<-supplements$NDID
              UCURdb2<-smartbind(foodomics,supplements)
              
              setwd("~/GitHub/foodomics/Foodomics Database Creation/")
              source("getEatenUSDA.R")
              UCURdb3<-smartbind(UCURdb2,eatenUSDAfoods)
              foodomics<-as.data.frame(UCURdb3)
            
#####################
#####SAVE RESULTS####
#####################
            
            setwd("~/GitHub/foodomics/Foodomics Database Creation/Output Data")
            time<-format(Sys.time(), "%b_%d_%Y_%H_%M_%S")
            
            name<-paste("foodomics_DB", time, sep="_")
            write.csv(foodomics, file=paste(name, "csv", sep="."))
            saveRDS(foodomics, file=paste(name, "rds", sep="."))
            foodomics<-readRDS(file=paste(name, "rds", sep="."))
            
            #save whole database with all unneeded columns
            setwd("~/GitHub/foodomics/Foodomics Database Creation/Output Data")
            name2<-paste("whole_foodomics_DB", time, sep="_")
            write.csv(UCURdb3, file=paste(name2, "csv", sep="."))
     
