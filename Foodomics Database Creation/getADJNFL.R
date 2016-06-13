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
              UCURdb$CHObyCAL <- ((((UCURdb$Calories_per_serving_kcal/100)* 100) - (((UCURdb$Total_Fat_per_serving_g/100)*100)*9) - (((UCURdb$Protein_per_serving_g/100)*100)*4))/4)
              UCURdb$CHObyWEIGHT <- ifelse(UCURdb$Carbohydrate_per_serving_g=="0" | UCURdb$Weight_per_serving=="0", 0, ((UCURdb$Carbohydrate_per_serving_g/100)*100))
              UCURdb$CHOdec<- ifelse(UCURdb$CHObyCAL>UCURdb$CHObyWEIGHT,UCURdb$CHObyCAL,UCURdb$CHObyWEIGHT)
              UCURdb$CHOdec<- ifelse(UCURdb$CHOdec>100,100,UCURdb$CHOdec)
              
              #DEFINE FACTOR (VALUE ON NFL DIVIDED BY VALUE IN USDA)
              UCURdb$Fat_conv<-ifelse(UCURdb$Total_Fat_per_serving_g=="0",0,ifelse(!is.na(UCURdb$Total_Fat_per_serving_g/UCURdb$`X204`),UCURdb$Total_Fat_per_serving_g/UCURdb$`X204`,1))  #this should be fat from label
              UCURdb$PRO_conv<-ifelse(UCURdb$Protein_per_serving_g=="0",0,ifelse(!is.na(UCURdb$Protein_per_serving_g/UCURdb$`X203`),UCURdb$Protein_per_serving_g/UCURdb$`X203`,1))   #this should be pro frmo label
              UCURdb$CHO_conv<-ifelse(UCURdb$CHOdec=="0",0,ifelse(!is.na(UCURdb$CHOdec/UCURdb$CHOdec2), UCURdb$CHOdec/UCURdb$CHOdec2,1)) #this should be adjusted carbohydate
              
              ###Macronutrient Rule: if NFL data are non-NA, use NFL data, else, use USDA data
              UCURdb$FNA_PROCNT<-ifelse(!is.na(UCURdb$Protein_per_serving_g),UCURdb$Protein_per_serving_g,UCURdb$`X203`)
              UCURdb$FNA_FAT<-ifelse(!is.na(UCURdb$Total_Fat_per_serving_g),UCURdb$Total_Fat_per_serving_g,UCURdb$`X204`)
              UCURdb$FNA_CHOCDF<-ifelse(!is.na(UCURdb$CHOdec),UCURdb$CHOdec,UCURdb$CHOdec2)
              UCURdb$FNA_ENERC_KCAL<-ifelse(!is.na(UCURdb$Calories_per_serving_kcal),UCURdb$Calories_per_serving_kcal,UCURdb$`X208`)
              is.na(UCURdb) <- do.call(cbind,lapply(UCURdb, is.infinite))
              ###Macronutrient Sub-Class Rule: if NFL data for sub-class are non-NA, use NFL sub-class data, else, use NFL macronutrient data factor for sub-class
              
              ################
              #CHO CONVERSIONS
              ################
              
              UCURdb$FNA_ALC<-ifelse(!is.na(UCURdb$Alcohol_per_serving_g),UCURdb$Alcohol_per_serving_g,UCURdb$`X221`)
              UCURdb$FNA_FIBTG<-ifelse(UCURdb$CHOdec=="0",0,ifelse(!is.na(UCURdb$Dietary_Fiber_per_serving_g),UCURdb$Dietary_Fiber_per_serving_g,UCURdb$CHO_conv*UCURdb$`X291`))
              UCURdb$FNA_FINSOL<-ifelse(UCURdb$CHOdec=="0",0,ifelse(!is.na(UCURdb$Insoluble_Fiber_per_serving_g),UCURdb$Insoluble_Fiber_per_serving_g,"NA"))
              UCURdb$FNA_FSOL<-ifelse(UCURdb$CHOdec=="0",0,ifelse(!is.na(UCURdb$Soluble_Fiber__per_serving_g),UCURdb$Soluble_Fiber__per_serving_g,"NA"))
              UCURdb$FNA_LACS<-ifelse(UCURdb$CHOdec=="0",0,ifelse(!is.na(UCURdb$Lactose_per_serving_g),UCURdb$Lactose_per_serving_g,UCURdb$CHO_conv*UCURdb$`X213`))
              UCURdb$FNA_MALS<-ifelse(UCURdb$CHOdec=="0",0,ifelse(!is.na(UCURdb$Maltose_per_serving_g),UCURdb$Maltose_per_serving_g,UCURdb$CHO_conv*UCURdb$`X214`))
              UCURdb$FNA_FRUS<-ifelse(UCURdb$CHOdec=="0",0,ifelse(!is.na(UCURdb$Fructose_per_serving_g),UCURdb$Fructose_per_serving_g,UCURdb$CHO_conv*UCURdb$`X212`))
              UCURdb$FNA_GALS<-ifelse(UCURdb$CHOdec=="0",0,ifelse(!is.na(UCURdb$Galactose_per_serving_g),UCURdb$Galactose_per_serving_g,UCURdb$CHO_conv*UCURdb$`X287`))
              UCURdb$FNA_GLUS<-ifelse(UCURdb$CHOdec=="0",0,ifelse(!is.na(UCURdb$Glucose_per_serving_g),UCURdb$Glucose_per_serving_g,UCURdb$CHO_conv*UCURdb$`X211`))
              UCURdb$FNA_STARCH<-ifelse(UCURdb$CHOdec=="0",0,ifelse(!is.na(UCURdb$Starch_per_serving_g),UCURdb$Starch_per_serving_g,UCURdb$CHO_conv*UCURdb$`X209`))
              UCURdb$FNA_SUCS<-ifelse(UCURdb$CHOdec=="0",0,ifelse(!is.na(UCURdb$Sucrose_per_serving_g),UCURdb$Sucrose_per_serving_g,UCURdb$CHO_conv*UCURdb$`X210`))
              UCURdb$FNA_SUGAR<-ifelse(UCURdb$CHOdec=="0",0,ifelse(!is.na(UCURdb$Sugar_per_serving_g),UCURdb$Sugar_per_serving_g,UCURdb$CHO_conv*UCURdb$`X269`))
                      
              ################
              #PRO CONVERSIONS
              ################
              
              UCURdb$FNA_ALA_G<-ifelse(UCURdb$Protein_per_serving_g=="0",0,ifelse(!is.na(UCURdb$Alanine_per_serving_g),UCURdb$Alanine_per_serving_g,UCURdb$PRO_conv*UCURdb$`X513`))
              UCURdb$FNA_ARG_G<-ifelse(UCURdb$Protein_per_serving_g=="0",0,ifelse(!is.na(UCURdb$Arginine_per_serving_g),UCURdb$Arginine_per_serving_g,UCURdb$PRO_conv*UCURdb$`X511`))
              UCURdb$FNA_ASP_G<-ifelse(UCURdb$Protein_per_serving_g=="0",0,ifelse(!is.na(UCURdb$Aspartic_acid_per_serving_g),UCURdb$Aspartic_acid_per_serving_g,UCURdb$PRO_conv*UCURdb$`X514`))
              UCURdb$FNA_CYS_G<-ifelse(UCURdb$Protein_per_serving_g=="0",0,ifelse(!is.na(UCURdb$Cystine_per_serving_g),UCURdb$Cystine_per_serving_g,UCURdb$PRO_conv*UCURdb$`X507`))
              UCURdb$FNA_GLU_G<-ifelse(UCURdb$Protein_per_serving_g=="0",0,ifelse(!is.na(UCURdb$Glutamic_acid_per_serving_g),UCURdb$Glutamic_acid_per_serving_g,UCURdb$PRO_conv*UCURdb$`X515`))
              UCURdb$FNA_GLY_G<-ifelse(UCURdb$Protein_per_serving_g=="0",0,ifelse(!is.na(UCURdb$Glycine_per_serving_g),UCURdb$Glycine_per_serving_g,UCURdb$PRO_conv*UCURdb$`X516`))
              UCURdb$FNA_HISTN_G<-ifelse(UCURdb$Protein_per_serving_g=="0",0,ifelse(!is.na(UCURdb$Histidine_per_serving_g),UCURdb$Histidine_per_serving_g,UCURdb$PRO_conv*UCURdb$`X512`))
              UCURdb$FNA_HYP<-ifelse(UCURdb$Protein_per_serving_g=="0",0,ifelse(!is.na(UCURdb$Hydroxyproline_per_serving_g),UCURdb$Hydroxyproline_per_serving_g,UCURdb$PRO_conv*UCURdb$`X521`))
              UCURdb$FNA_ILE_G<-ifelse(UCURdb$Protein_per_serving_g=="0",0,ifelse(!is.na(UCURdb$Isoleucine_per_serving_g),UCURdb$Isoleucine_per_serving_g,UCURdb$PRO_conv*UCURdb$`X503`))
              UCURdb$FNA_LEU_G<-ifelse(UCURdb$Protein_per_serving_g=="0",0,ifelse(!is.na(UCURdb$Leucine_per_serving_g),UCURdb$Leucine_per_serving_g,UCURdb$PRO_conv*UCURdb$`X504`))
              UCURdb$FNA_LYS_G<-ifelse(UCURdb$Protein_per_serving_g=="0",0,ifelse(!is.na(UCURdb$Lysine_per_serving_g),UCURdb$Lysine_per_serving_g,UCURdb$PRO_conv*UCURdb$`X505`))
              UCURdb$FNA_MET_G<-ifelse(UCURdb$Protein_per_serving_g=="0",0,ifelse(!is.na(UCURdb$Methionine_per_serving_g),UCURdb$Methionine_per_serving_g,UCURdb$PRO_conv*UCURdb$`X506`))
              UCURdb$FNA_PHE_G<-ifelse(UCURdb$Protein_per_serving_g=="0",0,ifelse(!is.na(UCURdb$Phenylalanine_per_serving_g),UCURdb$Phenylalanine_per_serving_g,UCURdb$PRO_conv*UCURdb$`X508`))
              UCURdb$FNA_PRO_G<-ifelse(UCURdb$Protein_per_serving_g=="0",0,ifelse(!is.na(UCURdb$Proline_per_serving_g),UCURdb$Proline_per_serving_g,UCURdb$PRO_conv*UCURdb$`X517`))
              UCURdb$FNA_SER_G<-ifelse(UCURdb$Protein_per_serving_g=="0",0,ifelse(!is.na(UCURdb$Serine_per_serving_g),UCURdb$Serine_per_serving_g,UCURdb$PRO_conv*UCURdb$`X518`))
              UCURdb$FNA_THR_G<-ifelse(UCURdb$Protein_per_serving_g=="0",0,ifelse(!is.na(UCURdb$Threonine_per_serving_g),UCURdb$Threonine_per_serving_g,UCURdb$PRO_conv*UCURdb$`X502`))
              UCURdb$FNA_TRP_G<-ifelse(UCURdb$Protein_per_serving_g=="0",0,ifelse(!is.na(UCURdb$Tryptophan_per_serving_g),UCURdb$Tryptophan_per_serving_g,UCURdb$PRO_conv*UCURdb$`X501`))
              UCURdb$FNA_TYR_G<-ifelse(UCURdb$Protein_per_serving_g=="0",0,ifelse(!is.na(UCURdb$Tyrosine_per_serving_g),UCURdb$Tyrosine_per_serving_g,UCURdb$PRO_conv*UCURdb$`X509`))
              UCURdb$FNA_VAL_G<-ifelse(UCURdb$Protein_per_serving_g=="0",0,ifelse(!is.na(UCURdb$Valine_per_serving_g),UCURdb$Valine_per_serving_g,UCURdb$PRO_conv*UCURdb$`X510`))
            
              
              ################
              #FAT CONVERSIONS
              ################
              
              #
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
             UCURdb$FNA_CARTA<-ifelse(!is.na(UCURdb$Carotene_alpha_per_serving_mcg),UCURdb$Carotene_alpha_per_serving_mcg,UCURdb$`X322`)
             UCURdb$FNA_CARTB<-ifelse(!is.na(UCURdb$Carotene_beta_per_serving_mcg),UCURdb$Carotene_beta_per_serving_mcg,UCURdb$`X321`)
             UCURdb$FNA_CRYPX<-ifelse(!is.na(UCURdb$Cryptoxanthin_beta_per_serving_mcg),UCURdb$Cryptoxanthin_beta_per_serving_mcg,UCURdb$`X334`)
             UCURdb$FNA_VITK1D<-ifelse(!is.na(UCURdb$Dihydrophylloquinone_per_serving_mcg),UCURdb$Dihydrophylloquinone_per_serving_mcg,UCURdb$`X429`)
             UCURdb$FNA_FOLDFE<-ifelse(!is.na(UCURdb$Folate_DFE_per_serving_mcg),UCURdb$Folate_DFE_per_serving_mcg,UCURdb$`X435`)
             UCURdb$FNA_FOLFD<-ifelse(!is.na(UCURdb$Folate_food_per_serving_mcg),UCURdb$Folate_food_per_serving_mcg,UCURdb$`X432`)
             UCURdb$FNA_MK4<-ifelse(!is.na(UCURdb$Menaquinone_4_per_serving_mcg),UCURdb$Menaquinone_4_per_serving_mcg,UCURdb$`X428`)
             UCURdb$FNA_FOL<-ifelse(!is.na(UCURdb$Folate_mcg),UCURdb$Folate_mcg,UCURdb$`X417`)
             UCURdb$FNA_VITD<-ifelse(!is.na(UCURdb$Vitamin_D_mcg),UCURdb$Vitamin_D_mcg,UCURdb$`X328`)
             UCURdb$FNA_TOCPHA<-ifelse(!is.na(UCURdb$Vitamin_E_mg),UCURdb$Vitamin_E_mg,UCURdb$`X323`)
             UCURdb$FNA_RETOL<-ifelse(!is.na(UCURdb$Retinol_per_serving_mcg),UCURdb$Retinol_per_serving_mcg,UCURdb$`X319`)
             UCURdb$FNA_TOCPHB<-ifelse(!is.na(UCURdb$Tocopherol_beta_per_serving_mg),UCURdb$Tocopherol_beta_per_serving_mg,UCURdb$`X341`)
             UCURdb$FNA_TOCPHD<-ifelse(!is.na(UCURdb$Tocopherol_delta_per_serving_mg),UCURdb$Tocopherol_delta_per_serving_mg,UCURdb$`X343`)
             UCURdb$FNA_TOCPHG<-ifelse(!is.na(UCURdb$Tocopherol_gamma_per_serving_mg),UCURdb$Tocopherol_gamma_per_serving_mg,UCURdb$`X342`)
             UCURdb$FNA_TOCTRA<-ifelse(!is.na(UCURdb$Tocotrienol_alpha_per_serving_mg),UCURdb$Tocotrienol_alpha_per_serving_mg,UCURdb$`X344`)
             UCURdb$FNA_TOCTRB<-ifelse(!is.na(UCURdb$Tocotrienol_beta_per_serving_mg),UCURdb$Tocotrienol_beta_per_serving_mg,UCURdb$`X345`)
             UCURdb$FNA_TOCTRD<-ifelse(!is.na(UCURdb$Tocotrienol_delta_per_serving_mg),UCURdb$Tocotrienol_delta_per_serving_mg,UCURdb$`X347`)
             UCURdb$FNA_TOCTRG<-ifelse(!is.na(UCURdb$Tocotrienol_gamma_per_serving_mg),UCURdb$Tocotrienol_gamma_per_serving_mg,UCURdb$`X346`)
             UCURdb$FNA_VITA_RAE<-ifelse(!is.na(UCURdb$Vitamin_A_RAE_per_serving_mcg),UCURdb$Vitamin_A_RAE_per_serving_mcg,UCURdb$`X320`)
             UCURdb$FNA_VITD<-ifelse(!is.na(UCURdb$Vitamin_D_per_serving_IU),UCURdb$Vitamin_D_per_serving_IU,UCURdb$`X324`)
             UCURdb$FNA_BETN<-ifelse(!is.na(UCURdb$Betaine_per_serving_mg),UCURdb$Betaine_per_serving_mg,UCURdb$`X454`)
             UCURdb$FNA_BIO<-ifelse(!is.na(UCURdb$Biotin_mcg),UCURdb$Biotin_mcg,"NA")
             UCURdb$FNA_BOR<-ifelse(!is.na(UCURdb$Boron_mg),UCURdb$Boron_mg,"NA")
             UCURdb$FNA_CAFFN<-ifelse(!is.na(UCURdb$Caffeine_per_serving_mg),UCURdb$Caffeine_per_serving_mg,UCURdb$`X262`)
             UCURdb$FNA_CA<-ifelse(!is.na(UCURdb$Calcium_mg),UCURdb$Calcium_mg,UCURdb$`X301`)
             UCURdb$FNA_CHL<-ifelse(!is.na(UCURdb$Chloride_mg),UCURdb$Chloride_mg,"NA")
             UCURdb$FNA_CHOLE<-ifelse(!is.na(UCURdb$Cholesterol_per_serving_mg),UCURdb$Cholesterol_per_serving_mg,UCURdb$`X601`)
             UCURdb$FNA_CHOLN<-ifelse(!is.na(UCURdb$Choline_mg),UCURdb$Choline_mg,UCURdb$`X421`)
             UCURdb$FNA_CHR<-ifelse(!is.na(UCURdb$Chromium_mcg),UCURdb$Chromium_mcg,"NA")
             UCURdb$FNA_CU<-ifelse(!is.na(UCURdb$Copper_mcg),UCURdb$Copper_mcg,UCURdb$`X312`)
             UCURdb$FNA_FLD<-ifelse(!is.na(UCURdb$Fluoride_mg),UCURdb$Fluoride_mg,UCURdb$`X313`)
             UCURdb$FNA_FOLAC<-ifelse(!is.na(UCURdb$Folic_acid_per_serving_mcg),UCURdb$Folic_acid_per_serving_mcg,UCURdb$`X431`)
             UCURdb$FNA_IOD<-ifelse(!is.na(UCURdb$Iodine_mcg),UCURdb$Iodine_mcg,"NA")
             UCURdb$FNA_FE<-ifelse(!is.na(UCURdb$Iron_mg),UCURdb$Iron_mg,UCURdb$`X303`)
             UCURdb$FNA_LCRN<-ifelse(!is.na(UCURdb$L_Carnitine_mg),UCURdb$L_Carnitine_mg,"NA")
             UCURdb$FNA_LUTZEA<-ifelse(!is.na(UCURdb$Lutein_zeaxanthin_per_serving_mcg),UCURdb$Lutein_zeaxanthin_per_serving_mcg,UCURdb$`X338`)
             UCURdb$FNA_LYCPN<-ifelse(!is.na(UCURdb$Lycopene_per_serving_mcg),UCURdb$Lycopene_per_serving_mcg,UCURdb$`X337`)
             UCURdb$FNA_MG<-ifelse(!is.na(UCURdb$Magnesium_mg),UCURdb$Magnesium_mg,UCURdb$`X304`)
             UCURdb$FNA_MN<-ifelse(!is.na(UCURdb$Manganese_mg),UCURdb$Manganese_mg,UCURdb$`X315`)
             UCURdb$FNA_MLY<-ifelse(!is.na(UCURdb$Molybdenum_mcg),UCURdb$Molybdenum_mcg,"NA")
             UCURdb$FNA_PROadj<-ifelse(!is.na(UCURdb$Adjusted_Protein_per_serving_g),UCURdb$Adjusted_Protein_per_serving_g,UCURdb$`X257`)
             UCURdb$FNA_ENERC_KJ<-ifelse(!is.na(UCURdb$Energy_per_serving_kJ),UCURdb$Energy_per_serving_kJ,UCURdb$`X268`)
             UCURdb$FNA_VITEadd<-ifelse(!is.na(UCURdb$Vitamin_E_added_per_serving_mg),UCURdb$Vitamin_E_added_per_serving_mg,UCURdb$`X573`)
             UCURdb$FNA_VITBadd<-ifelse(!is.na(UCURdb$Vitamin_B_12_added_per_serving_mcg),UCURdb$Vitamin_B_12_added_per_serving_mcg,UCURdb$`X578`)
             UCURdb$FNA_ERGCAL<-ifelse(!is.na(UCURdb$Vitamin_D2_ergocalciferol_per_serving_mcg),UCURdb$Vitamin_D2_ergocalciferol_per_serving_mcg,UCURdb$`X325`)
             UCURdb$FNA_CHOCAL<-ifelse(!is.na(UCURdb$Vitamin_D3_cholecalciferol_per_serving_mcg),UCURdb$Vitamin_D3_cholecalciferol_per_serving_mcg,UCURdb$`X326`)
             UCURdb$FNA_NAC<-ifelse(!is.na(UCURdb$N_acetyl_Carnitine_mg),UCURdb$N_acetyl_Carnitine_mg,"NA")
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

              foodomics<-UCURdb[ , c(1:8,97:100, 399:565) ]
              
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
     
