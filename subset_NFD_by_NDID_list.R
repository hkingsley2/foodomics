#Import required database files

compiledNFD<- read.csv(file = "formatting_condensed_db_export.txt", header = TRUE, sep="\t", na.strings=c("NA","NaN", " ", "N/A", ""))
              
                                         # na.strings=c("NA","NaN", " "), encoding = 'ASCI', fileEncoding='UTF-8') #import nutrition facts label database

compiledNFD[,c(5:7,13:104,123:124,141:250)] <- sapply(compiledNFD[,c(5:7,13:104,123:124,141:250)], as.numeric)

                                         # NFL[NFL==""] <- NA

NFLexpand<-c("Adjusted_Protein_per_serving_g",
             "Alanine_per_serving_g",
             "Alcohol_ethyl_per_serving_g",
             "Arginine_per_serving_g",
             "Ash_per_serving_g",
             "Aspartic_acid_per_serving_g",
             "Beta_sitosterol_per_serving_mg",
             "Betaine_per_serving_mg",
             "Caffeine_per_serving_mg",
             "Campesterol_per_serving_mg",
             "Carotene_alpha_per_serving_µg",
             "Carotene_beta_per_serving_µg",
             "Cryptoxanthin_beta_per_serving_µg",
             "Cystine_per_serving_g",
             "Dihydrophylloquinone_per_serving_µg",
             "Energy_per_serving_kJ",
             "F10D0_per_serving_g",
             "F12D0_per_serving_g",
             "F13D0_per_serving_g",
             "F14D0_per_serving_g",
             "F14D1_per_serving_g",
             "F15D0_per_serving_g",
             "F15D1_per_serving_g",
             "F16D0_per_serving_g",
             "F16D1_per_serving_g",
             "F16D1C_per_serving_g",
             "F16D1T_per_serving_g",
             "F17D0_per_serving_g",
             "F17D1_per_serving_g",
             "F182I_per_serving_g",
             "F183I_per_serving_g",
             "F18D0_per_serving_g",
             "F18D1_per_serving_g",
             "F18D1C_per_serving_g",
             "F18D1T_per_serving_g",
             "F18D1TN7_per_serving_g",
             "F18D2CLA_per_serving_g",
             "F18D2CN6_per_serving_g",
             "F18D2TT_per_serving_g",
             "F18D2X_per_serving_g",
             "F18D3CN3_per_serving_g",
             "F18D3CN6_per_serving_g",
             "F18D4_per_serving_g",
             "F20D0_per_serving_g",
             "F20D1_per_serving_g",
             "F20D2CN6_per_serving_g",
             "F20D3_per_serving_g",
             "F20D3N3_per_serving_g",
             "F20D3N6_per_serving_g",
             "F20D4_per_serving_g",
             "F21D5_per_serving_g",
             "F22D0_per_serving_g",
             "F22D1_per_serving_g",
             "F22D1C_per_serving_g",
             "F22D1T_per_serving_g",
             "F22D4_per_serving_g",
             "F22D5_per_serving_g",
             "F24D0_per_serving_g",
             "F24D1C_per_serving_g",
             "F4D0_per_serving_g",
             "F6D0_per_serving_g",
             "F8D0_per_serving_g",
             "FATRNM_per_serving_g",
             "FATRNP_per_serving_g",
             "Folate_DFE_per_serving_µg",
             "Folate_food_per_serving_µg",
             "Folic_acid_per_serving_µg",
             "Fructose_per_serving_g",
             "Galactose_per_serving_g",
             "Glucose_per_serving_g",
             "Glutamic_acid_per_serving_g",
             "Glycine_per_serving_g",
             "Histidine_per_serving_g",
             "Hydroxyproline_per_serving_g",
             "Isoleucine_per_serving_g",
             "L_Carnitine_mg",
             "Lactose_per_serving_g",
             "Leucine_per_serving_g",
             "`Lutein_+_zeaxanthin_per_serving_µg`",
             "Lycopene_per_serving_µg",
             "Lysine_per_serving_g",
             "Maltose_per_serving_g",
             "Menaquinone_4_per_serving_µg",
             "Methionine_per_serving_g",
             "N_acetyl_Carnitine_mg",
             "Phenylalanine_per_serving_g",
             "Phytosterols_per_serving_mg",
             "Proline_per_serving_g",
             "Retinol_per_serving_µg",
             "Serine_per_serving_g",
             "Soluble_Fiber_per_serving_g",
             "Starch_per_serving_g",
             "Stigmasterol_per_serving_mg",
             "Sucrose_per_serving_g",
             "Theobromine_per_serving_mg",
             "Threonine_per_serving_g",
             "Tocopherol_beta_per_serving_mg",
             "Tocopherol_delta_per_serving_mg",
             "Tocopherol_gamma_per_serving_mg",
             "Tocotrienol_alpha_per_serving_mg",
             "Tocotrienol_beta_per_serving_mg",
             "Tocotrienol_delta_per_serving_mg",
             "Tocotrienol_gamma_per_serving_mg",
             "Tryptophan_per_serving_g",
             "Tyrosine_per_serving_g",
             "Valine_per_serving_g",
             "Vitamin_A_RAE_per_serving_µg",
             "Vitamin_B_12_added_per_serving_µg",
             "Vitamin_D_per_serving_IU",
             "`Vitamin_D2_(ergocalciferol)_per_serving_µg`",
             "`Vitamin_D3_(cholecalciferol)_per_serving_µg`",
             "Vitamin_E_added_per_serving_mg",
             "Water_per_serving_g")

NFL[,c(NFLexpand)] <- NA


USDA<-read.csv(file="TEST_SR28_PROFILE_DATA.csv", header=TRUE) #import USDA profile data
#  need to import blanks as NA
USDAdef<-read.csv(file="NUTR_DEF.csv", header=FALSE) #import USDA profile definitions, not sure if we need this
USDAdef$V1<-gsub("~", "", USDAdef$V1)
USDAdef$V2<-gsub("~", "", USDAdef$V2)
USDAdef$V3<-gsub("~", "", USDAdef$V3)

#1) Create a pre-nutrient database (contains unformatted NFL/USDA data)
#subset USDA database to foods in the NFL database
USDAmod = USDA[USDA$NDB_No %in% NFL$NDB_No, ] 
#Transform USDA data from parameter/100g food to parameter/g serving in NFL database
NFL$NFL_Factor= 100/(as.numeric(NFL$Weight_per_serving_g))
NFL[,20:316] <- sapply(NFL[, 20:316], as.numeric)
NFL[,20:316]=as.numeric(apply(NFL[,20:316], 2, function(x) x * NFL$NFL_Factor))
#Merge the USDA data with the NFL data by food ID (NDB_No)
NFL_USDA = merge(x = USDAmod, y = NFL, by = "NDB_No", all = TRUE)