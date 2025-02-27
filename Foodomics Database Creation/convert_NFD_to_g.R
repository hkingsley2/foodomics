###########################
#####CONVERT NFD VALUES####
###########################

#ASSIGN SUPPLEMENT NUTRITION FACTS INFORMATION TO FOODOMIC VARIABLE COLUMN NAMES
compiledNFD$Carotene_alpha_per_serving_mcg<-compiledNFD$Carotene_alpha_per_serving_mcg*0.000001
compiledNFD$Carotene_beta_per_serving_mcg<-compiledNFD$Carotene_beta_per_serving_mcg*0.000001
compiledNFD$Cryptoxanthin_beta_per_serving_mcg<-compiledNFD$Cryptoxanthin_beta_per_serving_mcg*0.000001
compiledNFD$Dihydrophylloquinone_per_serving_mcg<-compiledNFD$Dihydrophylloquinone_per_serving_mcg*0.000001
compiledNFD$Folate_DFE_per_serving_mcg<-compiledNFD$Folate_DFE_per_serving_mcg*0.000001
compiledNFD$Folate_food_per_serving_mcg<-compiledNFD$Folate_food_per_serving_mcg*0.000001
compiledNFD$Menaquinone_4_per_serving_mcg<-compiledNFD$Menaquinone_4_per_serving_mcg*0.000001
compiledNFD$Folate_mcg<-compiledNFD$Folate_mcg*0.000001
compiledNFD$Vitamin_D_mcg<-compiledNFD$Vitamin_D_mcg*0.000001
compiledNFD$Retinol_per_serving_mcg<-compiledNFD$Retinol_per_serving_mcg*0.000001
compiledNFD$Vitamin_A_RAE_per_serving_mcg<-compiledNFD$Vitamin_A_RAE_per_serving_mcg*0.000001
compiledNFD$Biotin_mcg<-compiledNFD$Biotin_mcg*0.000001
compiledNFD$Chromium_mcg<-compiledNFD$Chromium_mcg*0.000001
compiledNFD$Copper_mcg<-compiledNFD$Copper_mcg*0.000001
compiledNFD$Folic_acid_per_serving_mcg<-compiledNFD$Folic_acid_per_serving_mcg*0.000001
compiledNFD$Iodine_mcg<-compiledNFD$Iodine_mcg*0.000001
compiledNFD$Lutein_zeaxanthin_per_serving_mcg<-compiledNFD$Lutein_zeaxanthin_per_serving_mcg*0.000001
compiledNFD$Lycopene_per_serving_mcg<-compiledNFD$Lycopene_per_serving_mcg*0.000001
compiledNFD$Molybdenum_mcg<-compiledNFD$Molybdenum_mcg*0.000001
compiledNFD$Vitamin_B_12_added_per_serving_mcg<-compiledNFD$Vitamin_B_12_added_per_serving_mcg*0.000001
compiledNFD$Vitamin_D2_ergocalciferol_per_serving_mcg<-compiledNFD$Vitamin_D2_ergocalciferol_per_serving_mcg*0.000001
compiledNFD$Vitamin_D3_cholecalciferol_per_serving_mcg<-compiledNFD$Vitamin_D3_cholecalciferol_per_serving_mcg*0.000001
compiledNFD$Selenium_mcg<-compiledNFD$Selenium_mcg*0.000001
compiledNFD$Silicon_mcg<-compiledNFD$Silicon_mcg*0.000001
compiledNFD$Vitamin_A_mcg<-compiledNFD$Vitamin_A_mcg*0.000001
compiledNFD$Vitamin_B12_mcg<-compiledNFD$Vitamin_B12_mcg*0.000001
compiledNFD$Vitamin_K_mcg<-compiledNFD$Vitamin_K_mcg*0.000001
compiledNFD$LA_per_serving_g<-compiledNFD$LA_per_serving_g*0.001
compiledNFD$ALA_per_serving_g<-compiledNFD$ALA_per_serving_g*0.001
compiledNFD$ARA_per_serving_g<-compiledNFD$ARA_per_serving_g*0.001
compiledNFD$EPA_per_serving_g<-compiledNFD$EPA_per_serving_g*0.001
compiledNFD$DHA_per_serving_g<-compiledNFD$DHA_per_serving_g*0.001
compiledNFD$Vitamin_E_mg<-compiledNFD$Vitamin_E_mg*0.001
compiledNFD$Tocopherol_beta_per_serving_mg<-compiledNFD$Tocopherol_beta_per_serving_mg*0.001
compiledNFD$Tocopherol_delta_per_serving_mg<-compiledNFD$Tocopherol_delta_per_serving_mg*0.001
compiledNFD$Tocopherol_gamma_per_serving_mg<-compiledNFD$Tocopherol_gamma_per_serving_mg*0.001
compiledNFD$Tocotrienol_alpha_per_serving_mg<-compiledNFD$Tocotrienol_alpha_per_serving_mg*0.001
compiledNFD$Tocotrienol_beta_per_serving_mg<-compiledNFD$Tocotrienol_beta_per_serving_mg*0.001
compiledNFD$Tocotrienol_delta_per_serving_mg<-compiledNFD$Tocotrienol_delta_per_serving_mg*0.001
compiledNFD$Tocotrienol_gamma_per_serving_mg<-compiledNFD$Tocotrienol_gamma_per_serving_mg*0.001
compiledNFD$Betaine_per_serving_mg<-compiledNFD$Betaine_per_serving_mg*0.001
compiledNFD$Boron_mg<-compiledNFD$Boron_mg*0.001
compiledNFD$Caffeine_per_serving_mg<-compiledNFD$Caffeine_per_serving_mg*0.001
compiledNFD$Calcium_mg<-compiledNFD$Calcium_mg*0.001
compiledNFD$Chloride_mg<-compiledNFD$Chloride_mg*0.001
compiledNFD$Cholesterol_per_serving_mg<-compiledNFD$Cholesterol_per_serving_mg*0.001
compiledNFD$Choline_mg<-compiledNFD$Choline_mg*0.001
compiledNFD$Fluoride_mg<-compiledNFD$Fluoride_mg*0.001
compiledNFD$Iron_mg<-compiledNFD$Iron_mg*0.001
compiledNFD$L_Carnitine_mg<-compiledNFD$L_Carnitine_mg*0.001
compiledNFD$Magnesium_mg<-compiledNFD$Magnesium_mg*0.001
compiledNFD$Manganese_mg<-compiledNFD$Manganese_mg*0.001
compiledNFD$Vitamin_E_added_per_serving_mg<-compiledNFD$Vitamin_E_added_per_serving_mg*0.001
compiledNFD$N_acetyl_Carnitine_mg<-compiledNFD$N_acetyl_Carnitine_mg*0.001
compiledNFD$Niacin_mg<-compiledNFD$Niacin_mg*0.001
compiledNFD$Nickel_mg<-compiledNFD$Nickel_mg*0.001
compiledNFD$Pantothenic_mg<-compiledNFD$Pantothenic_mg*0.001
compiledNFD$Phosphorus_mg<-compiledNFD$Phosphorus_mg*0.001
compiledNFD$Potassium_mg<-compiledNFD$Potassium_mg*0.001
compiledNFD$Riboflavin_mg<-compiledNFD$Riboflavin_mg*0.001
compiledNFD$Sodium_mg<-compiledNFD$Sodium_mg*0.001
compiledNFD$Theobromine_per_serving_mg<-compiledNFD$Theobromine_per_serving_mg*0.001
compiledNFD$Thiamin_mg<-compiledNFD$Thiamin_mg*0.001
compiledNFD$Tin_mg<-compiledNFD$Tin_mg*0.001
compiledNFD$Vanadium_mg<-compiledNFD$Vanadium_mg*0.001
compiledNFD$Vitamin_B6_mg<-compiledNFD$Vitamin_B6_mg*0.001
compiledNFD$Vitamin_C_mg<-compiledNFD$Vitamin_C_mg*0.001
compiledNFD$Zinc_mg<-compiledNFD$Zinc_mg*0.001
compiledNFD$Beta_sitosterol_per_serving_mg<-compiledNFD$Beta_sitosterol_per_serving_mg*0.001
compiledNFD$Campesterol_per_serving_mg<-compiledNFD$Campesterol_per_serving_mg*0.001
compiledNFD$Phytosterols_per_serving_mg<-compiledNFD$Phytosterols_per_serving_mg*0.001
compiledNFD$Stigmasterol_per_serving_mg<-compiledNFD$Stigmasterol_per_serving_mg*0.001

