###############################
##########MAIN DATABASE########
###############################

      ###############################
      #####TRANSLATE DAILY VALUES####
      ###############################
      
      #NOTE: Reference Values for Nutrition Labeling, Based on a 2,000 Calorie Intake, for Adults and Children 4 or More Years of Age

      ###DAILY VALUE CALCULATIONS ---> this code chunk takes daily value percentages and multiplies them by the weight of the daily value from 
      #http://www.fda.gov/Food/GuidanceRegulation/GuidanceDocumentsRegulatoryInformation/LabelingNutrition/ucm064928.htm.
      #The end result of the conversion is in the same units as the USDA unit for that nutrient
      compiledNFD$Sodium_DV<-compiledNFD$Sodium_DV*2400/100
      compiledNFD$Potassium_DV<-compiledNFD$Potassium_DV*3500/100
      compiledNFD$Vitamin_A_DV<-compiledNFD$Vitamin_A_DV*5000*.3/100
      compiledNFD$Vitamin_C_DV<-compiledNFD$Vitamin_C_DV*60/100
      compiledNFD$Calcium_DV<-compiledNFD$Calcium_DV*1000/100
      compiledNFD$Iron_DV<-compiledNFD$Iron_DV*18/100
      compiledNFD$Vitamin_E_DV<-compiledNFD$Vitamin_E_DV*30/100
      compiledNFD$Thiamin_DV<-compiledNFD$Thiamin_DV*1.5/100
      compiledNFD$Riboflavin_DV<-compiledNFD$Riboflavin_DV*1.7/100
      compiledNFD$Niacin_DV<-compiledNFD$Niacin_DV*20/100
      compiledNFD$Vitamin_B6_DV<-compiledNFD$Vitamin_B6_DV*2/100
      compiledNFD$Vitamin_B12_DV<-compiledNFD$Vitamin_B12_DV*6/100
      compiledNFD$Zinc_DV<-compiledNFD$Zinc_DV*15/100
      compiledNFD$Vitamin_D_DV<-compiledNFD$Vitamin_D_DV*400 * 0.025/100
      #vitamin D conversion from http://dietarysupplementdatabase.usda.nih.gov/ingredient_calculator/equation.php
      compiledNFD$Phosphorus_DV<-compiledNFD$Phosphorus_DV*1000/100
      compiledNFD$Magnesium_DV<-compiledNFD$Magnesium_DV*400/100
      compiledNFD$Vitamin_K_DV<-compiledNFD$Vitamin_K_DV*80/100
      compiledNFD$Folate_DV<-compiledNFD$Folate_DV*400/100
      compiledNFD$Pantothenic_DV<-compiledNFD$Pantothenic_DV*10/100
      compiledNFD$Copper_DV<-compiledNFD$Copper_DV*2/100
      compiledNFD$Selenium_DV<-compiledNFD$Selenium_DV*70/100
      compiledNFD$Manganese_DV<-compiledNFD$Manganese_DV*2/100
      
      compiledNFD$Biotin_DV<-compiledNFD$Biotin_DV*300/100
      compiledNFD$Iodine_DV<-compiledNFD$Iodine_DV*150/100
      compiledNFD$Chromium_DV<-compiledNFD$Chromium_DV*120/100
      compiledNFD$Molybdenum_DV<-compiledNFD$Molybdenum_DV*75/100
      compiledNFD$Chloride_DV<-compiledNFD$Chloride_DV*3400/100
      
      #############################################
      #####USE DV/milligram DAILY VALUE AMOUNTS####
      #############################################
      
      ##USE MG IF AVAILABLE, ELSE USE DAILY VALUES
      compiledNFD$Sodium_mg<-ifelse(!is.na(compiledNFD$Sodium_mg),compiledNFD$Sodium_mg,compiledNFD$Sodium_DV)
      compiledNFD$Potassium_mg<-ifelse(!is.na(compiledNFD$Potassium_mg),compiledNFD$Potassium_mg,compiledNFD$Potassium_DV)
      compiledNFD$Vitamin_A_mcg<-ifelse(!is.na(compiledNFD$Vitamin_A_mcg),compiledNFD$Vitamin_A_mcg,compiledNFD$Vitamin_A_DV)
      compiledNFD$Vitamin_C_mg<-ifelse(!is.na(compiledNFD$Vitamin_C_mg),compiledNFD$Vitamin_C_mg,compiledNFD$Vitamin_C_DV)
      compiledNFD$Calcium_mg<-ifelse(!is.na(compiledNFD$Calcium_mg),compiledNFD$Calcium_mg,compiledNFD$Calcium_DV)
      compiledNFD$Iron_mg<-ifelse(!is.na(compiledNFD$Iron_mg),compiledNFD$Iron_mg,compiledNFD$Iron_DV)
      compiledNFD$Vitamin_E_mg<-ifelse(!is.na(compiledNFD$Vitamin_E_mg),compiledNFD$Vitamin_E_mg,compiledNFD$Vitamin_E_DV)
      compiledNFD$Thiamin_mg<-ifelse(!is.na(compiledNFD$Thiamin_mg),compiledNFD$Thiamin_mg,compiledNFD$Thiamin_DV)
      compiledNFD$Riboflavin_mg<-ifelse(!is.na(compiledNFD$Riboflavin_mg),compiledNFD$Riboflavin_mg,compiledNFD$Riboflavin_DV)
      compiledNFD$Niacin_mg<-ifelse(!is.na(compiledNFD$Niacin_mg),compiledNFD$Niacin_mg,compiledNFD$Niacin_DV)
      compiledNFD$Vitamin_B6_mg<-ifelse(!is.na(compiledNFD$Vitamin_B6_mg),compiledNFD$Vitamin_B6_mg,compiledNFD$Vitamin_B6_DV)
      compiledNFD$Vitamin_B12_mcg<-ifelse(!is.na(compiledNFD$Vitamin_B12_mcg),compiledNFD$Vitamin_B12_mcg,compiledNFD$Vitamin_B12_DV)
      compiledNFD$Zinc_mg<-ifelse(!is.na(compiledNFD$Zinc_mg),compiledNFD$Zinc_mg,compiledNFD$Zinc_DV)
      compiledNFD$Vitamin_D_mcg<-ifelse(!is.na(compiledNFD$Vitamin_D_mcg),compiledNFD$Vitamin_D_mcg,compiledNFD$Vitamin_D_DV)
      compiledNFD$Phosphorus_mg<-ifelse(!is.na(compiledNFD$Phosphorus_mg),compiledNFD$Phosphorus_mg,compiledNFD$Phosphorus_DV)
      compiledNFD$Magnesium_mg<-ifelse(!is.na(compiledNFD$Magnesium_mg),compiledNFD$Magnesium_mg,compiledNFD$Magnesium_DV)
      compiledNFD$Vitamin_K_mcg<-ifelse(!is.na(compiledNFD$Vitamin_K_mcg),compiledNFD$Vitamin_K_mcg,compiledNFD$Vitamin_K_DV)
      compiledNFD$Folate_mcg<-ifelse(!is.na(compiledNFD$Folate_mcg),compiledNFD$Folate_mcg,compiledNFD$Folate_DV)
      compiledNFD$Pantothenic_mg<-ifelse(!is.na(compiledNFD$Pantothenic_mg),compiledNFD$Pantothenic_mg,compiledNFD$Pantothenic_DV)
      compiledNFD$Copper_mcg<-ifelse(!is.na(compiledNFD$Copper_mcg),compiledNFD$Copper_mcg,compiledNFD$Copper_DV)
      compiledNFD$Selenium_mcg<-ifelse(!is.na(compiledNFD$Selenium_mcg),compiledNFD$Selenium_mcg,compiledNFD$Selenium_DV)
      compiledNFD$Manganese_mg<-ifelse(!is.na(compiledNFD$Manganese_mg),compiledNFD$Manganese_mg,compiledNFD$Manganese_DV)
      compiledNFD$Choline_mg<-ifelse(!is.na(compiledNFD$Choline_mg),compiledNFD$Choline_mg,compiledNFD$Choline_DV)
      compiledNFD$Biotin_mcg<-ifelse(!is.na(compiledNFD$Biotin_mcg),compiledNFD$Biotin_mcg,compiledNFD$Biotin_DV)
      compiledNFD$Iodine_mcg<-ifelse(!is.na(compiledNFD$Iodine_mcg),compiledNFD$Iodine_mcg,compiledNFD$Iodine_DV)
      compiledNFD$Chromium_mcg<-ifelse(!is.na(compiledNFD$Chromium_mcg),compiledNFD$Chromium_mcg,compiledNFD$Chromium_DV)
      compiledNFD$Molybdenum_mcg<-ifelse(!is.na(compiledNFD$Molybdenum_mcg),compiledNFD$Molybdenum_mcg,compiledNFD$Molybdenum_DV)
      compiledNFD$Chloride_mg<-ifelse(!is.na(compiledNFD$Chloride_mg),compiledNFD$Chloride_mg,compiledNFD$Chloride_DV)

      
###############################
############BABY FOODS#########
###############################      

      #NOTE: Reference Values for Nutrition Labeling for Infants
      
      ###############################
      #####TRANSLATE DAILY VALUES####
      ###############################
      
      ###DAILY VALUE CALCULATIONS ---> this code chunk takes daily value percentages and multiplies them by the weight of the daily value from 
      #http://www.fda.gov/Food/GuidanceRegulation/GuidanceDocumentsRegulatoryInformation/LabelingNutrition/ucm064928.htm.
      #The end result of the conversion is in the same units as the USDA unit for that nutrient
      baby_foods$Vitamin_A_DV<-baby_foods$Vitamin_A_DV*1500*.3/100
      baby_foods$Vitamin_C_DV<-baby_foods$Vitamin_C_DV*35/100
      baby_foods$Calcium_DV<-baby_foods$Calcium_DV*600/100
      baby_foods$Iron_DV<-baby_foods$Iron_DV*15/100
      baby_foods$Vitamin_E_DV<-baby_foods$Vitamin_E_DV*5/100
      baby_foods$Thiamin_DV<-baby_foods$Thiamin_DV*.5/100
      baby_foods$Riboflavin_DV<-baby_foods$Riboflavin_DV*.6/100
      baby_foods$Niacin_DV<-baby_foods$Niacin_DV*8/100
      baby_foods$Vitamin_B6_DV<-baby_foods$Vitamin_B6_DV*.4/100
      baby_foods$Vitamin_B12_DV<-baby_foods$Vitamin_B12_DV*2/100
      baby_foods$Zinc_DV<-baby_foods$Zinc_DV*5/100
      baby_foods$Vitamin_D_DV<-baby_foods$Vitamin_D_DV*400 * 0.025/100
      #vitamin D conversion from http://dietarysupplementdatabase.usda.nih.gov/ingredient_calculator/equation.php
      baby_foods$Phosphorus_DV<-baby_foods$Phosphorus_DV*500/100
      baby_foods$Magnesium_DV<-baby_foods$Magnesium_DV*70/100
      baby_foods$Folate_DV<-baby_foods$Folate_DV*100/100
      baby_foods$Pantothenic_DV<-baby_foods$Pantothenic_DV*3/100
      baby_foods$Copper_DV<-baby_foods$Copper_DV*.6/100
      baby_foods$Biotin_DV<-baby_foods$Biotin_DV*50/100
      baby_foods$Iodine_DV<-baby_foods$Iodine_DV*45/100

      
      #############################################
      #####USE DV/milligram DAILY VALUE AMOUNTS####
      #############################################
      
      ##USE MG IF AVAILABLE, ELSE USE DAILY VALUES
      baby_foods$Vitamin_A_mcg<-ifelse(!is.na(baby_foods$Vitamin_A_mcg),baby_foods$Vitamin_A_mcg,baby_foods$Vitamin_A_DV)
      baby_foods$Vitamin_C_mg<-ifelse(!is.na(baby_foods$Vitamin_C_mg),baby_foods$Vitamin_C_mg,baby_foods$Vitamin_C_DV)
      baby_foods$Calcium_mg<-ifelse(!is.na(baby_foods$Calcium_mg),baby_foods$Calcium_mg,baby_foods$Calcium_DV)
      baby_foods$Iron_mg<-ifelse(!is.na(baby_foods$Iron_mg),baby_foods$Iron_mg,baby_foods$Iron_DV)
      baby_foods$Vitamin_E_mg<-ifelse(!is.na(baby_foods$Vitamin_E_mg),baby_foods$Vitamin_E_mg,baby_foods$Vitamin_E_DV)
      baby_foods$Thiamin_mg<-ifelse(!is.na(baby_foods$Thiamin_mg),baby_foods$Thiamin_mg,baby_foods$Thiamin_DV)
      baby_foods$Riboflavin_mg<-ifelse(!is.na(baby_foods$Riboflavin_mg),baby_foods$Riboflavin_mg,baby_foods$Riboflavin_DV)
      baby_foods$Niacin_mg<-ifelse(!is.na(baby_foods$Niacin_mg),baby_foods$Niacin_mg,baby_foods$Niacin_DV)
      baby_foods$Vitamin_B6_mg<-ifelse(!is.na(baby_foods$Vitamin_B6_mg),baby_foods$Vitamin_B6_mg,baby_foods$Vitamin_B6_DV)
      baby_foods$Vitamin_B12_mcg<-ifelse(!is.na(baby_foods$Vitamin_B12_mcg),baby_foods$Vitamin_B12_mcg,baby_foods$Vitamin_B12_DV)
      baby_foods$Zinc_mg<-ifelse(!is.na(baby_foods$Zinc_mg),baby_foods$Zinc_mg,baby_foods$Zinc_DV)
      baby_foods$Vitamin_D_mcg<-ifelse(!is.na(baby_foods$Vitamin_D_mcg),baby_foods$Vitamin_D_mcg,baby_foods$Vitamin_D_DV)
      baby_foods$Phosphorus_mg<-ifelse(!is.na(baby_foods$Phosphorus_mg),baby_foods$Phosphorus_mg,baby_foods$Phosphorus_DV)
      baby_foods$Magnesium_mg<-ifelse(!is.na(baby_foods$Magnesium_mg),baby_foods$Magnesium_mg,baby_foods$Magnesium_DV)
      baby_foods$Folate_mcg<-ifelse(!is.na(baby_foods$Folate_mcg),baby_foods$Folate_mcg,baby_foods$Folate_DV)
      baby_foods$Pantothenic_mg<-ifelse(!is.na(baby_foods$Pantothenic_mg),baby_foods$Pantothenic_mg,baby_foods$Pantothenic_DV)
      baby_foods$Choline_mg<-ifelse(!is.na(baby_foods$Choline_mg),baby_foods$Choline_mg,baby_foods$Choline_DV)
      baby_foods$Biotin_mcg<-ifelse(!is.na(baby_foods$Biotin_mcg),baby_foods$Biotin_mcg,baby_foods$Biotin_DV)
      baby_foods$Iodine_mcg<-ifelse(!is.na(baby_foods$Iodine_mcg),baby_foods$Iodine_mcg,baby_foods$Iodine_DV)
