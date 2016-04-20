###This script will fun the foodomics analysis


#Step 1: Create reference database (of profiles to use in an analysis)
setwd("~/GitHub/foodomics/Foodomics Database Creation")
source("getBASEP.R")
                      #uses: getADJUSDA.R
                      #creates: referenceBASEP object

#Step 2
#----->Incomplete records are filled in with "NULL", which are treated as NA in database
#----->This is done manually in excel
#----->New empty variables are added, these variables are USDA variables that we do not collect from the label
#----->This requires a template that is formatted specifically for the historical food database (i.e. we need a new template for the 2016 food database)
#----->Result is saved as a tab delimited text file

#Step 3: Create foodomics database of Nutrition Facts Label Adjusted reference profiles
setwd("~/GitHub/foodomics/Foodomics Database Creation")
source("getADJNFL.R")
                      #uses: getSUPPLEMENTS.R
                      #creates: foodomics object





