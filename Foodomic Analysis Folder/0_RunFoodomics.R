#To use this code:

#First, set your working directory to ~/GitHub/foodomics/Foodomic Analysis Folder

#Second, set variables

patientfolder<-"G:/Data_D/D18/Clinic/Patient Folders/XXXXXXXXXXXXXX"
kgid<-"KGXXXX.txt"
  

#Third, run a foodomics analysis
#Source the files needed in the analysis
setwd("~/GitHub/foodomics/Foodomic Analysis Folder")
source("1_TS_PreProcessing.R")
setwd("~/GitHub/foodomics/Foodomic Analysis Folder")
source("2_mooshingData.R")
setwd("~/GitHub/foodomics/Foodomic Analysis Folder")
source("3_Wt_Standardize.R")
setwd("~/GitHub/foodomics/Foodomic Analysis Folder")
