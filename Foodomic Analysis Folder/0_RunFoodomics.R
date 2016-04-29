#To use this code:

#First, set your working directory to ~/GitHub/foodomics/Foodomic Analysis Folder
#sys.source('~/GitHub/foodomics/Foodomic Analysis Folder/0_RunFoodomics.R', list(patientfolder="G:/Data_D/D18/Clinic/Patient Folders/Non-Current Patients/TaLy1721408/test", kgid="KG0228.txt"))

#Run a foodomics analysis

patientfolder<-"G:/Data_D/D18/Clinic/Patient Folders/XXXXXXXXXXXXXX"
kgid<-"KGXXXX.txt"
  
#Source the files needed in the analysis
setwd("~/GitHub/foodomics/Foodomic Analysis Folder")
source("1_TS_PreProcessing.R")
setwd("~/GitHub/foodomics/Foodomic Analysis Folder")
source("2_mooshingData.R")
setwd("~/GitHub/foodomics/Foodomic Analysis Folder")
source("3_Wt_Standardize.R")
setwd("~/GitHub/foodomics/Foodomic Analysis Folder")