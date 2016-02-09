#Get NDIDs from patient intake sheets
#for some reason files from 1030840 1062595 and 1094584 did not work

#Get all the files in the MySQL directory that we want
file_list <- list.files(getwd())

for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.csv(file, header=TRUE, sep="\t")[ ,c(1:13)]
    dataset[is.na(dataset)] <- c("")
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.csv(file, header=TRUE, sep="\t")[ ,c(1:13)]
    temp_dataset[is.na(temp_dataset)] <- c("")
    dataset<-rbind(dataset[ ,c(1:13)], temp_dataset[ ,c(1:13)])
    rm(temp_dataset)
  }
  
}

#Remove if you don't need, some rows import as blank
#dataset<-dataset[-c(464:778),]

write.csv(dataset, "all_patient_ME.csv")
#Get only the NDIDs
preLIST <- dataset[,c("MRNUMBER","PKT_Recipe_Ingredient_Amount","Ingredient_ID")]

#Remove duplicated NDIDs, this gives you a list of brand name products and USDA products (with NDIDs) that were eaten
eatenNDIDs<-unique(preLIST$Ingredient_ID)




################
## For Daily Intakes
## Compile All Patients
###############

#Get NDIDs from patient intake sheets

#Get all the files in the MySQL directory that we want
file_list <- list.files(getwd())

for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.csv(file, header=TRUE, sep="\t")[ ,c(1:5)]
    dataset[is.na(dataset)] <- c("")
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.csv(file, header=TRUE, sep="\t")[ ,c(1:5)]
    temp_dataset[is.na(temp_dataset)] <- c("")
    dataset<-rbind(dataset[ ,c(1:5)], temp_dataset[ ,c(1:5)])
    rm(temp_dataset)
  }
  
}

#Remove if you don't need, some rows import as blank
#dataset<-dataset[-c(464:778),]

write.csv(dataset, "all_patient_DI.csv")
#Get only the NDIDs
preLIST <- dataset[,c("MRNUMBER","PKT_Recipe_Ingredient_Amount","Ingredient_ID")]

#Remove duplicated NDIDs, this gives you a list of brand name products and USDA products (with NDIDs) that were eaten
eatenNDIDs<-unique(preLIST$Ingredient_ID)