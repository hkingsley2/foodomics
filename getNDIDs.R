#Get NDIDs from patient intake sheets


#Get all the files in the MySQL directory that we want
file_list <- list.files(getwd())

for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.csv(file, header=TRUE, sep="\t")
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.csv(file, header=TRUE, sep="\t")
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  
}

#Get only the NDIDs
preLIST <- dataset[,c("MRNUMBER","PKT_Recipe_Ingredient_Amount","Ingredient_ID")]

#Remove duplicated NDIDs
preLIST$Ingredient_ID<-substring(preLIST$Ingredient_ID, 5)
eatenNDIDs<-unique(preLIST$Ingredient_ID)