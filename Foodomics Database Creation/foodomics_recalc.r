#########################################################    
######Script to Calculate Saturation Categories##########
#########################################################  

#Read .CSV file
dataframe<- UCURdb

#Total Fat = `X204`
#Monounsaturated Fat=`X645`
#Polyunsaturated Fat=`X646`
#Saturated Fat=`X606`
#Trans Fat=`X605`


#creates new columns and says that if total fat is 0 on the food label then all the fats should be 0 
dataframe$FAT_FDB <- ifelse(!is.na(dataframe$TOTAL_FAT_PER_SERVING_G
),  dataframe$TOTAL_FAT_PER_SERVING_G, dataframe$X204 )
dataframe$FASAT_FDB <- ifelse(dataframe$TOTAL_FAT_PER_SERVING_G ==0, 0,dataframe$SATURATED_FAT_PER_SERVING_G)  
dataframe$FATRN_FDB <- ifelse(dataframe$TOTAL_FAT_PER_SERVING_G == 0,0,dataframe$TRANS_FAT_PER_SERVING_G)    
dataframe$FAMS_FDB <- ifelse(dataframe$TOTAL_FAT_PER_SERVING_G == 0,0,dataframe$MONOUNSATURATED_FAT_PER_SERVING_G)    
dataframe$FAPU_FDB <- ifelse(dataframe$TOTAL_FAT_PER_SERVING_G == 0,0,dataframe$POLYUNSATURATED_FAT_PER_SERVING_G)    

#creates a new column that counts the number of NA's 
dataframe$na_count <- apply(dataframe[,c("FAT_FDB","FASAT_FDB","FATRN_FDB","FAMS_FDB","FAPU_FDB")],1,function(x) length(which(is.na(x))))

#if there is 1 NA this fills it in with the total fat - sum of other individual fats  
dataframe$FASAT_FDB <- ifelse(dataframe$na_count ==1 & is.na(dataframe$FASAT_FDB),dataframe$TOTAL_FAT_PER_SERVING_G- apply(dataframe[,c("FASAT_FDB","FATRN_FDB","FAMS_FDB","FAPU_FDB")],1, function(x) sum(x,na.rm=TRUE)),dataframe$FASAT_FDB) 
dataframe$FATRN_FDB <- ifelse(dataframe$na_count ==1 & is.na(dataframe$FATRN_FDB),dataframe$TOTAL_FAT_PER_SERVING_G - apply(dataframe[,c("FASAT_FDB","FATRN_FDB","FAMS_FDB","FAPU_FDB")],1, function(x) sum(x,na.rm=TRUE)),dataframe$FATRN_FDB)
dataframe$FAMS_FDB <- ifelse(dataframe$na_count ==1 & is.na(dataframe$FAMS_FDB),dataframe$TOTAL_FAT_PER_SERVING_G - apply(dataframe[,c("FASAT_FDB","FATRN_FDB","FAMS_FDB","FAPU_FDB")],1, function(x) sum(x,na.rm=TRUE)),dataframe$FAMS_FDB)
dataframe$FAPU_FDB <- ifelse(dataframe$na_count ==1 & is.na(dataframe$FAPU_FDB),dataframe$TOTAL_FAT_PER_SERVING_G - apply(dataframe[,c("FASAT_FDB","FATRN_FDB","FAMS_FDB","FAPU_FDB")],1, function(x) sum(x,na.rm=TRUE)),dataframe$FAPU_FDB)


#######by percent calculation I use the fat from the food database and devide it
#######by the sum of the individual fats in the food databasethat do not appear on the label and
######multiply it by the total fat on the food label 

#creates a temporary set of columns that allows the percent calculation to work
dataframe$FASAT_FDB2 <- ifelse(!is.na(dataframe$SATURATED_FAT_PER_SERVING_G),NA,dataframe$X606)
dataframe$FATRN_FDB2 <- ifelse(!is.na(dataframe$TRANS_FAT_PER_SERVING_G),NA,dataframe$X605)
dataframe$FAMS_FDB2 <- ifelse(!is.na(dataframe$MONOUNSATURATED_FAT_PER_SERVING_G),NA,dataframe$X645)
dataframe$FAPU_FDB2 <- ifelse(!is.na(dataframe$POLYUNSATURATED_FAT_PER_SERVING_G),NA,dataframe$X646)

#if there are 2 NA's this fills in with a percent calcuation 
dataframe$FASAT_FDB <- ifelse(dataframe$na_count == 2 & is.na(dataframe$FASAT_FDB),dataframe$X606/apply(dataframe[,c("FASAT_FDB2","FATRN_FDB2","FAMS_FDB2","FAPU_FDB2")],1,function(x) sum(x,na.rm=TRUE))*(dataframe$FAT_FDB-apply(dataframe[,c("SATURATED_FAT_PER_SERVING_G","TRANS_FAT_PER_SERVING_G","MONOUNSATURATED_FAT_PER_SERVING_G","POLYUNSATURATED_FAT_PER_SERVING_G")],1,function(x) sum(x,na.rm=TRUE))),dataframe$FASAT_FDB)
dataframe$FATRN_FDB <- ifelse(dataframe$na_count == 2 & is.na(dataframe$FATRN_FDB),dataframe$X605/apply(dataframe[,c("FASAT_FDB2","FATRN_FDB2","FAMS_FDB2","FAPU_FDB2")],1,function(x) sum(x,na.rm=TRUE))*(dataframe$FAT_FDB-apply(dataframe[,c("SATURATED_FAT_PER_SERVING_G","TRANS_FAT_PER_SERVING_G","MONOUNSATURATED_FAT_PER_SERVING_G","POLYUNSATURATED_FAT_PER_SERVING_G")],1,function(x) sum(x,na.rm=TRUE))),dataframe$FATRN_FDB)
dataframe$FAMS_FDB <- ifelse(dataframe$na_count == 2 & is.na(dataframe$FAMS_FDB),dataframe$X645/apply(dataframe[,c("FASAT_FDB2","FATRN_FDB2","FAMS_FDB2","FAPU_FDB2")],1,function(x) sum(x,na.rm=TRUE))*(dataframe$FAT_FDB-apply(dataframe[,c("SATURATED_FAT_PER_SERVING_G","TRANS_FAT_PER_SERVING_G","MONOUNSATURATED_FAT_PER_SERVING_G","POLYUNSATURATED_FAT_PER_SERVING_G")],1,function(x) sum(x,na.rm=TRUE))),dataframe$FAMS_FDB)
dataframe$FAPU_FDB <- ifelse(dataframe$na_count == 2 & is.na(dataframe$FAPU_FDB),dataframe$X646/apply(dataframe[,c("FASAT_FDB2","FATRN_FDB2","FAMS_FDB2","FAPU_FDB2")],1,function(x) sum(x,na.rm=TRUE))*(dataframe$FAT_FDB-apply(dataframe[,c("SATURATED_FAT_PER_SERVING_G","TRANS_FAT_PER_SERVING_G","MONOUNSATURATED_FAT_PER_SERVING_G","POLYUNSATURATED_FAT_PER_SERVING_G")],1,function(x) sum(x,na.rm=TRUE))),dataframe$FAPU_FDB)

#if there are 3 NA's this fills in the data with amother percent cacluation
dataframe$FASAT_FDB <- ifelse(dataframe$na_count == 3 & is.na(dataframe$FASAT_FDB), dataframe$X606/apply(dataframe[,c("FASAT_FDB2","FATRN_FDB2","FAMS_FDB2","FAPU_FDB2")],1,function(x) sum(x,na.rm=TRUE))*(dataframe$FAT_FDB-apply(dataframe[,c("SATURATED_FAT_PER_SERVING_G","TRANS_FAT_PER_SERVING_G","MONOUNSATURATED_FAT_PER_SERVING_G","POLYUNSATURATED_FAT_PER_SERVING_G")],1,function(x) sum(x,na.rm=TRUE))),dataframe$FASAT_FDB)
dataframe$FATRN_FDB <- ifelse(dataframe$na_count == 3 & is.na(dataframe$FATRN_FDB), dataframe$X605/apply(dataframe[,c("FASAT_FDB2","FATRN_FDB2","FAMS_FDB2","FAPU_FDB2")],1,function(x) sum(x,na.rm=TRUE))*(dataframe$FAT_FDB-apply(dataframe[,c("SATURATED_FAT_PER_SERVING_G","TRANS_FAT_PER_SERVING_G","MONOUNSATURATED_FAT_PER_SERVING_G","POLYUNSATURATED_FAT_PER_SERVING_G")],1,function(x) sum(x,na.rm=TRUE))),dataframe$FATRN_FDB)
dataframe$FAMS_FDB <- ifelse(dataframe$na_count == 3 & is.na(dataframe$FAMS_FDB), dataframe$X645/apply(dataframe[,c("FASAT_FDB2","FATRN_FDB2","FAMS_FDB2","FAPU_FDB2")],1,function(x) sum(x,na.rm=TRUE))*(dataframe$FAT_FDB-apply(dataframe[,c("SATURATED_FAT_PER_SERVING_G","TRANS_FAT_PER_SERVING_G","MONOUNSATURATED_FAT_PER_SERVING_G","POLYUNSATURATED_FAT_PER_SERVING_G")],1,function(x) sum(x,na.rm=TRUE))),dataframe$FAMS_FDB)
dataframe$FAPU_FDB <- ifelse(dataframe$na_count == 3 & is.na(dataframe$FAPU_FDB), dataframe$X646/apply(dataframe[,c("FASAT_FDB2","FATRN_FDB2","FAMS_FDB2","FAPU_FDB2")],1,function(x) sum(x,na.rm=TRUE))*(dataframe$FAT_FDB-apply(dataframe[,c("SATURATED_FAT_PER_SERVING_G","TRANS_FAT_PER_SERVING_G","MONOUNSATURATED_FAT_PER_SERVING_G","POLYUNSATURATED_FAT_PER_SERVING_G")],1,function(x) sum(x,na.rm=TRUE))),dataframe$FAPU_FDB)

#if there are 4 NA's (only total fat is avalaible) this fills in with percent calculation 
dataframe$FASAT_FDB <- ifelse(dataframe$na_count == 4 & is.na(dataframe$FASAT_FDB), dataframe$X606/apply(dataframe[,c("FASAT_FDB2","FATRN_FDB2","FAMS_FDB2","FAPU_FDB2")],1,function(x) sum(x,na.rm=TRUE))*(dataframe$FAT_FDB-apply(dataframe[,c("SATURATED_FAT_PER_SERVING_G","TRANS_FAT_PER_SERVING_G","MONOUNSATURATED_FAT_PER_SERVING_G","POLYUNSATURATED_FAT_PER_SERVING_G")],1,function(x) sum(x,na.rm=TRUE))),dataframe$FASAT_FDB)
dataframe$FATRN_FDB <- ifelse(dataframe$na_count == 4 & is.na(dataframe$FATRN_FDB), dataframe$X605/apply(dataframe[,c("FASAT_FDB2","FATRN_FDB2","FAMS_FDB2","FAPU_FDB2")],1,function(x) sum(x,na.rm=TRUE))*(dataframe$FAT_FDB-apply(dataframe[,c("SATURATED_FAT_PER_SERVING_G","TRANS_FAT_PER_SERVING_G","MONOUNSATURATED_FAT_PER_SERVING_G","POLYUNSATURATED_FAT_PER_SERVING_G")],1,function(x) sum(x,na.rm=TRUE))),dataframe$FATRN_FDB)
dataframe$FAMS_FDB <- ifelse(dataframe$na_count == 4 & is.na(dataframe$FAMS_FDB), dataframe$X645/apply(dataframe[,c("FASAT_FDB2","FATRN_FDB2","FAMS_FDB2","FAPU_FDB2")],1,function(x) sum(x,na.rm=TRUE))*(dataframe$FAT_FDB-apply(dataframe[,c("SATURATED_FAT_PER_SERVING_G","TRANS_FAT_PER_SERVING_G","MONOUNSATURATED_FAT_PER_SERVING_G","POLYUNSATURATED_FAT_PER_SERVING_G")],1,function(x) sum(x,na.rm=TRUE))),dataframe$FAMS_FDB)
dataframe$FAPU_FDB <- ifelse(dataframe$na_count == 4 & is.na(dataframe$FAPU_FDB), dataframe$X646/apply(dataframe[,c("FASAT_FDB2","FATRN_FDB2","FAMS_FDB2","FAPU_FDB2")],1,function(x) sum(x,na.rm=TRUE))*(dataframe$FAT_FDB-apply(dataframe[,c("SATURATED_FAT_PER_SERVING_G","TRANS_FAT_PER_SERVING_G","MONOUNSATURATED_FAT_PER_SERVING_G","POLYUNSATURATED_FAT_PER_SERVING_G")],1,function(x) sum(x,na.rm=TRUE))),dataframe$FAPU_FDB)

#if all 5 of the food label data is NA then just fill in with database data if possible
dataframe$FAT_FDB <- ifelse(dataframe$na_count == 5 & is.na(dataframe$FAT_FDB),dataframe$X204,dataframe$FAT_FDB)
dataframe$FASAT_FDB <- ifelse(dataframe$na_count == 5 & is.na(dataframe$FASAT_FDB), dataframe$X606,dataframe$FASAT_FDB)
dataframe$FATRN_FDB <- ifelse(dataframe$na_count == 5 & is.na(dataframe$FATRN_FDB), dataframe$X605,dataframe$FATRN_FDB)
dataframe$FAMS_FDB <- ifelse(dataframe$na_count == 5 & is.na(dataframe$FAMS_FDB), dataframe$X645,dataframe$FAMS_FDB)
dataframe$FAPU_FDB <- ifelse(dataframe$na_count == 5 & is.na(dataframe$FAPU_FDB), dataframe$X646,dataframe$FAPU_FDB)

#fixes problem where 0/0 provides NA when it should be producing 0
dataframe$FASAT_FDB <- ifelse(is.na(dataframe$FASAT_FDB) & ((dataframe$FAT_FDB-apply(dataframe[,c("FASAT_FDB","FATRN_FDB","FAMS_FDB","FAPU_FDB")],1,function(x) sum(x,na.rm=TRUE))) == 0), 0, dataframe$FASAT_FDB)
dataframe$FATRN_FDB <- ifelse(is.na(dataframe$FATRN_FDB) & ((dataframe$FAT_FDB-apply(dataframe[,c("FASAT_FDB","FATRN_FDB","FAMS_FDB","FAPU_FDB")],1,function(x) sum(x,na.rm=TRUE))) == 0), 0, dataframe$FATRN_FDB)
dataframe$FAMS_FDB <- ifelse(is.na(dataframe$FAMS_FDB) & ((dataframe$FAT_FDB-apply(dataframe[,c("FASAT_FDB","FATRN_FDB","FAMS_FDB","FAPU_FDB")],1,function(x) sum(x,na.rm=TRUE))) == 0), 0, dataframe$FAMS_FDB)
dataframe$FAPU_FDB <- ifelse(is.na(dataframe$FAPU_FDB) & ((dataframe$FAT_FDB-apply(dataframe[,c("FASAT_FDB","FATRN_FDB","FAMS_FDB","FAPU_FDB")],1,function(x) sum(x,na.rm=TRUE))) == 0), 0, dataframe$FAPU_FDB)



#creates a dataframe to simplify code later
foodlabel <- UCURdb[,c("F4D0_PER_SERVING_G","F6D0_PER_SERVING_G","F8D0_PER_SERVING_G","F10D0_PER_SERVING_G","F12D0_PER_SERVING_G","F13D0_PER_SERVING_G","F14D0_PER_SERVING_G","F15D0_PER_SERVING_G","F16D0_PER_SERVING_G","F17D0_PER_SERVING_G","F18D0_PER_SERVING_G","F20D0_PER_SERVING_G","F22D0_PER_SERVING_G","F24D0_PER_SERVING_G")]

#########################SATURATED FAT#############################
#adds columns to allow for the calculation 
UCURdb$F4D02_for_calc <- ifelse(!is.na(UCURdb$F4D0_PER_SERVING_G),NA,UCURdb$X607)
UCURdb$F6D02_for_calc <- ifelse(!is.na(UCURdb$F6D0_PER_SERVING_G),NA,UCURdb$X608)
UCURdb$F8D02_for_calc <- ifelse(!is.na(UCURdb$F8D0_PER_SERVING_G),NA,UCURdb$X609)
UCURdb$F10D02_for_calc <- ifelse(!is.na(UCURdb$F10D0_PER_SERVING_G),NA,UCURdb$X610)
UCURdb$F12D02_for_calc <- ifelse(!is.na(UCURdb$F12D0_PER_SERVING_G),NA,UCURdb$X611)
UCURdb$F13D02_for_calc <- ifelse(!is.na(UCURdb$F13D0_PER_SERVING_G),NA,UCURdb$X696)
UCURdb$F14D02_for_calc <- ifelse(!is.na(UCURdb$F14D0_PER_SERVING_G),NA,UCURdb$X612)
UCURdb$F15D02_for_calc <- ifelse(!is.na(UCURdb$F15D0_PER_SERVING_G),NA,UCURdb$X652)
UCURdb$F16D02_for_calc <- ifelse(!is.na(UCURdb$F16D0_PER_SERVING_G),NA,UCURdb$X613)
UCURdb$F17D02_for_calc <- ifelse(!is.na(UCURdb$F17D0_PER_SERVING_G),NA,UCURdb$X653)
UCURdb$F18D02_for_calc <- ifelse(!is.na(UCURdb$F18D0_PER_SERVING_G),NA,UCURdb$X614)
UCURdb$F20D02_for_calc <- ifelse(!is.na(UCURdb$F20D0_PER_SERVING_G),NA,UCURdb$X615)
UCURdb$F22D02_for_calc <- ifelse(!is.na(UCURdb$F22D0_PER_SERVING_G),NA,UCURdb$X624)
UCURdb$F24D02_for_calc <- ifelse(!is.na(UCURdb$F24D0_PER_SERVING_G),NA,UCURdb$X654)


#creates the final columns and fills in some of the Na's 
UCURdb$F4D0_FDB <- ifelse(!is.na(UCURdb$F4D0_PER_SERVING_G),UCURdb$F4D0_PER_SERVING_G,ifelse(dataframe$FASAT_FDB == 0, 0,UCURdb$F4D0_PER_SERVING_G))
UCURdb$F6D0_FDB <- ifelse(!is.na(UCURdb$F6D0_PER_SERVING_G),UCURdb$F6D0_PER_SERVING_G,ifelse(dataframe$FASAT_FDB == 0, 0,UCURdb$F6D0_PER_SERVING_G))
UCURdb$F8D0_FDB <- ifelse(!is.na(UCURdb$F8D0_PER_SERVING_G),UCURdb$F8D0_PER_SERVING_G,ifelse(dataframe$FASAT_FDB == 0, 0,UCURdb$F8D0_PER_SERVING_G))
UCURdb$F10D0_FDB <- ifelse(!is.na(UCURdb$F10D0_PER_SERVING_G),UCURdb$F10D0_PER_SERVING_G,ifelse(dataframe$FASAT_FDB == 0, 0,UCURdb$F10D0_PER_SERVING_G))
UCURdb$F12D0_FDB <- ifelse(!is.na(UCURdb$F12D0_PER_SERVING_G),UCURdb$F12D0_PER_SERVING_G,ifelse(dataframe$FASAT_FDB == 0, 0,UCURdb$F12D0_PER_SERVING_G))
UCURdb$F13D0_FDB <- ifelse(!is.na(UCURdb$F13D0_PER_SERVING_G),UCURdb$F13D0_PER_SERVING_G,ifelse(dataframe$FASAT_FDB == 0, 0,UCURdb$F13D0_PER_SERVING_G))
UCURdb$F14D0_FDB <- ifelse(!is.na(UCURdb$F14D0_PER_SERVING_G),UCURdb$F14D0_PER_SERVING_G,ifelse(dataframe$FASAT_FDB == 0, 0,UCURdb$F14D0_PER_SERVING_G))
UCURdb$F15D0_FDB <- ifelse(!is.na(UCURdb$F15D0_PER_SERVING_G),UCURdb$F15D0_PER_SERVING_G,ifelse(dataframe$FASAT_FDB == 0, 0,UCURdb$F15D0_PER_SERVING_G))
UCURdb$F16D0_FDB <- ifelse(!is.na(UCURdb$F16D0_PER_SERVING_G),UCURdb$F16D0_PER_SERVING_G,ifelse(dataframe$FASAT_FDB== 0, 0,UCURdb$F16D0_PER_SERVING_G))
UCURdb$F17D0_FDB <- ifelse(!is.na(UCURdb$F17D0_PER_SERVING_G),UCURdb$F17D0_PER_SERVING_G,ifelse(dataframe$FASAT_FDB == 0, 0,UCURdb$F17D0_PER_SERVING_G))
UCURdb$F18D0_FDB <- ifelse(!is.na(UCURdb$F18D0_PER_SERVING_G),UCURdb$F18D0_PER_SERVING_G,ifelse(dataframe$FASAT_FDB == 0, 0,UCURdb$F18D0_PER_SERVING_G))
UCURdb$F20D0_FDB <- ifelse(!is.na(UCURdb$F20D0_PER_SERVING_G),UCURdb$F20D0_PER_SERVING_G,ifelse(dataframe$FASAT_FDB == 0, 0,UCURdb$F20D0_PER_SERVING_G))
UCURdb$F22D0_FDB <- ifelse(!is.na(UCURdb$F22D0_PER_SERVING_G),UCURdb$F22D0_PER_SERVING_G,ifelse(dataframe$FASAT_FDB == 0, 0,UCURdb$F22D0_PER_SERVING_G))
UCURdb$F24D0_FDB <- ifelse(!is.na(UCURdb$F24D0_PER_SERVING_G),UCURdb$F24D0_PER_SERVING_G,ifelse(dataframe$FASAT_FDB == 0, 0,UCURdb$F24D0_PER_SERVING_G))


#these lines preform the calculation for the saturated fats
UCURdb$F4D0_FDB <- ifelse(is.na(UCURdb$F4D0_FDB),(UCURdb$X607/apply(UCURdb[433:446],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FASAT_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F4D0_FDB)
UCURdb$F6D0_FDB <- ifelse(is.na(UCURdb$F6D0_FDB),(UCURdb$X608/apply(UCURdb[433:446],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FASAT_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F6D0_FDB)
UCURdb$F8D0_FDB <- ifelse(is.na(UCURdb$F8D0_FDB),(UCURdb$X609/apply(UCURdb[433:446],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FASAT_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F8D0_FDB)
UCURdb$F10D0_FDB <- ifelse(is.na(UCURdb$F10D0_FDB),(UCURdb$X610/apply(UCURdb[433:446],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FASAT_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F10D0_FDB)
UCURdb$F12D0_FDB <- ifelse(is.na(UCURdb$F12D0_FDB),(UCURdb$X611/apply(UCURdb[433:446],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FASAT_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F12D0_FDB)
UCURdb$F13D0_FDB <- ifelse(is.na(UCURdb$F13D0_FDB),(UCURdb$X696/apply(UCURdb[433:446],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FASAT_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F13D0_FDB)
UCURdb$F14D0_FDB <- ifelse(is.na(UCURdb$F14D0_FDB),(UCURdb$X612/apply(UCURdb[433:446],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FASAT_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F14D0_FDB)
UCURdb$F15D0_FDB <- ifelse(is.na(UCURdb$F15D0_FDB),(UCURdb$X652/apply(UCURdb[433:446],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FASAT_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F15D0_FDB)
UCURdb$F16D0_FDB <- ifelse(is.na(UCURdb$F16D0_FDB),(UCURdb$X613/apply(UCURdb[433:446],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FASAT_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F16D0_FDB)
UCURdb$F17D0_FDB <- ifelse(is.na(UCURdb$F17D0_FDB),(UCURdb$X653/apply(UCURdb[433:446],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FASAT_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F17D0_FDB)
UCURdb$F18D0_FDB <- ifelse(is.na(UCURdb$F18D0_FDB),(UCURdb$X614/apply(UCURdb[433:446],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FASAT_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F18D0_FDB)
UCURdb$F20D0_FDB <- ifelse(is.na(UCURdb$F20D0_FDB),(UCURdb$X615/apply(UCURdb[433:446],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FASAT_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F20D0_FDB)
UCURdb$F22D0_FDB <- ifelse(is.na(UCURdb$F22D0_FDB),(UCURdb$X624/apply(UCURdb[433:446],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FASAT_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F22D0_FDB)
UCURdb$F24D0_FDB <- ifelse(is.na(UCURdb$F24D0_FDB),(UCURdb$X654/apply(UCURdb[433:446],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FASAT_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F24D0_FDB)


########################MONO FAT#######################################
foodlabel <- UCURdb[,c("F14D1_PER_SERVING_G","F15D1_PER_SERVING_G","F17D1_PER_SERVING_G","F20D1_PER_SERVING_G","F16D1C_PER_SERVING_G","F16D1_PER_SERVING_G","F18D1C_PER_SERVING_G","F18D1_PER_SERVING_G","F22D1C_PER_SERVING_G","F22D1_PER_SERVING_G","F24D1C_PER_SERVING_G")]
UCURdb <- subset(UCURdb, select = -c(433:446))
#adds columns to allow for the calculation 
UCURdb$F14D1_for_calc <- ifelse(!is.na(UCURdb$F14D1_PER_SERVING_G),NA,UCURdb$X625)
UCURdb$F15D1_for_calc <- ifelse(!is.na(UCURdb$F15D1_PER_SERVING_G),NA,UCURdb$X697)
UCURdb$F17D1_for_calc <- ifelse(!is.na(UCURdb$F17D1_PER_SERVING_G),NA,UCURdb$X687)
UCURdb$F20D1_for_calc <- ifelse(!is.na(UCURdb$F20D1_PER_SERVING_G),NA,UCURdb$X628)
UCURdb$F16D1C_for_calc <- ifelse(!is.na(UCURdb$F16D1C_PER_SERVING_G),NA,UCURdb$X673)
UCURdb$F16D1_for_calc <- ifelse(!is.na(UCURdb$F16D1_PER_SERVING_G),NA,UCURdb$X626)
UCURdb$F18D1C_for_calc <- ifelse(!is.na(UCURdb$F18D1C_PER_SERVING_G),NA,UCURdb$X674)
UCURdb$F18D1_for_calc <- ifelse(!is.na(UCURdb$F18D1_PER_SERVING_G),NA,UCURdb$X617)
UCURdb$F22D1C_for_calc <- ifelse(!is.na(UCURdb$F22D1C_PER_SERVING_G),NA,UCURdb$X676)
UCURdb$F22D1_for_calc <- ifelse(!is.na(UCURdb$F22D1_PER_SERVING_G),NA,UCURdb$X630)
UCURdb$F24D1C_for_calc <- ifelse(!is.na(UCURdb$F24D1C_PER_SERVING_G),NA,UCURdb$X671)


#creates the final columns and fills in some of the Na's 
UCURdb$F14D1_FDB <- ifelse(!is.na(UCURdb$F14D1_PER_SERVING_G),UCURdb$F14D1_PER_SERVING_G,ifelse(dataframe$FAMS_FDB == 0, 0,UCURdb$F14D1_PER_SERVING_G))
UCURdb$F15D1_FDB <- ifelse(!is.na(UCURdb$F15D1_PER_SERVING_G),UCURdb$F15D1_PER_SERVING_G,ifelse(dataframe$FAMS_FDB == 0, 0,UCURdb$F15D1_PER_SERVING_G))
UCURdb$F17D1_FDB <- ifelse(!is.na(UCURdb$F17D1_PER_SERVING_G),UCURdb$F17D1_PER_SERVING_G,ifelse(dataframe$FAMS_FDB == 0, 0,UCURdb$F17D1_PER_SERVING_G))
UCURdb$F20D1_FDB <- ifelse(!is.na(UCURdb$F20D1_PER_SERVING_G),UCURdb$F20D1_PER_SERVING_G,ifelse(dataframe$FAMS_FDB == 0, 0,UCURdb$F20D1_PER_SERVING_G))
UCURdb$F16D1C_FDB <- ifelse(!is.na(UCURdb$F16D1C_PER_SERVING_G),UCURdb$F16D1C_PER_SERVING_G,ifelse(dataframe$FAMS_FDB == 0, 0,UCURdb$F16D1C_PER_SERVING_G))
UCURdb$F16D1_FDB <- ifelse(!is.na(UCURdb$F16D1_PER_SERVING_G),UCURdb$F16D1_PER_SERVING_G,ifelse(dataframe$FAMS_FDB == 0, 0,UCURdb$F16D1_PER_SERVING_G))
UCURdb$F18D1C_FDB <- ifelse(!is.na(UCURdb$F18D1C_PER_SERVING_G),UCURdb$F18D1C_PER_SERVING_G,ifelse(dataframe$FAMS_FDB == 0, 0,UCURdb$F18D1C_PER_SERVING_G))
UCURdb$F18D1_FDB <- ifelse(!is.na(UCURdb$F18D1_PER_SERVING_G),UCURdb$F18D1_PER_SERVING_G,ifelse(dataframe$FAMS_FDB == 0, 0,UCURdb$F18D1_PER_SERVING_G))
UCURdb$F22D1C_FDB <- ifelse(!is.na(UCURdb$F22D1C_PER_SERVING_G),UCURdb$F22D1C_PER_SERVING_G,ifelse(dataframe$FAMS_FDB== 0, 0,UCURdb$F22D1C_PER_SERVING_G))
UCURdb$F22D1_FDB <- ifelse(!is.na(UCURdb$F22D1_PER_SERVING_G),UCURdb$F22D1_PER_SERVING_G,ifelse(dataframe$FAMS_FDB == 0, 0,UCURdb$F22D1_PER_SERVING_G))
UCURdb$F24D1C_FDB <- ifelse(!is.na(UCURdb$F24D1C_PER_SERVING_G),UCURdb$F24D1C_PER_SERVING_G,ifelse(dataframe$FAMS_FDB == 0, 0,UCURdb$F24D1C_PER_SERVING_G))

#these lines preform the calculation for the MONO fats
UCURdb$F14D1_FDB <- ifelse(is.na(UCURdb$F14D1_FDB),(UCURdb$X625/apply(UCURdb[447:457],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAMS_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F14D1_FDB)
UCURdb$F15D1_FDB <- ifelse(is.na(UCURdb$F15D1_FDB),(UCURdb$X697/apply(UCURdb[447:457],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAMS_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F15D1_FDB)
UCURdb$F17D1_FDB <- ifelse(is.na(UCURdb$F17D1_FDB),(UCURdb$X687/apply(UCURdb[447:457],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAMS_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F17D1_FDB)
UCURdb$F20D1_FDB <- ifelse(is.na(UCURdb$F20D1_FDB),(UCURdb$X628/apply(UCURdb[447:457],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAMS_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F20D1_FDB)
UCURdb$F16D1C_FDB <- ifelse(is.na(UCURdb$F16D1C_FDB),(UCURdb$X673/apply(UCURdb[447:457],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAMS_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F16D1C_FDB)
UCURdb$F16D1_FDB <- ifelse(is.na(UCURdb$F16D1_FDB),(UCURdb$X626/apply(UCURdb[447:457],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAMS_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F16D1_FDB)
UCURdb$F18D1C_FDB <- ifelse(is.na(UCURdb$F18D1C_FDB),(UCURdb$X674/apply(UCURdb[447:457],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAMS_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F18D1C_FDB)
UCURdb$F18D1_FDB <- ifelse(is.na(UCURdb$F18D1_FDB),(UCURdb$X617/apply(UCURdb[447:457],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAMS_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F18D1_FDB)
UCURdb$F22D1C_FDB <- ifelse(is.na(UCURdb$F22D1C_FDB),(UCURdb$X676/apply(UCURdb[447:457],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAMS_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F22D1C_FDB)
UCURdb$F22D1_FDB <- ifelse(is.na(UCURdb$F22D1_FDB),(UCURdb$X630/apply(UCURdb[447:457],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAMS_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F22D1_FDB)
UCURdb$F24D1C_FDB <- ifelse(is.na(UCURdb$F24D1C_FDB),(UCURdb$X671/apply(UCURdb[447:457],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAMS_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F24D1C_FDB)
###############################POLY FAT############################
foodlabel <- UCURdb[,c("F18D4_PER_SERVING_G","F21D5_PER_SERVING_G","F22D4_PER_SERVING_G","LA_PER_SERVING_G","F18D3CN3_PER_SERVING_G","F18D3CN6_PER_SERVING_G","ALA_PER_SERVING_G","F20D2CN6_PER_SERVING_G","F20D3_PER_SERVING_G","ARA_PER_SERVING_G","F20D4_PER_SERVING_G","EPA_PER_SERVING_G","F22D5_PER_SERVING_G","DHA_PER_SERVING_G","F183I_PER_SERVING_G","F182I_PER_SERVING_G","F18D2CN6_PER_SERVING_G","F18D2CLA_PER_SERVING_G")]
UCURdb <- subset(UCURdb, select = -c(447:457))
#adds columns to allow for the calculation 
UCURdb$F18D4_for_calc <- ifelse(!is.na(UCURdb$F18D4_PER_SERVING_G),NA,UCURdb$X627)
UCURdb$F21D5_for_calc <- ifelse(!is.na(UCURdb$F21D5_PER_SERVING_G),NA,UCURdb$X857)
UCURdb$F22D4_for_calc <- ifelse(!is.na(UCURdb$F22D4_PER_SERVING_G),NA,UCURdb$X858)
UCURdb$F18D2_for_calc <- ifelse(!is.na(UCURdb$LA_PER_SERVING_G),NA,UCURdb$X618)
UCURdb$F18D3CN3_for_calc <- ifelse(!is.na(UCURdb$F18D3CN3_PER_SERVING_G),NA,UCURdb$X851)
UCURdb$F18D3CN6_for_calc <- ifelse(!is.na(UCURdb$F18D3CN6_PER_SERVING_G),NA,UCURdb$X685)
UCURdb$F18D3_for_calc <- ifelse(!is.na(UCURdb$ALA_PER_SERVING_G),NA,UCURdb$X619)
UCURdb$F20D2CN6_for_calc <- ifelse(!is.na(UCURdb$F20D2CN6_PER_SERVING_G),NA,UCURdb$X672)
UCURdb$F20D3N3_for_calc <- ifelse(!is.na(UCURdb$F20D3N3_PER_SERVING_G),NA,UCURdb$X852)
UCURdb$F20D3N6_for_calc <- ifelse(!is.na(UCURdb$F20D3N6_PER_SERVING_G),NA,UCURdb$X853)
UCURdb$F20D3_for_calc <- ifelse(!is.na(UCURdb$F20D3_PER_SERVING_G),NA,UCURdb$X689)
UCURdb$F20D4N6_for_calc <- ifelse(!is.na(UCURdb$ARA_PER_SERVING_G),NA,UCURdb$X855)
UCURdb$F20D4_for_calc <- ifelse(!is.na(UCURdb$F20D4_PER_SERVING_G),NA,UCURdb$X620)
UCURdb$F20D5_for_calc <- ifelse(!is.na(UCURdb$EPA_PER_SERVING_G),NA,UCURdb$X629)
UCURdb$F22D5_for_calc <- ifelse(!is.na(UCURdb$F22D5_PER_SERVING_G),NA,UCURdb$X631)
UCURdb$F22D6_for_calc <- ifelse(!is.na(UCURdb$DHA_PER_SERVING_G),NA,UCURdb$X621)
UCURdb$F183I_for_calc <- ifelse(!is.na(UCURdb$F183I_PER_SERVING_G),NA,UCURdb$X856)
UCURdb$F182I_for_calc <- ifelse(!is.na(UCURdb$F182I_PER_SERVING_G),NA,UCURdb$X666)
UCURdb$F18D2CN6_for_calc <- ifelse(!is.na(UCURdb$F18D2CN6_PER_SERVING_G),NA,UCURdb$X675)
UCURdb$F18D2CLA_for_calc <- ifelse(!is.na(UCURdb$F18D2CLA_PER_SERVING_G),NA,UCURdb$X670)


#creates the final columns and fills in some of the Na's 
UCURdb$F18D4_FDB <- ifelse(!is.na(UCURdb$F18D4_PER_SERVING_G),UCURdb$F18D4_PER_SERVING_G,ifelse(dataframe$FAPU_FDB == 0, 0,UCURdb$F18D4_PER_SERVING_G))
UCURdb$F21D5_FDB <- ifelse(!is.na(UCURdb$F21D5_PER_SERVING_G),UCURdb$F21D5_PER_SERVING_G,ifelse(dataframe$FAPU_FDB == 0, 0,UCURdb$F21D5_PER_SERVING_G))
UCURdb$F22D4_FDB <- ifelse(!is.na(UCURdb$F22D4_PER_SERVING_G),UCURdb$F22D4_PER_SERVING_G,ifelse(dataframe$FAPU_FDB == 0, 0,UCURdb$F22D4_PER_SERVING_G))
UCURdb$F18D2_FDB <- ifelse(!is.na(UCURdb$LA_PER_SERVING_G),UCURdb$LA_PER_SERVING_G,ifelse(dataframe$FAPU_FDB == 0, 0,UCURdb$LA_PER_SERVING_G))
UCURdb$F18D3CN3_FDB<- ifelse(!is.na(UCURdb$F18D3CN3_PER_SERVING_G),UCURdb$F18D3CN3_PER_SERVING_G,ifelse(dataframe$FAPU_FDB == 0, 0,UCURdb$F18D3CN3_PER_SERVING_G))
UCURdb$F18D3CN6_FDB <- ifelse(!is.na(UCURdb$F18D3CN6_PER_SERVING_G),UCURdb$F18D3CN6_PER_SERVING_G,ifelse(dataframe$FAPU_FDB == 0, 0,UCURdb$F18D3CN6_PER_SERVING_G))
UCURdb$F18D3_FDB <- ifelse(!is.na(UCURdb$ALA_PER_SERVING_G),UCURdb$ALA_PER_SERVING_G,ifelse(dataframe$FAPU_FDB == 0, 0,UCURdb$ALA_PER_SERVING_G))
UCURdb$F20D2CN6_FDB <- ifelse(!is.na(UCURdb$F20D2CN6_PER_SERVING_G),UCURdb$F20D2CN6_PER_SERVING_G,ifelse(dataframe$FAPU_FDB == 0, 0,UCURdb$F20D2CN6_PER_SERVING_G))
UCURdb$F20D3N3_FDB <- ifelse(!is.na(UCURdb$F20D3N3_PER_SERVING_G),UCURdb$F20D3N3_PER_SERVING_G,ifelse(dataframe$FAPU_FDB== 0, 0,UCURdb$F20D3N3_PER_SERVING_G))
UCURdb$F20D3N6_FDB <- ifelse(!is.na(UCURdb$F20D3N6_PER_SERVING_G),UCURdb$F20D3N6_PER_SERVING_G,ifelse(dataframe$FAPU_FDB == 0, 0,UCURdb$F20D3N6_PER_SERVING_G))
UCURdb$F20D3_FDB <- ifelse(!is.na(UCURdb$F20D3_PER_SERVING_G),UCURdb$F20D3_PER_SERVING_G,ifelse(dataframe$FAPU_FDB == 0, 0,UCURdb$F20D3_PER_SERVING_G))
UCURdb$F20D4N6_FDB <- ifelse(!is.na(UCURdb$ARA_PER_SERVING_G),UCURdb$ARA_PER_SERVING_G,ifelse(dataframe$FAPU_FDB == 0, 0,UCURdb$ARA_PER_SERVING_G))
UCURdb$F20D4_FDB <- ifelse(!is.na(UCURdb$F20D4_PER_SERVING_G),UCURdb$F20D4_PER_SERVING_G,ifelse(dataframe$FAPU_FDB == 0, 0,UCURdb$F20D4_PER_SERVING_G))
UCURdb$F20D5_FDB <- ifelse(!is.na(UCURdb$EPA_PER_SERVING_G),UCURdb$EPA_PER_SERVING_G,ifelse(dataframe$FAPU_FDB == 0, 0,UCURdb$EPA_PER_SERVING_G))
UCURdb$F22D5_FDB <- ifelse(!is.na(UCURdb$F22D5_PER_SERVING_G),UCURdb$F22D5_PER_SERVING_G,ifelse(dataframe$FAPU_FDB == 0, 0,UCURdb$F22D5_PER_SERVING_G))
UCURdb$F22D6_FDB <- ifelse(!is.na(UCURdb$DHA_PER_SERVING_G),UCURdb$DHA_PER_SERVING_G,ifelse(dataframe$FAPU_FDB == 0, 0,UCURdb$DHA_PER_SERVING_G))
UCURdb$F183I_FDB <- ifelse(!is.na(UCURdb$F183I_PER_SERVING_G),UCURdb$F183I_PER_SERVING_G,ifelse(dataframe$FAPU_FDB == 0, 0,UCURdb$F183I_PER_SERVING_G))
UCURdb$F182II_FDB <- ifelse(!is.na(UCURdb$F182I_PER_SERVING_G),UCURdb$F182I_PER_SERVING_G,ifelse(dataframe$FAPU_FDB == 0, 0,UCURdb$F182I_PER_SERVING_G))
UCURdb$F18D2CN6_FDB <- ifelse(!is.na(UCURdb$F18D2CN6_PER_SERVING_G),UCURdb$F18D2CN6_PER_SERVING_G,ifelse(dataframe$FAPU_FDB == 0, 0,UCURdb$F18D2CN6_PER_SERVING_G))
UCURdb$F18D2CLA_FDB <- ifelse(!is.na(UCURdb$F18D2CLA_PER_SERVING_G),UCURdb$F18D2CLA_PER_SERVING_G,ifelse(dataframe$FAPU_FDB == 0, 0,UCURdb$F18D2CLA_PER_SERVING_G))

#these lines preform the calculation for the Poly fats
UCURdb$F18D4_FDB <- ifelse(is.na(UCURdb$F18D4_FDB),(UCURdb$X627/apply(UCURdb[458:477],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAPU_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F18D4_FDB)
UCURdb$F21D5_FDB <- ifelse(is.na(UCURdb$F21D5_FDB),(UCURdb$X857/apply(UCURdb[458:477],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAPU_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F21D5_FDB)
UCURdb$F22D4_FDB <- ifelse(is.na(UCURdb$F22D4_FDB),(UCURdb$X858/apply(UCURdb[458:477],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAPU_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F22D4_FDB)
UCURdb$F18D2_FDB <- ifelse(is.na(UCURdb$F18D2_FDB),(UCURdb$X618/apply(UCURdb[458:477],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAPU_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F18D2_FDB)
UCURdb$F18D3CN3_FDB <- ifelse(is.na(UCURdb$F18D3CN3_FDB),(UCURdb$X851/apply(UCURdb[458:477],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAPU_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F18D3CN3_FDB)
UCURdb$F18D3CN6_FDB <- ifelse(is.na(UCURdb$F18D3CN6_FDB),(UCURdb$X685/apply(UCURdb[458:477],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAPU_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F18D3CN6_FDB)
UCURdb$F18D3_FDB <- ifelse(is.na(UCURdb$F18D3_FDB),(UCURdb$X619/apply(UCURdb[458:477],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAPU_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F18D3_FDB)
UCURdb$F20D2CN6_FDB <- ifelse(is.na(UCURdb$F20D2CN6_FDB),(UCURdb$X672/apply(UCURdb[458:477],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAPU_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F20D2CN6_FDB)
UCURdb$F20D3N3_FDB <- ifelse(is.na(UCURdb$F20D3N3_FDB),(UCURdb$X852/apply(UCURdb[458:477],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAPU_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F20D3N3_FDB)
UCURdb$F20D3N6_FDB <- ifelse(is.na(UCURdb$F20D3N6_FDB),(UCURdb$X853/apply(UCURdb[458:477],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAPU_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F20D3N6_FDB)
UCURdb$F20D3_FDB <- ifelse(is.na(UCURdb$F20D3_FDB),(UCURdb$X689/apply(UCURdb[458:477],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAPU_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F20D3_FDB)
UCURdb$F20D4N6_FDB <- ifelse(is.na(UCURdb$F20D4N6_FDB),(UCURdb$X855/apply(UCURdb[458:477],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAPU_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F20D4N6_FDB)
UCURdb$F20D4_FDB <- ifelse(is.na(UCURdb$F20D4_FDB),(UCURdb$X620/apply(UCURdb[458:477],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAPU_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F20D4_FDB)
UCURdb$F20D5_FDB <- ifelse(is.na(UCURdb$F20D5_FDB),(UCURdb$X629/apply(UCURdb[458:477],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAPU_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F20D5_FDB)
UCURdb$F22D5_FDB <- ifelse(is.na(UCURdb$F22D5_FDB),(UCURdb$X631/apply(UCURdb[458:477],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAPU_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F22D5_FDB)
UCURdb$F22D6_FDB <- ifelse(is.na(UCURdb$F22D6_FDB),(UCURdb$X621/apply(UCURdb[458:477],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAPU_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F22D6_FDB)
UCURdb$F183I_FDB <- ifelse(is.na(UCURdb$F183I_FDB),(UCURdb$X856/apply(UCURdb[458:477],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAPU_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F183I_FDB)
UCURdb$F182II_FDB <- ifelse(is.na(UCURdb$F182II_FDB),(UCURdb$X666/apply(UCURdb[458:477],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAPU_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F182II_FDB)
UCURdb$F18D2CN6_FDB <- ifelse(is.na(UCURdb$F18D2CN6_FDB),(UCURdb$X675/apply(UCURdb[458:477],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAPU_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F18D2CN6_FDB)
UCURdb$F18D2CLA_FDB <- ifelse(is.na(UCURdb$F18D2CLA_FDB),(UCURdb$X670/apply(UCURdb[458:477],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FAPU_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F18D2CLA_FDB)
#########################TRANS FAT###################
foodlabel <- UCURdb[,c("F16D1T_PER_SERVING_G","F18D1T_PER_SERVING_G","F18D1TN7_PER_SERVING_G","F18D2X_PER_SERVING_G","F18D2TT_PER_SERVING_G","F22D1T_PER_SERVING_G","FATRNM_PER_SERVING_G","FATRNP_PER_SERVING_G")]
UCURdb <- subset(UCURdb, select = -c(458:477))
#adds columns to allow for the calculation 
UCURdb$F16D1T_for_calc <- ifelse(!is.na(UCURdb$F16D1T_PER_SERVING_G),NA,UCURdb$X662)
UCURdb$F18D1T_for_calc <- ifelse(!is.na(UCURdb$F18D1T_PER_SERVING_G),NA,UCURdb$X663)
UCURdb$F18D1TN7_for_calc <- ifelse(!is.na(UCURdb$F18D1TN7_PER_SERVING_G),NA,UCURdb$X859)
UCURdb$F18D2X_for_calc <- ifelse(!is.na(UCURdb$F18D2X_PER_SERVING_G),NA,UCURdb$X665)
UCURdb$F18D2TT_for_calc <- ifelse(!is.na(UCURdb$F18D2TT_PER_SERVING_G),NA,UCURdb$X669)
UCURdb$F22D1T_for_calc <- ifelse(!is.na(UCURdb$F22D1T_PER_SERVING_G),NA,UCURdb$X664)
UCURdb$FATRNM_for_calc <- ifelse(!is.na(UCURdb$FATRNM_PER_SERVING_G),NA,UCURdb$X693)
UCURdb$FATRNP_for_calc <- ifelse(!is.na(UCURdb$FATRNP_PER_SERVING_G),NA,UCURdb$X695)


#creates the final columns and fills in some of the Na's 
UCURdb$F16D1T_FDB <- ifelse(!is.na(UCURdb$F16D1T_PER_SERVING_G),UCURdb$F16D1T_PER_SERVING_G,ifelse(dataframe$FATRN_FDB == 0, 0,UCURdb$F16D1T_PER_SERVING_G))
UCURdb$F18D1T_FDB <- ifelse(!is.na(UCURdb$F18D1T_PER_SERVING_G),UCURdb$F18D1T_PER_SERVING_G,ifelse(dataframe$FATRN_FDB == 0, 0,UCURdb$F18D1T_PER_SERVING_G))
UCURdb$F18D1TN7_FDB <- ifelse(!is.na(UCURdb$F18D1TN7_PER_SERVING_G),UCURdb$F18D1TN7_PER_SERVING_G,ifelse(dataframe$FATRN_FDB == 0, 0,UCURdb$F18D1TN7_PER_SERVING_G))
UCURdb$F18D2X_FDB <- ifelse(!is.na(UCURdb$F18D2X_PER_SERVING_G),UCURdb$F18D2X_PER_SERVING_G,ifelse(dataframe$FATRN_FDB == 0, 0,UCURdb$F18D2X_PER_SERVING_G))
UCURdb$F18D2TT_FDB <- ifelse(!is.na(UCURdb$F18D2TT_PER_SERVING_G),UCURdb$F18D2TT_PER_SERVING_G,ifelse(dataframe$FATRN_FDB == 0, 0,UCURdb$F18D2TT_PER_SERVING_G))
UCURdb$F22D1T_FDB <- ifelse(!is.na(UCURdb$F22D1T_PER_SERVING_G),UCURdb$F22D1T_PER_SERVING_G,ifelse(dataframe$FATRN_FDB == 0, 0,UCURdb$F22D1T_PER_SERVING_G))
UCURdb$FATRNM_FDB <- ifelse(!is.na(UCURdb$FATRNM_PER_SERVING_G),UCURdb$FATRNM_PER_SERVING_G,ifelse(dataframe$FATRN_FDB == 0, 0,UCURdb$FATRNM_PER_SERVING_G))
UCURdb$FATRNP_FDB <- ifelse(!is.na(UCURdb$FATRNP_PER_SERVING_G),UCURdb$FATRNP_PER_SERVING_G,ifelse(dataframe$FATRN_FDB == 0, 0,UCURdb$FATRNP_PER_SERVING_G))

#these lines preform the calculation for the Trans fats
UCURdb$F16D1T_FDB <- ifelse(is.na(UCURdb$F16D1T_FDB),(UCURdb$X662/apply(UCURdb[470:477],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FATRN_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F16D1T_FDB)
UCURdb$F18D1T_FDB <- ifelse(is.na(UCURdb$F18D1T_FDB),(UCURdb$X663/apply(UCURdb[470:477],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FATRN_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F18D1T_FDB)
UCURdb$F18D1TN7_FDB <- ifelse(is.na(UCURdb$F18D1TN7_FDB),(UCURdb$X859/apply(UCURdb[470:477],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FATRN_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F18D1TN7_FDB)
UCURdb$F18D2X_FDB <- ifelse(is.na(UCURdb$F18D2X_FDB),(UCURdb$X665/apply(UCURdb[470:477],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FATRN_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F18D2X_FDB)
UCURdb$F18D2TT_FDB <- ifelse(is.na(UCURdb$F18D2TT_FDB),(UCURdb$X669/apply(UCURdb[470:477],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FATRN_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F18D2TT_FDB)
UCURdb$F22D1T_FDB <- ifelse(is.na(UCURdb$F22D1T_FDB),(UCURdb$X664/apply(UCURdb[470:477],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FATRN_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$F22D1T_FDB)
UCURdb$FATRNM_FDB <- ifelse(is.na(UCURdb$FATRNM_FDB),(UCURdb$X693/apply(UCURdb[470:477],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FATRN_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FATRNM_FDB)
UCURdb$FATRNP_FDB <- ifelse(is.na(UCURdb$FATRNP_FDB),(UCURdb$X695/apply(UCURdb[470:477],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$FATRN_FDB-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FATRNP_FDB)
#drops the columns that were used for calculation 
UCURdb <- subset(UCURdb, select = -c(470:477))