mooshIntake.R

menus<-dataset
daily_intakes<-dataset

library(data.table)
dt <- data.table(menus) #after this the date is mm/dd/yy 12:00 AM
test<-dt[, number := 1:.N, by = c("MRNUMBER","PKT_Recipe_Number")] #this numbers every row in the dt by date
data<-as.data.frame(test) #this converts the dt to a dataframe


#Make Menus in Wide Format
menus_ID<-menus[,c("MRNUMBER","PKT_Recipe_Number","Date.of.Change","Ingredient_ID")]
menus_Amt<-menus[,c("MRNUMBER","PKT_Recipe_Number","Date.of.Change","PKT_Recipe_Ingredient_Amount")]
menus_wide_ID <- reshape(menus_ID, direction="wide", idvar = c("MRNUMBER","PKT_Recipe_Number","Date.of.Change"), timevar = "Ingredient_ID")
menus_wide_amt <- reshape(menus_Amt, direction="wide", idvar = c("MRNUMBER","PKT_Recipe_Number","Date.of.Change"), timevar = "PKT_Recipe_Ingredient_Amount")