#######################################################
###Manipulating Patient Data to get it ready to view###
#######################################################

setwd("Z:/MySQL Database/Anthros/Database_Ready/Test Patients/Interpolated data for foodomics")

#Fixing dates
weights<-read.csv(file="KG0222.txt", sep="\t", header=TRUE, stringsAsFactors = FALSE)
names(weights)[names(weights) == 'Date'] <- 'Group.date'
weights$Group.date<-as.POSIXlt(weights$Group.date,format="%m/%d/%y")
weights$Group.date<-as.Date(weights$Group.date)
food_and_weight<-merge(result_daily_summed, weights, by=c("Group.date"))

#Doing division
food_and_weight[,-c(1,42:45,164:165)]=as.data.frame(apply(food_and_weight[,-c(1,42:45,164:165)], 2, function(x) x / food_and_weight$WT))

food_and_weight_clean<-food_and_weight[,-c(42:45,164:165)]


parameters<-names(food_and_weight_clean)

#PLOT every nutrient

library(ggplot2)

# Make list of variable names to loop over.
parameter_list = parameters

# Make plots.
plot_list = list()
for (i in 2:94) {
  foodomics_one_var<-food_and_weight_clean[, c(1,i)] 
  
  foodomics_boxes <- melt(foodomics_one_var ,  id.vars = c('Group.date'), variable.name = 'Chemical')
  foodomics_boxes$value<-as.numeric(foodomics_boxes$value)
  library(ggplot2)
  
  p<-ggplot(foodomics_boxes, aes(x=Group.date, y=value, color=Chemical)) + 
    geom_line() +
    ggtitle(parameter_list[[i]])+
    ylab("Unit/kg") + 
    xlab("Date")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +guides(colour=FALSE)
  
  
  plot_list[[i]] = p 
  
}

# Another option: create pdf where each page is a separate plot.

pdf("plots_scaled.pdf")
for (i in 2:94) {
  print(plot_list[[i]])
}
dev.off()



#######################################################
###STOP RUNNING THE CODE ABOVE THIS LINE###
#######################################################


foodomics_fat<-food_and_weight_clean[, c(1,4,42:92)] 

foodomics_boxes <- melt(foodomics_fat ,  id.vars = c('Group.date'), variable.name = 'Chemical')
foodomics_boxes$value<-as.numeric(foodomics_boxes$value)
library(ggplot2)

ggplot(foodomics_boxes, aes(x=Group.date, y=value, color=Chemical)) + 
  geom_line() 