#getQUALITY.R
#Looks at the quality of the Brand Name Food Database

#(1)
#Import the files and do some cleaning up
library("reshape")
cat<-read.csv(file="NDID_cat1_year.txt", sep="\t", header=TRUE)
cat<-cat[,c(2:3)]
cat<-unique(cat)
compiledNFD<- read.csv(file = "TRANSLATING_BNFD_TO_R_DB.txt", header = TRUE, sep="\t", na.strings=c("NA","NaN", " ", "N/A", ""),stringsAsFactors=FALSE)
cat$PRODUCTNDID<-as.character(cat$PRODUCTNDID)

#(2) 
#Rename the Product NDID column
names(compiledNFD)[names(compiledNFD)=="NDID"] <- "PRODUCTNDID"

#(3)
#Get only REAL PRODUCTS THAT ARE NOT DUPLICATED
wo_duplicates_orname<-unique(compiledNFD[,-c(1:3,5:9,13,100:114)])
wo_duplicates_orname2<-merge(wo_duplicates_orname,cat, by=c("PRODUCTNDID"), drop=TRUE)

#(4)
#Subset to just a USDA category
subset<-subset(wo_duplicates_orname2[,c(1:15,18,24,223)], wo_duplicates_orname2$Category_1=="Sweets")
subset[,c(2:18)] <- sapply(subset[,c(2:18)], as.numeric)
subsetMelt <- melt(subset,  id.vars = c('PRODUCTNDID', 'Category_1'))

#(5)
#Now create subsets for specific variables
subsetMelt_Calssvg<-subsetMelt[subsetMelt$variable=="Carbohydrate_per_serving_g",]
subsetMelt_Calssvg$value<-as.numeric(subsetMelt_Calssvg$value)

#(6)
#Plot the data
library(ggplot2)

#for outliers
ggplot(subsetMelt_Calssvg, aes(x=PRODUCTNDID,y=value)) + geom_boxplot(outlier.colour="red", outlier.shape=8)  +
  theme(axis.text.x = element_text(angle=90, vjust=.8, hjust=0.8)) + ggtitle("Carbohydrate per Serving in BNFD")






#for fun
ggplot(subsetMelt_CHO, aes(x = NDID, y = value, group=NDID, color=NDID), label = subsetMelt$Product_Name) + geom_boxplot()










#How many unique brand name products are in the BNFD
length(unique(compiledNFD2$PRODUCTNDID))

#Macronutrients

#What are summary stats for Total Protein per 100g of the product
library(dplyr)
library(ggplot2)
library(reshape)
compiledNFD2<-as.data.frame(compiledNFD2[,c(1:15)])
subset<-subset(compiledNFD2[,c(1:15)], compiledNFD2$Category_1=="Baby_Foods")

subsetMelt <- melt(subset,  id.vars = c('PRODUCTNDID','Year', 'Count', 'Category_1', 'Product_Name'))

ggplot(subsetMelt, aes(x = Category_1, y = value, group=Chemical, color=Chemical), label = subsetMelt$Product_Name) + geom_boxplot()



outliers<-compiledNFD2[is_outlier(!is.na(grams_Pro_per_100g)),]

compiledNFD2 %>%
  mutate(outlier = ifelse(is_outlier(!is.na(compiledNFD2$grams_Pro_per_100g)), !is.na(compiledNFD2$grams_Pro_per_100g), as.numeric(NA))) %>%
  ggplot(., aes(x = Category_1, y = grams_Pro_per_100g)) +
  geom_boxplot() +
  geom_text(data=outliers, aes(label=ifelse((!is.na(compiledNFD2$grams_Pro_per_100g))>1.5*IQR(!is.na(compiledNFD2$grams_Pro_per_100g)),Product_Name,"")), hjust=1.1)

summary(compiledNFD2$grams_Pro_per_100g)




###########GRAPHING VARIOUS FOODOMIC PARAMETERS
library(ggplot2)
library(reshape2)
pufa<-c(449:464)
mufa<-c(438:448)
sfa<-c(424:437)
tfa<-c(465:476)
cho<-c(385:396)
pro<-c(397:415)

UCURmeltOTHER <- melt(UCURdb[,c(2,sfa)] ,  id.vars = 'Count', variable.name = 'Chemical')
UCURmeltOTHER$Count<-as.numeric(UCURmeltOTHER$Count)
UCURmeltOTHER$value<-as.numeric(UCURmeltOTHER$value)
ggplot(UCURmeltOTHER, aes(x=Count,value)) + geom_point() + facet_wrap(~Chemical, ncol = 10)

#Obtain outliers
outliers = boxplot(UCURdb$FNA_F8D0)$out
UCURdb[UCURdb$FNA_F8D0 %in% outliers,]



####Add in Category 1
cat<-read.csv(file="NDID_cat1_year.txt", sep="\t", header=TRUE)
foodomics2<-merge(cat, UCURdb, by=c("PRODUCTNDID", "Year"))

subset<-subset(foodomics2[,c(4:5,sfa)], foodomics2$Category_1=="Sauces_Soups_Condiments_Dressing")
UCURmeltOTHER <- melt(subset,  id.vars = 'Count', variable.name = 'Chemical')
UCURmeltOTHER$Count<-as.numeric(UCURmeltOTHER$Count)
UCURmeltOTHER$value<-as.numeric(UCURmeltOTHER$value)
UCURmeltOTHER2<-merge(foodomics2[,c("Count", "Product_Name")])
ggplot(UCURmeltOTHER2, aes(Count,value), label = UCURmeltOTHER2$Product_Name) + geom_point() + facet_wrap(~variable, ncol = 10) 






subsetMelt$Year<-as.numeric(subsetMelt$Year)
subsetMeltcho<-subset(subsetMelt, subsetMelt$variable=="grams_Pro_per_100g")
ggplot(subsetMeltcho, aes(x="",y=value, color=factor(Year))) + geom_point() + facet_wrap(~PRODUCTNDID, ncol = 30) +
  theme(axis.text.x = element_text(angle=90, hjust = 1)) + ggtitle("Baby Food Protein Percentage in BNFD") + theme(strip.text = element_text(size=6))
ggsave(file="0132016_Baby_Food_Pro_2.png")

subsetMelt$Year<-as.numeric(subsetMelt$Year)
subsetMeltcho<-subset(subsetMelt, subsetMelt$variable=="grams_Cho_per_100g")
ggplot(subsetMeltcho, aes(x="",y=value, color=factor(Year))) + geom_point() + facet_wrap(~PRODUCTNDID, ncol = 30) +
  theme(axis.text.x = element_text(angle=90, hjust = 1)) + ggtitle("Baby Food Carbohydate Percentage in BNFD") + theme(strip.text = element_text(size=6))
ggsave(file="0132016_Baby_Food_Cho_2.png")

subsetMelt$Year<-as.numeric(subsetMelt$Year)
subsetMeltcho<-subset(subsetMelt, subsetMelt$variable=="grams_Fat_per_100g")
ggplot(subsetMeltcho, aes(x="",y=value, color=factor(Year))) + geom_point() + facet_wrap(~PRODUCTNDID, ncol = 30) +
  theme(axis.text.x = element_text(angle=90, hjust = 1)) + ggtitle("Baby Food Fat Percentage in BNFD") + theme(strip.text = element_text(size=6))
ggsave(file="0132016_Baby_Food_Fat_2.png")



subsetMelt$Year<-as.numeric(subsetMelt$Year)
subsetMeltcho<-subset(subsetMelt, subsetMelt$variable=="grams_Pro_per_100g")
ggplot(subsetMeltcho, aes(x=PRODUCTNDID,y=value)) + geom_boxplot(outlier.colour="red", outlier.shape=8)  +
  theme(axis.text.x = element_text(angle=90, hjust = .1)) + ggtitle("Baby Food Pro Percentage in BNFD")
ggsave(file="0132016_Baby_Food_Pro.png")


subsetMelt$Year<-as.numeric(subsetMelt$Year)
subsetMeltcho<-subset(subsetMelt, subsetMelt$variable=="Calories_per_serving_kcal")
ggplot(subsetMeltcho, aes(x=PRODUCTNDID,y=value)) + geom_boxplot(outlier.colour="red", outlier.shape=8)  +
  theme(axis.text.x = element_text(angle=90, vjust=.8, hjust=0.8)) + ggtitle("Baby Food Calories_per_serving_kcal in BNFD")
ggsave(file="0132016_Baby_Calories_per_serving_kcal.png")




subsetMelt$PRODUCTNDIDYEAR<-paste0(subsetMelt$PRODUCTNDID,subsetMelt$Year, sep="_")
c <- ggplot(subsetMelt, aes(x = PRODUCTNDIDYEAR, y = value, fill = variable))
c + geom_bar(stat = "identity")


######Trying to Figure out how to calculate some column summary stats - these will be applied over all subsets of the database (categories)


is_outlier <- function(x) {
  x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x)
}

library(plyr)
Baby_food_macro_stats <- ddply(subsetMeltcho, c("PRODUCTNDID"), summarise, 
mean = mean(!is.na(value)) ,
sd = sd(!is.na(value)) ,
median = median(!is.na(value)) ,
minimum = min(!is.na(value)) ,
maximum = max(!is.na(value)) ,
s.size = length(!is.na(value)),
lower.bound = quantile(!is.na(value), 0.25) * IQR(!is.na(value)),
upper.bound = quantile(!is.na(value), 0.75) * IQR(!is.na(value))
) 


