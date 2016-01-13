#getQUALITY.R
#Looks at the quality of the Brand Name Food Database
cat<-read.csv(file="NDID_cat1_year.txt", sep="\t", header=TRUE)
names(compiledNFD)[names(compiledNFD)=="NDID"] <- "PRODUCTNDID"
compiledNFD2<-merge(cat, compiledNFD, by=c("PRODUCTNDID", "Year"))

#How many unique brand name products are in the BNFD
length(unique(compiledNFD$NDID))

#Macronutrients

#What are summary stats for Total Protein per 100g of the product
library(dplyr)
library(ggplot2)

compiledNFD2<-as.data.frame(compiledNFD2[,c(1:8)])
subset<-subset(compiledNFD2[,c(1:8)], compiledNFD2$Category_1=="Baby_Foods")

subsetMelt <- melt(subset,  id.vars = c('PRODUCTNDID','Year', 'Count', 'Category_1', 'Product_Name'), variable.name = 'Chemical')
ggplot(subsetMelt, aes(x = Category_1, y = value, group=Chemical, color=Chemical), label = subsetMelt$Product_Name) + geom_boxplot()



is_outlier <- function(x) {
  x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x)
}

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
subsetMeltcho<-subset(subsetMelt, subsetMelt$Chemical=="grams_Cho_per_100g")
ggplot(subsetMeltcho, aes(x="",y=value, color=factor(Year))) + geom_point() + facet_wrap(~PRODUCTNDID, ncol = 30) +
  theme(axis.text.x = element_text(angle=90)) + ggtitle("Baby Food Carbohydate Percentage in BNFD")




subsetMelt$Year<-as.numeric(subsetMelt$Year)
subsetMeltcho<-subset(subsetMelt, subsetMelt$Chemical=="grams_Fat_per_100g")
ggplot(subsetMeltcho, aes(x=PRODUCTNDID,y=value)) + geom_boxplot(outlier.colour="red", outlier.shape=8)  +
  theme(axis.text.x = element_text(angle=90)) + ggtitle("Baby Food Fat Percentage in BNFD")

