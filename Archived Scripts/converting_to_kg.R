
##########################
#####PLAY WITH RESULTS####
##########################
foodomics_run<-result_daily_summed
###########GRAPHING VARIOUS FOODOMIC PARAMETERS
library(ggplot2)
library(reshape2)
library(ggrepel)
pufa<-c(71:90)
mufa<-c(60:70)
sfa<-c(46:59)
tfa<-c(91:96)
cho<-c(17:28)
cho_g<-c(8,11,12,13,14,15,16,17,18)
pro<-c(19:37)

main fat =14
main pro=13
main cho = 15

#CALORIE PROPORTION CALCULATIONS
macro_cals<-foodomics_run$FNA_PROCNT*4 + foodomics_run$FNA_FAT*9 + foodomics_run$FNA_CHOCDF*4
foodomics_run$Protein<-foodomics_run$FNA_PROCNT*4/macro_cals
foodomics_run$Fat<-foodomics_run$FNA_FAT*9/macro_cals
foodomics_run$Carbohydrate<-foodomics_run$FNA_CHOCDF*4/macro_cals
cals <- c(164:166)  

#plotting overall thingies
foodomics_run$Count<-1:length(foodomics_run$PRODUCTNDID)

#plotting categories
library(data.table)
foodomics_runtest<-foodomics_run[!(foodomics_run$Protein==0 & foodomics_run$Carbohydrate==0 & foodomics_run$Fat==0),]
foodomics_runtest<-foodomics_run[!(is.na(foodomics_run$Protein) & is.na(foodomics_run$Carbohydrate==0) & is.na(foodomics_run$Fat==0)),]
write.csv(foodomics_runtest, file="foodomics_runtest_food_profiles.csv")

DT <- data.table(foodomics_runtest)

foodomics_runtest <-DT[, id := seq_len(.N), by = Category_1]
foodomics_runtest<-as.data.frame(foodomics_runtest)

#plotting CALORIES AND MACRONUTRIENTS
#remove any foods where all the values are 0
UCURmeltOTHER_CALS <- melt(foodomics_runtest[,c(4, cals, 187)] ,  id.vars = c('id','Category_1'), variable.name = 'Chemical')
UCURmeltOTHER_CALS$id<-as.numeric(UCURmeltOTHER_CALS$id)
UCURmeltOTHER_CALS$value<-round(as.numeric(UCURmeltOTHER_CALS$value), digits=3)
ggplot(UCURmeltOTHER_CALS, aes(id,value, fill=Chemical)) + 
  geom_bar(position="stack",stat="identity") + 
  facet_wrap(~Category_1, ncol = 4, scales="free") +
  theme_bw(base_size = 16) + 
  theme(strip.text.x = element_text(size = 15, colour = "black"), plot.title = element_text(size=20, face="bold", 
                                                                                            margin = margin(10, 0, 10, 0))) +
  ggtitle("Macronutrient Profile of Foodomic Database") +
  ylab("Percent") +
  xlab("Number of Foods")


##########PLOTTING FATTY ACID PROFILE OF FOODS IN THE DATABASE

foodomics_run[, c(mufa, pufa, sfa, tfa)]<-foodomics_run[, c(mufa, pufa, sfa, tfa)]/foodomics_run[,14]


foodomics$TOT_AA<-rowSums(foodomics[, c(pro)],na.rm=TRUE)
foodomics$TOT_FA<-rowSums(foodomics[, c(mufa, pufa, sfa, tfa)],na.rm=TRUE)
foodomics$TOT_CHO<-rowSums(foodomics[, c(cho_g)],na.rm=TRUE)

foodomics[, c(pro)]<-foodomics[, c(pro)]/ foodomics$TOT_AA
foodomics<-foodomics[!(is.na(foodomics$TOT_AA)),]
foodomics<-foodomics[!(foodomics$TOT_AA==0),]


foodomics[, c(mufa, pufa, sfa, tfa)]<-foodomics[, c(mufa, pufa, sfa, tfa)]/ foodomics$TOT_FA
foodomics<-foodomics[!(is.na(foodomics$TOT_FA)),]
foodomics<-foodomics[!(foodomics$TOT_FA==0),]

foodomics[, c(cho_g)]<-foodomics[, c(cho_g)]/ foodomics$TOT_CHO
foodomics<-foodomics[!(is.na(foodomics$TOT_CHO)),]
foodomics<-foodomics[!(foodomics$TOT_CHO==0),]



write.csv(foodomics, file="foodomics_runtest_food_profiles_forfatpro3.csv")



####Amino ACIDS######################################
library(plotly)
#19 colors

amino_acids<-c("#6766D2","#6CDD41","#DC3E32","#3FBFB4","#D8398F","#5E722D","#D0B936","#4F8DB9","#5EDB8C","#B144D5","#9D552A","#C64F5B","#96C452","#9D7CBF","#B35A8C","#CEE340","#DF7230","#C98F31","#44933C")

library(data.table)
DT <- data.table(foodomics)
foodomics <-DT[, id := seq_len(.N), by = Category_1]
foodomics<-as.data.frame(foodomics)

UCURmeltOTHER_CALS <- melt(foodomics[,c(4, pro, 181)] ,  id.vars = c('id','Category_1'), variable.name = 'Chemical')
library(ggplot2)
ggplot(UCURmeltOTHER_CALS, aes(id,value, fill=Chemical, width=1)) + 
  geom_bar(position="stack",stat="identity") + 
  facet_wrap(~Category_1, ncol = 4, scales="free") +
  theme_bw(base_size = 16) + 
  theme(strip.text.x = element_text(size = 15, colour = "black"), plot.title = element_text(size=20, face="bold", 
                                                                                            margin = margin(10, 0, 10, 0))) +
  ggtitle("Amino Acid Profile of Foodomic Database") +
  ylab("Proportion") +
  xlab("Number of Foods") +
  scale_fill_manual(values=amino_acids) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        panel.grid=element_blank())


ggplotly()
#####################################################


####FATTY  ACIDS

#51 colors

fatty_acids<-c("#F7645E",
               "#27EC56",
               "#8753FF",
               "#61DFFB",
               "#1F3506",
               "#430B4D",
               "#FFE111",
               "#FCB7EF",
               "#E7F3B3",
               "#086873",
               "#1ACF90",
               "#4289CF",
               "#693318",
               "#223B99",
               "#FDBA85",
               "#A589FE",
               "#F45A90",
               "#49069B",
               "#3E0D20",
               "#696C0F",
               "#122D53",
               "#F6BEBB",
               "#D84415",
               "#ADB923",
               "#9B1C66",
               "#128D63",
               "#3DB4B1",
               "#D4EBDA",
               "#F9064C",
               "#F48442",
               "#A2FE5D",
               "#A1CEFE",
               "#F081C5",
               "#B02B3B",
               "#A3A6F2",
               "#9103BC",
               "#F68483",
               "#36DEBD",
               "#153524",
               "#204C86",
               "#F2FD99",
               "#781C1E",
               "#00DDDD",
               "#74F9A3",
               "#033547",
               "#3CD61F",
               "#610B64",
               "#4204B1",
               "#4C5002",
               "#FD9D89",
               "#41F3C9")

library(data.table)
DT <- data.table(foodomics)
foodomics <-DT[, id := seq_len(.N), by = Category_1]
foodomics<-as.data.frame(foodomics)
#foodomics4<-subset(foodomics, Category_1=="Fats_and_Oils")
#foodomics4<-foodomics4[,c(1:8,29:47)]
UCURmeltOTHER_CALS <- melt(foodomics[,c(4, c(mufa, pufa, sfa, tfa), 181)] ,  id.vars = c('id','Category_1'), variable.name = 'Chemical')
library(ggplot2)
ggplot(UCURmeltOTHER_CALS, aes(id,value, fill=Chemical, width=1)) + 
  geom_bar(position="stack",stat="identity") + 
  facet_wrap(~Category_1, ncol = 4, scales="free") +
  theme_bw(base_size = 16) + 
  theme(strip.text.x = element_text(size = 15, colour = "black"), plot.title = element_text(size=20, face="bold", 
                                                                                            margin = margin(10, 0, 10, 0))) +
  ggtitle("Fatty Acid Profile of Foodomic Database") +
  ylab("Proportion") +
  xlab("Number of Foods") +
  scale_fill_manual(values=fatty_acids) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        panel.grid=element_blank())

ggplotly()

#######################CARBOHYDRATE

cho_acids<-c("#F7645E",
             "#27EC56",
             "#8753FF",
             "#61DFFB",
             "#1F3506",
             "#430B4D",
             "#FFE111",
             "#41F3C9")

library(data.table)
DT <- data.table(foodomics)
foodomics <-DT[, id := seq_len(.N), by = Category_1]
foodomics<-as.data.frame(foodomics)

UCURmeltOTHER_CALS <- melt(foodomics[,c(4, c(cho_g), 181)] ,  id.vars = c('id','Category_1'), variable.name = 'Chemical')
library(ggplot2)
ggplot(UCURmeltOTHER_CALS, aes(id,value, fill=Chemical, width=1)) + 
  geom_bar(position="stack",stat="identity") + 
  facet_wrap(~Category_1, ncol = 4, scales="free") +
  theme_bw(base_size = 16) + 
  theme(strip.text.x = element_text(size = 15, colour = "black"), plot.title = element_text(size=20, face="bold", 
                                                                                            margin = margin(10, 0, 10, 0))) +
  ggtitle("Carbohydrate Profile of Foodomic Database") +
  ylab("Proportion") +
  xlab("Number of Foods") +
  scale_fill_manual(values=cho_acids) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        panel.grid=element_blank())

######################

ggplot(UCURmeltOTHER_CALS, aes(x=as.numeric(Count),y=as.numeric(value))) + 
  geom_point(aes(color=factor(Category_1))) +
  theme_bw(base_size = 16) + 
  facet_wrap(~Chemical, ncol = 10)

#plotting other variables
UCURmeltOTHER <- melt(foodomics_run[,c(4, pufa, 184)] ,  id.vars = c('Count','Category_1'), variable.name = 'Chemical')
UCURmeltOTHER$Count<-as.numeric(UCURmeltOTHER$Count)
UCURmeltOTHER$value<-as.numeric(UCURmeltOTHER$value)
ggplot(UCURmeltOTHER, aes(Count,value, color=factor(Category_1))) + geom_point() + facet_wrap(~Chemical, ncol = 10)
ggplot(UCURmeltOTHER, aes(x=as.numeric(Count),y=as.numeric(value))) + 
  geom_point(aes(color=factor(Category_1))) +
  theme_bw(base_size = 16) + 
  facet_wrap(~Chemical, ncol = 10)


#get rid of entries that are identical to plot individual parameters with labels
unDUPfoodomics<-unique(foodomics_run[,-c(2,3,5,6,7,8,9,10,11,180)])
unDUPfoodomics_run<-unDUPfoodomics
unDUPfoodomics_run$Count<-1:length(unDUPfoodomics$PRODUCTNDID)

UCURmeltOTHER <- melt(unDUPfoodomics_run[,c(1,2, pufa, 171)] ,  id.vars = c('Count','Category_1', 'PRODUCTNDID'), variable.name = 'Chemical')
UCURmeltOTHER$Count<-as.numeric(UCURmeltOTHER$Count)
UCURmeltOTHER$value<-as.numeric(UCURmeltOTHER$value)

#plotting just one variable with labels
UCURmeltOTHER <- melt(foodomics_run[,c(1,4, pufa, 180)] ,  id.vars = c('id','Category_1', 'PRODUCTNDID'), variable.name = 'Chemical')
UCURmeltOTHERFNA<-subset(UCURmeltOTHER, UCURmeltOTHER$Chemical=="FNA_F18D2")
ggplot(UCURmeltOTHERFNA, aes(x=as.numeric(id),y=as.numeric(value))) + 
  geom_point(aes(color=factor(Category_1))) +
  theme_bw(base_size = 16) +
  geom_text_repel(
    data=subset(UCURmeltOTHER,value>30),
    aes(label=PRODUCTNDID),
    fontface = 'bold', color = 'black'
  )


#Obtain outliers
outliers = boxplot(foodomics_run$FNA_GLU_G)$out
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