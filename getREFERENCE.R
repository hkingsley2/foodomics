#Average the data in preSCALING PER PRODUCT except where values are NA then just use value
#First get rid of columns that we don't need
preSCALING2<-preSCALING[,c(2,7:156)]
library(magrittr)
library(dplyr)
referenceBASEP<- preSCALING2%>% group_by(New.NDID.Number) %>% summarise_each(funs(mean))

saveRDS(referenceBASEP, file="reference_BASEP_based_on_12162015_compiledDB.rds")




#STATS
referenceBASEP_CHO_stats <- lapply( referenceBASEP[,c("X221","X291","X213","X214","X212","X287","X211","X209","X210","X269")] , function(x) rbind( mean = mean(!is.na(x)) ,
                                                              sd = sd(!is.na(x)) ,
                                                              median = median(!is.na(x)) ,
                                                              minimum = min(!is.na(x)) ,
                                                              maximum = max(!is.na(x)) ,
                                                              s.size = length(!is.na(x)) ) )

referenceBASEP_CHO_stats<- data.frame( referenceBASEP_CHO_stats )