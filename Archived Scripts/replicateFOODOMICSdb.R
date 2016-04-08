#Replicate foodomics DB

#Replicate years
replicatedFDb <- data.frame(Year=c('1997',
                        '2000',
                        '2001',
                        '2002',
                        '2003',
                        '2004',
                        '2005',
                        '2006',
                        '2007',
                        '2008',
                        '2009',
                        '2010',
                        '2011',
                        '2012',
                        '2013',
                        '2014',
                        '2015'))


each_YEAR_1121_times<-data.frame(replicatedFDb[rep(seq_len(nrow(replicatedFDb)), 1121), ]) #where 1121 is the number of products we have that are unique

#Replicate NDIDs
uniqueNDIDforREP<-unique(UCURdb$New.NDID.Number)
foodomicsDB.expanded <-data.frame(NDID=(uniqueNDIDforREP))
each_NDID_17_times<-data.frame(foodomicsDB.expanded[rep(seq_len(nrow(foodomicsDB.expanded)), each=17),])

#Combine into one DF
final_product<-data.frame(YEAR=each_YEAR_1121_times,NDID=each_NDID_17_times)

final_product$pasteNAME<-paste(final_product[,1],final_product[,2], sep="_")


#Now, we can merge in the actual foodomics data, but only the years with real calculated info will fill in, and we will have to use time series methods to fill in the rest...

#First, create a YEAR_NDID entry for each foodomic dataset from the UCURdb

#Second, merge the data together
missing_data_foodomics<-merge()