#getADJUSDA.r

#This script adjusts seed profiles from the USDA table in the foodomics database.
#Profile data compiled in the USDA National Nutrient database may contain NA 
#values for sub-class parameters when the measured level of the class is 0. In this case, 
#a value of 0 is given to all sub-class nutrients when the measured level of the class is 0. 

#Calculate Adjusted Carbohydate, remembering that every serving is 100 grams
USDA$CHObyCAL <- ((((USDA$`X208`/100)* 100) - (((USDA$`X204`/100)*100)*9) - (((USDA$`X203`/100)*100)*4))/4)
USDA$CHObyWEIGHT <- ifelse(USDA$`X205`=="0", 0, ((USDA$`X205`/100)*100))
USDA$CHOdec<- ifelse(USDA$CHObyCAL>USDA$CHObyWEIGHT,USDA$CHObyCAL,USDA$CHObyWEIGHT)
USDA$CHOdec2<- ifelse(USDA$CHOdec>100,100,USDA$CHOdec)

################
#CHO CONVERSIONS
################
#IF CARBOHYDRATE IS ZERO, THEN DATA FOR CARBOHYDRATES SHOULD ALSO BE ZERO
USDA$`X221`<-ifelse(USDA$CHOdec2=="0",0,USDA$`X221`)
USDA$`X291`<-ifelse(USDA$CHOdec2=="0",0,USDA$`X291`)
USDA$`X213`<-ifelse(USDA$CHOdec2=="0",0,USDA$`X213`)
USDA$`X214`<-ifelse(USDA$CHOdec2=="0",0,USDA$`X214`)
USDA$`X212`<-ifelse(USDA$CHOdec2=="0",0,USDA$`X212`)
USDA$`X287`<-ifelse(USDA$CHOdec2=="0",0,USDA$`X287`)
USDA$`X211`<-ifelse(USDA$CHOdec2=="0",0,USDA$`X211`)
USDA$`X209`<-ifelse(USDA$CHOdec2=="0",0,USDA$`X209`)
USDA$`X210`<-ifelse(USDA$CHOdec2=="0",0,USDA$`X210`)
USDA$`X269`<-ifelse(USDA$CHOdec2=="0",0,USDA$`X269`)


################
#PRO CONVERSIONS
################
#IF PROTEIN IS ZERO, THEN DATA FOR AMINO ACIDS SHOULD ALSO BE ZERO
USDA$`X513`<-ifelse(USDA$`X203`=="0",0,USDA$`X513`)
USDA$`X511`<-ifelse(USDA$`X203`=="0",0,USDA$`X511`)
USDA$`X514`<-ifelse(USDA$`X203`=="0",0,USDA$`X514`)
USDA$`X507`<-ifelse(USDA$`X203`=="0",0,USDA$`X507`)
USDA$`X515`<-ifelse(USDA$`X203`=="0",0,USDA$`X515`)
USDA$`X516`<-ifelse(USDA$`X203`=="0",0,USDA$`X516`)
USDA$`X512`<-ifelse(USDA$`X203`=="0",0,USDA$`X512`)
USDA$`X521`<-ifelse(USDA$`X203`=="0",0,USDA$`X521`)
USDA$`X503`<-ifelse(USDA$`X203`=="0",0,USDA$`X503`)
USDA$`X504`<-ifelse(USDA$`X203`=="0",0,USDA$`X504`)
USDA$`X505`<-ifelse(USDA$`X203`=="0",0,USDA$`X505`)
USDA$`X506`<-ifelse(USDA$`X203`=="0",0,USDA$`X506`)
USDA$`X508`<-ifelse(USDA$`X203`=="0",0,USDA$`X508`)
USDA$`X517`<-ifelse(USDA$`X203`=="0",0,USDA$`X517`)
USDA$`X518`<-ifelse(USDA$`X203`=="0",0,USDA$`X518`)
USDA$`X502`<-ifelse(USDA$`X203`=="0",0,USDA$`X502`)
USDA$`X501`<-ifelse(USDA$`X203`=="0",0,USDA$`X501`)
USDA$`X509`<-ifelse(USDA$`X203`=="0",0,USDA$`X509`)
USDA$`X510`<-ifelse(USDA$`X203`=="0",0,USDA$`X510`)

################
#FAT CONVERSIONS
################
#IF FAT IS ZERO, THEN DATA FOR FAT SUB-CLASSES SHOULD ALSO BE ZERO
USDA$`X645`<-ifelse(USDA$`X204`=="0",0,USDA$`X645`)
USDA$`X646`<-ifelse(USDA$`X204`=="0",0,USDA$`X646`)
USDA$`X606`<-ifelse(USDA$`X204`=="0",0,USDA$`X606`)
USDA$`X605`<-ifelse(USDA$`X204`=="0",0,USDA$`X605`)

#IF FAT IS ZERO, THEN DATA FOR FAT SUB-CLASSES SHOULD ALSO BE ZERO
#IF FAT IS NOT ZERO, BUT DATA FOR FAT-SUBCLASSES IS ZERO, THEN DATA FOR FATTY ACIDS SHOULD ALSO BE ZERO
USDA$`X662`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X605`=="0",0,USDA$`X662`))
USDA$`X663`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X605`=="0",0,USDA$`X663`))
USDA$`X859`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X605`=="0",0,USDA$`X859`))
USDA$`X670`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X605`=="0",0,USDA$`X670`))
USDA$`X666`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X605`=="0",0,USDA$`X666`))
USDA$`X675`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X605`=="0",0,USDA$`X675`))
USDA$`X665`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X605`=="0",0,USDA$`X665`))
USDA$`X669`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X605`=="0",0,USDA$`X669`))
USDA$`X856`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X605`=="0",0,USDA$`X856`))
USDA$`X664`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X605`=="0",0,USDA$`X664`))
USDA$`X693`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X605`=="0",0,USDA$`X693`))
USDA$`X695`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X605`=="0",0,USDA$`X695`))




USDA$`X627`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X646`=="0",0,USDA$`X627`))
USDA$`X857`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X646`=="0",0,USDA$`X857`))
USDA$`X858`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X646`=="0",0,USDA$`X858`))
USDA$`X618`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X646`=="0",0,USDA$`X618`))
USDA$`X851`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X646`=="0",0,USDA$`X851`))
USDA$`X685`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X646`=="0",0,USDA$`X685`))
USDA$`X619`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X646`=="0",0,USDA$`X619`))
USDA$`X672`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X646`=="0",0,USDA$`X672`))
USDA$`X852`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X646`=="0",0,USDA$`X852`))
USDA$`X853`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X646`=="0",0,USDA$`X853`))
USDA$`X689`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X646`=="0",0,USDA$`X689`))
USDA$`X855`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X646`=="0",0,USDA$`X855`))
USDA$`X620`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X646`=="0",0,USDA$`X620`))
USDA$`X629`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X646`=="0",0,USDA$`X629`))
USDA$`X631`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X646`=="0",0,USDA$`X631`))
USDA$`X621`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X646`=="0",0,USDA$`X621`))




USDA$`X625`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X645`=="0",0,USDA$`X625`))
USDA$`X697`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X645`=="0",0,USDA$`X697`))
USDA$`X687`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X645`=="0",0,USDA$`X687`))
USDA$`X628`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X645`=="0",0,USDA$`X628`))
USDA$`X673`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X645`=="0",0,USDA$`X673`))
USDA$`X626`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X645`=="0",0,USDA$`X626`))
USDA$`X674`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X645`=="0",0,USDA$`X674`))
USDA$`X617`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X645`=="0",0,USDA$`X617`))
USDA$`X676`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X645`=="0",0,USDA$`X676`))
USDA$`X630`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X645`=="0",0,USDA$`X630`))
USDA$`X671`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X645`=="0",0,USDA$`X671`))




USDA$`X607`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X606`=="0",0,USDA$`X607`))
USDA$`X608`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X606`=="0",0,USDA$`X608`))
USDA$`X609`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X606`=="0",0,USDA$`X609`))
USDA$`X610`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X606`=="0",0,USDA$`X610`))
USDA$`X611`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X606`=="0",0,USDA$`X611`))
USDA$`X696`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X606`=="0",0,USDA$`X696`))
USDA$`X612`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X606`=="0",0,USDA$`X612`))
USDA$`X652`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X606`=="0",0,USDA$`X652`))
USDA$`X613`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X606`=="0",0,USDA$`X613`))
USDA$`X653`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X606`=="0",0,USDA$`X653`))
USDA$`X614`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X606`=="0",0,USDA$`X614`))
USDA$`X615`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X606`=="0",0,USDA$`X615`))
USDA$`X624`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X606`=="0",0,USDA$`X624`))
USDA$`X654`<-ifelse(USDA$`X204`=="0",0,ifelse(USDA$`X606`=="0",0,USDA$`X654`))


