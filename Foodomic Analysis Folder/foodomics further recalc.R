#currently running first 142 lines of getADJNFL.R and all of foodomics recalc 


#creates a dataframe to simplify code later
foodlabel <- UCURdb[,c("F4D0_per_serving_g","F6D0_per_serving_g","F8D0_per_serving_g","F10D0_per_serving_g","F12D0_per_serving_g","F13D0_per_serving_g","F14D0_per_serving_g","F15D0_per_serving_g","F16D0_per_serving_g","F17D0_per_serving_g","F18D0_per_serving_g","F20D0_per_serving_g","F22D0_per_serving_g","F24D0_per_serving_g")]

#########################SATURATED FAT#############################
#adds columns to allow for the calculation 
UCURdb$F4D02_for_calc <- ifelse(!is.na(UCURdb$F4D0_per_serving_g),NA,UCURdb$X607)
UCURdb$F6D02_for_calc <- ifelse(!is.na(UCURdb$F6D0_per_serving_g),NA,UCURdb$X608)
UCURdb$F8D02_for_calc <- ifelse(!is.na(UCURdb$F8D0_per_serving_g),NA,UCURdb$X609)
UCURdb$F10D02_for_calc <- ifelse(!is.na(UCURdb$F10D0_per_serving_g),NA,UCURdb$X610)
UCURdb$F12D02_for_calc <- ifelse(!is.na(UCURdb$F12D0_per_serving_g),NA,UCURdb$X611)
UCURdb$F13D02_for_calc <- ifelse(!is.na(UCURdb$F13D0_per_serving_g),NA,UCURdb$X696)
UCURdb$F14D02_for_calc <- ifelse(!is.na(UCURdb$F14D0_per_serving_g),NA,UCURdb$X612)
UCURdb$F15D02_for_calc <- ifelse(!is.na(UCURdb$F15D0_per_serving_g),NA,UCURdb$X652)
UCURdb$F16D02_for_calc <- ifelse(!is.na(UCURdb$F16D0_per_serving_g),NA,UCURdb$X613)
UCURdb$F17D02_for_calc <- ifelse(!is.na(UCURdb$F17D0_per_serving_g),NA,UCURdb$X653)
UCURdb$F18D02_for_calc <- ifelse(!is.na(UCURdb$F18D0_per_serving_g),NA,UCURdb$X614)
UCURdb$F20D02_for_calc <- ifelse(!is.na(UCURdb$F20D0_per_serving_g),NA,UCURdb$X615)
UCURdb$F22D02_for_calc <- ifelse(!is.na(UCURdb$F22D0_per_serving_g),NA,UCURdb$X624)
UCURdb$F24D02_for_calc <- ifelse(!is.na(UCURdb$F24D0_per_serving_g),NA,UCURdb$X654)


#creates the final columns and fills in some of the Na's 
UCURdb$FNA_F4D0 <- ifelse(!is.na(UCURdb$F4D0_per_serving_g),UCURdb$F4D0_per_serving_g,ifelse(dataframe$calc_saturatedfat == 0, 0,UCURdb$F4D0_per_serving_g))
UCURdb$FNA_F6D0 <- ifelse(!is.na(UCURdb$F6D0_per_serving_g),UCURdb$F6D0_per_serving_g,ifelse(dataframe$calc_saturatedfat == 0, 0,UCURdb$F6D0_per_serving_g))
UCURdb$FNA_F8D0 <- ifelse(!is.na(UCURdb$F8D0_per_serving_g),UCURdb$F8D0_per_serving_g,ifelse(dataframe$calc_saturatedfat == 0, 0,UCURdb$F8D0_per_serving_g))
UCURdb$FNA_F10D0 <- ifelse(!is.na(UCURdb$F10D0_per_serving_g),UCURdb$F10D0_per_serving_g,ifelse(dataframe$calc_saturatedfat == 0, 0,UCURdb$F10D0_per_serving_g))
UCURdb$FNA_F12D0 <- ifelse(!is.na(UCURdb$F12D0_per_serving_g),UCURdb$F12D0_per_serving_g,ifelse(dataframe$calc_saturatedfat == 0, 0,UCURdb$F12D0_per_serving_g))
UCURdb$FNA_F13D0 <- ifelse(!is.na(UCURdb$F13D0_per_serving_g),UCURdb$F13D0_per_serving_g,ifelse(dataframe$calc_saturatedfat == 0, 0,UCURdb$F13D0_per_serving_g))
UCURdb$FNA_F14D0 <- ifelse(!is.na(UCURdb$F14D0_per_serving_g),UCURdb$F14D0_per_serving_g,ifelse(dataframe$calc_saturatedfat == 0, 0,UCURdb$F14D0_per_serving_g))
UCURdb$FNA_F15D0 <- ifelse(!is.na(UCURdb$F15D0_per_serving_g),UCURdb$F15D0_per_serving_g,ifelse(dataframe$calc_saturatedfat == 0, 0,UCURdb$F15D0_per_serving_g))
UCURdb$FNA_F16D0 <- ifelse(!is.na(UCURdb$F16D0_per_serving_g),UCURdb$F16D0_per_serving_g,ifelse(dataframe$calc_saturatedfat== 0, 0,UCURdb$F16D0_per_serving_g))
UCURdb$FNA_F17D0 <- ifelse(!is.na(UCURdb$F17D0_per_serving_g),UCURdb$F17D0_per_serving_g,ifelse(dataframe$calc_saturatedfat == 0, 0,UCURdb$F17D0_per_serving_g))
UCURdb$FNA_F18D0 <- ifelse(!is.na(UCURdb$F18D0_per_serving_g),UCURdb$F18D0_per_serving_g,ifelse(dataframe$calc_saturatedfat == 0, 0,UCURdb$F18D0_per_serving_g))
UCURdb$FNA_F20D0 <- ifelse(!is.na(UCURdb$F20D0_per_serving_g),UCURdb$F20D0_per_serving_g,ifelse(dataframe$calc_saturatedfat == 0, 0,UCURdb$F20D0_per_serving_g))
UCURdb$FNA_F22D0 <- ifelse(!is.na(UCURdb$F22D0_per_serving_g),UCURdb$F22D0_per_serving_g,ifelse(dataframe$calc_saturatedfat == 0, 0,UCURdb$F22D0_per_serving_g))
UCURdb$FNA_F24D0 <- ifelse(!is.na(UCURdb$F24D0_per_serving_g),UCURdb$F24D0_per_serving_g,ifelse(dataframe$calc_saturatedfat == 0, 0,UCURdb$F24D0_per_serving_g))


#these lines preform the calculation for the saturated fats
UCURdb$FNA_F4D0 <- ifelse(is.na(UCURdb$FNA_F4D0),(UCURdb$X607/apply(UCURdb[443:456],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_saturatedfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F4D0)
UCURdb$FNA_F6D0 <- ifelse(is.na(UCURdb$FNA_F6D0),(UCURdb$X608/apply(UCURdb[443:456],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_saturatedfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F6D0)
UCURdb$FNA_F8D0 <- ifelse(is.na(UCURdb$FNA_F8D0),(UCURdb$X609/apply(UCURdb[443:456],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_saturatedfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F8D0)
UCURdb$FNA_F10D0 <- ifelse(is.na(UCURdb$FNA_F10D0),(UCURdb$X610/apply(UCURdb[443:456],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_saturatedfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F10D0)
UCURdb$FNA_F12D0 <- ifelse(is.na(UCURdb$FNA_F12D0),(UCURdb$X611/apply(UCURdb[443:456],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_saturatedfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F12D0)
UCURdb$FNA_F13D0 <- ifelse(is.na(UCURdb$FNA_F13D0),(UCURdb$X696/apply(UCURdb[443:456],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_saturatedfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F13D0)
UCURdb$FNA_F14D0 <- ifelse(is.na(UCURdb$FNA_F14D0),(UCURdb$X612/apply(UCURdb[443:456],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_saturatedfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F14D0)
UCURdb$FNA_F15D0 <- ifelse(is.na(UCURdb$FNA_F15D0),(UCURdb$X652/apply(UCURdb[443:456],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_saturatedfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F15D0)
UCURdb$FNA_F16D0 <- ifelse(is.na(UCURdb$FNA_F16D0),(UCURdb$X613/apply(UCURdb[443:456],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_saturatedfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F16D0)
UCURdb$FNA_F17D0 <- ifelse(is.na(UCURdb$FNA_F17D0),(UCURdb$X653/apply(UCURdb[443:456],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_saturatedfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F17D0)
UCURdb$FNA_F18D0 <- ifelse(is.na(UCURdb$FNA_F18D0),(UCURdb$X614/apply(UCURdb[443:456],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_saturatedfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F18D0)
UCURdb$FNA_F20D0 <- ifelse(is.na(UCURdb$FNA_F20D0),(UCURdb$X615/apply(UCURdb[443:456],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_saturatedfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F20D0)
UCURdb$FNA_F22D0 <- ifelse(is.na(UCURdb$FNA_F22D0),(UCURdb$X624/apply(UCURdb[443:456],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_saturatedfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F22D0)
UCURdb$FNA_F24D0 <- ifelse(is.na(UCURdb$FNA_F24D0),(UCURdb$X654/apply(UCURdb[443:456],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_saturatedfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F24D0)


########################MONO FAT#######################################
foodlabel <- UCURdb[,c("F14D1_per_serving_g","F15D1_per_serving_g","F17D1_per_serving_g","F20D1_per_serving_g","F16D1C_per_serving_g","F16D1_per_serving_g","F18D1C_per_serving_g","F18D1_per_serving_g","F22D1C_per_serving_g","F22D1_per_serving_g","F24D1C_per_serving_g")]
UCURdb <- subset(UCURdb, select = -c(443:456))
#adds columns to allow for the calculation 
UCURdb$F14D1_for_calc <- ifelse(!is.na(UCURdb$F14D1_per_serving_g),NA,UCURdb$X625)
UCURdb$F15D1_for_calc <- ifelse(!is.na(UCURdb$F15D1_per_serving_g),NA,UCURdb$X697)
UCURdb$F17D1_for_calc <- ifelse(!is.na(UCURdb$F17D1_per_serving_g),NA,UCURdb$X687)
UCURdb$F20D1_for_calc <- ifelse(!is.na(UCURdb$F20D1_per_serving_g),NA,UCURdb$X628)
UCURdb$F16D1C_for_calc <- ifelse(!is.na(UCURdb$F16D1C_per_serving_g),NA,UCURdb$X673)
UCURdb$F16D1_for_calc <- ifelse(!is.na(UCURdb$F16D1_per_serving_g),NA,UCURdb$X626)
UCURdb$F18D1C_for_calc <- ifelse(!is.na(UCURdb$F18D1C_per_serving_g),NA,UCURdb$X674)
UCURdb$F18D1_for_calc <- ifelse(!is.na(UCURdb$F18D1_per_serving_g),NA,UCURdb$X617)
UCURdb$F22D1C_for_calc <- ifelse(!is.na(UCURdb$F22D1C_per_serving_g),NA,UCURdb$X676)
UCURdb$F22D1_for_calc <- ifelse(!is.na(UCURdb$F22D1_per_serving_g),NA,UCURdb$X630)
UCURdb$F24D1C_for_calc <- ifelse(!is.na(UCURdb$F24D1C_per_serving_g),NA,UCURdb$X671)


#creates the final columns and fills in some of the Na's 
UCURdb$FNA_F14D1 <- ifelse(!is.na(UCURdb$F14D1_per_serving_g),UCURdb$F14D1_per_serving_g,ifelse(dataframe$calc_monofat == 0, 0,UCURdb$F14D1_per_serving_g))
UCURdb$FNA_F15D1 <- ifelse(!is.na(UCURdb$F15D1_per_serving_g),UCURdb$F15D1_per_serving_g,ifelse(dataframe$calc_monofat == 0, 0,UCURdb$F15D1_per_serving_g))
UCURdb$FNA_F17D1 <- ifelse(!is.na(UCURdb$F17D1_per_serving_g),UCURdb$F17D1_per_serving_g,ifelse(dataframe$calc_monofat == 0, 0,UCURdb$F17D1_per_serving_g))
UCURdb$FNA_F20D1 <- ifelse(!is.na(UCURdb$F20D1_per_serving_g),UCURdb$F20D1_per_serving_g,ifelse(dataframe$calc_monofat == 0, 0,UCURdb$F20D1_per_serving_g))
UCURdb$FNA_F16D1C <- ifelse(!is.na(UCURdb$F16D1C_per_serving_g),UCURdb$F16D1C_per_serving_g,ifelse(dataframe$calc_monofat == 0, 0,UCURdb$F16D1C_per_serving_g))
UCURdb$FNA_F16D1 <- ifelse(!is.na(UCURdb$F16D1_per_serving_g),UCURdb$F16D1_per_serving_g,ifelse(dataframe$calc_monofat == 0, 0,UCURdb$F16D1_per_serving_g))
UCURdb$FNA_F18D1C <- ifelse(!is.na(UCURdb$F18D1C_per_serving_g),UCURdb$F18D1C_per_serving_g,ifelse(dataframe$calc_monofat == 0, 0,UCURdb$F18D1C_per_serving_g))
UCURdb$FNA_F18D1 <- ifelse(!is.na(UCURdb$F18D1_per_serving_g),UCURdb$F18D1_per_serving_g,ifelse(dataframe$calc_monofat == 0, 0,UCURdb$F18D1_per_serving_g))
UCURdb$FNA_F22D1C <- ifelse(!is.na(UCURdb$F22D1C_per_serving_g),UCURdb$F22D1C_per_serving_g,ifelse(dataframe$calc_monofat== 0, 0,UCURdb$F22D1C_per_serving_g))
UCURdb$FNA_F22D1 <- ifelse(!is.na(UCURdb$F22D1_per_serving_g),UCURdb$F22D1_per_serving_g,ifelse(dataframe$calc_monofat == 0, 0,UCURdb$F22D1_per_serving_g))
UCURdb$FNA_F24D1C <- ifelse(!is.na(UCURdb$F24D1C_per_serving_g),UCURdb$F24D1C_per_serving_g,ifelse(dataframe$calc_monofat == 0, 0,UCURdb$F24D1C_per_serving_g))

#these lines preform the calculation for the saturated fats
UCURdb$FNA_F14D1 <- ifelse(is.na(UCURdb$FNA_F14D1),(UCURdb$X625/apply(UCURdb[457:467],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_monofat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F14D1)
UCURdb$FNA_F15D1 <- ifelse(is.na(UCURdb$FNA_F15D1),(UCURdb$X697/apply(UCURdb[457:467],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_monofat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F15D1)
UCURdb$FNA_F17D1 <- ifelse(is.na(UCURdb$FNA_F17D1),(UCURdb$X687/apply(UCURdb[457:467],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_monofat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F17D1)
UCURdb$FNA_F20D1 <- ifelse(is.na(UCURdb$FNA_F20D1),(UCURdb$X628/apply(UCURdb[457:467],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_monofat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F20D1)
UCURdb$FNA_F16D1C <- ifelse(is.na(UCURdb$FNA_F16D1C),(UCURdb$X673/apply(UCURdb[457:467],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_monofat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F16D1C)
UCURdb$FNA_F16D1 <- ifelse(is.na(UCURdb$FNA_F16D1),(UCURdb$X626/apply(UCURdb[457:467],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_monofat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F16D1)
UCURdb$FNA_F18D1C <- ifelse(is.na(UCURdb$FNA_F18D1C),(UCURdb$X674/apply(UCURdb[457:467],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_monofat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F18D1C)
UCURdb$FNA_F18D1 <- ifelse(is.na(UCURdb$FNA_F18D1),(UCURdb$X617/apply(UCURdb[457:467],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_monofat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F18D1)
UCURdb$FNA_F22D1C <- ifelse(is.na(UCURdb$FNA_F22D1C),(UCURdb$X676/apply(UCURdb[457:467],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_monofat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F22D1C)
UCURdb$FNA_F22D1 <- ifelse(is.na(UCURdb$FNA_F22D1),(UCURdb$X630/apply(UCURdb[457:467],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_monofat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F22D1)
UCURdb$FNA_F24D1C <- ifelse(is.na(UCURdb$FNA_F24D1C),(UCURdb$X671/apply(UCURdb[457:467],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_monofat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F24D1C)
###############################POLY FAT############################
foodlabel <- UCURdb[,c("F18D4_per_serving_g","F21D5_per_serving_g","F22D4_per_serving_g","LA_per_serving_g","F18D3CN3_per_serving_g","F18D3CN6_per_serving_g","ALA_per_serving_g","F20D2CN6_per_serving_g","F20D3_per_serving_g","ARA_per_serving_g","F20D4_per_serving_g","EPA_per_serving_g","F22D5_per_serving_g","DHA_per_serving_g","F183I_per_serving_g","F182I_per_serving_g","F18D2CN6_per_serving_g","F18D2CLA_per_serving_g")]
UCURdb <- subset(UCURdb, select = -c(457:467))
#adds columns to allow for the calculation 
UCURdb$F18D4_for_calc <- ifelse(!is.na(UCURdb$F18D4_per_serving_g),NA,UCURdb$X627)
UCURdb$F21D5_for_calc <- ifelse(!is.na(UCURdb$F21D5_per_serving_g),NA,UCURdb$X857)
UCURdb$F22D4_for_calc <- ifelse(!is.na(UCURdb$F22D4_per_serving_g),NA,UCURdb$X858)
UCURdb$F18D2_for_calc <- ifelse(!is.na(UCURdb$LA_per_serving_g),NA,UCURdb$X618)
UCURdb$F18D3CN3_for_calc <- ifelse(!is.na(UCURdb$F18D3CN3_per_serving_g),NA,UCURdb$X851)
UCURdb$F18D3CN6_for_calc <- ifelse(!is.na(UCURdb$F18D3CN6_per_serving_g),NA,UCURdb$X685)
UCURdb$F18D3_for_calc <- ifelse(!is.na(UCURdb$ALA_per_serving_g),NA,UCURdb$X619)
UCURdb$F20D2CN6_for_calc <- ifelse(!is.na(UCURdb$F20D2CN6_per_serving_g),NA,UCURdb$X672)
UCURdb$F20D3N3_for_calc <- ifelse(!is.na(UCURdb$F20D3N3_per_serving_g),NA,UCURdb$X852)
UCURdb$F20D3N6_for_calc <- ifelse(!is.na(UCURdb$F20D3N6_per_serving_g),NA,UCURdb$X853)
UCURdb$F20D3_for_calc <- ifelse(!is.na(UCURdb$F20D3_per_serving_g),NA,UCURdb$X689)
UCURdb$F20D4N6_for_calc <- ifelse(!is.na(UCURdb$ARA_per_serving_g),NA,UCURdb$X855)
UCURdb$F20D4_for_calc <- ifelse(!is.na(UCURdb$F20D4_per_serving_g),NA,UCURdb$X620)
UCURdb$F20D5_for_calc <- ifelse(!is.na(UCURdb$EPA_per_serving_g),NA,UCURdb$X629)
UCURdb$F22D5_for_calc <- ifelse(!is.na(UCURdb$F22D5_per_serving_g),NA,UCURdb$X631)
UCURdb$F22D6_for_calc <- ifelse(!is.na(UCURdb$DHA_per_serving_g),NA,UCURdb$X621)
UCURdb$F183I_for_calc <- ifelse(!is.na(UCURdb$F183I_per_serving_g),NA,UCURdb$X856)
UCURdb$F182I_for_calc <- ifelse(!is.na(UCURdb$F182I_per_serving_g),NA,UCURdb$X666)
UCURdb$F18D2CN6_for_calc <- ifelse(!is.na(UCURdb$F18D2CN6_per_serving_g),NA,UCURdb$X675)
UCURdb$F18D2CLA_for_calc <- ifelse(!is.na(UCURdb$F18D2CLA_per_serving_g),NA,UCURdb$X670)


#creates the final columns and fills in some of the Na's 
UCURdb$FNA_F18D4 <- ifelse(!is.na(UCURdb$F18D4_per_serving_g),UCURdb$F18D4_per_serving_g,ifelse(dataframe$calc_polyfat == 0, 0,UCURdb$F18D4_per_serving_g))
UCURdb$FNA_F21D5 <- ifelse(!is.na(UCURdb$F21D5_per_serving_g),UCURdb$F21D5_per_serving_g,ifelse(dataframe$calc_polyfat == 0, 0,UCURdb$F21D5_per_serving_g))
UCURdb$FNA_F22D4 <- ifelse(!is.na(UCURdb$F22D4_per_serving_g),UCURdb$F22D4_per_serving_g,ifelse(dataframe$calc_polyfat == 0, 0,UCURdb$F22D4_per_serving_g))
UCURdb$FNA_F18D2 <- ifelse(!is.na(UCURdb$LA_per_serving_g),UCURdb$LA_per_serving_g,ifelse(dataframe$calc_polyfat == 0, 0,UCURdb$LA_per_serving_g))
UCURdb$FNA_F18D3CN3<- ifelse(!is.na(UCURdb$F18D3CN3_per_serving_g),UCURdb$F18D3CN3_per_serving_g,ifelse(dataframe$calc_polyfat == 0, 0,UCURdb$F18D3CN3_per_serving_g))
UCURdb$FNA_F18D3CN6 <- ifelse(!is.na(UCURdb$F18D3CN6_per_serving_g),UCURdb$F18D3CN6_per_serving_g,ifelse(dataframe$calc_polyfat == 0, 0,UCURdb$F18D3CN6_per_serving_g))
UCURdb$FNA_F18D3 <- ifelse(!is.na(UCURdb$ALA_per_serving_g),UCURdb$ALA_per_serving_g,ifelse(dataframe$calc_polyfat == 0, 0,UCURdb$ALA_per_serving_g))
UCURdb$FNA_F20D2CN6 <- ifelse(!is.na(UCURdb$F20D2CN6_per_serving_g),UCURdb$F20D2CN6_per_serving_g,ifelse(dataframe$calc_polyfat == 0, 0,UCURdb$F20D2CN6_per_serving_g))
UCURdb$FNA_F20D3N3 <- ifelse(!is.na(UCURdb$F20D3N3_per_serving_g),UCURdb$F20D3N3_per_serving_g,ifelse(dataframe$calc_polyfat== 0, 0,UCURdb$F20D3N3_per_serving_g))
UCURdb$FNA_F20D3N6 <- ifelse(!is.na(UCURdb$F20D3N6_per_serving_g),UCURdb$F20D3N6_per_serving_g,ifelse(dataframe$calc_polyfat == 0, 0,UCURdb$F20D3N6_per_serving_g))
UCURdb$FNA_F20D3 <- ifelse(!is.na(UCURdb$F20D3_per_serving_g),UCURdb$F20D3_per_serving_g,ifelse(dataframe$calc_polyfat == 0, 0,UCURdb$F20D3_per_serving_g))
UCURdb$FNA_F20D4N6 <- ifelse(!is.na(UCURdb$ARA_per_serving_g),UCURdb$ARA_per_serving_g,ifelse(dataframe$calc_polyfat == 0, 0,UCURdb$ARA_per_serving_g))
UCURdb$FNA_F20D4 <- ifelse(!is.na(UCURdb$F20D4_per_serving_g),UCURdb$F20D4_per_serving_g,ifelse(dataframe$calc_polyfat == 0, 0,UCURdb$F20D4_per_serving_g))
UCURdb$FNA_F20D5 <- ifelse(!is.na(UCURdb$EPA_per_serving_g),UCURdb$EPA_per_serving_g,ifelse(dataframe$calc_polyfat == 0, 0,UCURdb$EPA_per_serving_g))
UCURdb$FNA_F22D5 <- ifelse(!is.na(UCURdb$F22D5_per_serving_g),UCURdb$F22D5_per_serving_g,ifelse(dataframe$calc_polyfat == 0, 0,UCURdb$F22D5_per_serving_g))
UCURdb$FNA_F22D6 <- ifelse(!is.na(UCURdb$DHA_per_serving_g),UCURdb$DHA_per_serving_g,ifelse(dataframe$calc_polyfat == 0, 0,UCURdb$DHA_per_serving_g))
UCURdb$FNA_F183I <- ifelse(!is.na(UCURdb$F183I_per_serving_g),UCURdb$F183I_per_serving_g,ifelse(dataframe$calc_polyfat == 0, 0,UCURdb$F183I_per_serving_g))
UCURdb$FNA_F182I <- ifelse(!is.na(UCURdb$F182I_per_serving_g),UCURdb$F182I_per_serving_g,ifelse(dataframe$calc_polyfat == 0, 0,UCURdb$F182I_per_serving_g))
UCURdb$FNA_F18D2CN6 <- ifelse(!is.na(UCURdb$F18D2CN6_per_serving_g),UCURdb$F18D2CN6_per_serving_g,ifelse(dataframe$calc_polyfat == 0, 0,UCURdb$F18D2CN6_per_serving_g))
UCURdb$FNA_F18D2CLA <- ifelse(!is.na(UCURdb$F18D2CLA_per_serving_g),UCURdb$F18D2CLA_per_serving_g,ifelse(dataframe$calc_polyfat == 0, 0,UCURdb$F18D2CLA_per_serving_g))

#these lines preform the calculation for the saturated fats
UCURdb$FNA_F18D4 <- ifelse(is.na(UCURdb$FNA_F18D4),(UCURdb$X627/apply(UCURdb[468:487],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_polyfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F18D4)
UCURdb$FNA_F21D5 <- ifelse(is.na(UCURdb$FNA_F21D5),(UCURdb$X857/apply(UCURdb[468:487],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_polyfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F21D5)
UCURdb$FNA_F22D4 <- ifelse(is.na(UCURdb$FNA_F22D4),(UCURdb$X858/apply(UCURdb[468:487],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_polyfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F22D4)
UCURdb$FNA_F18D2 <- ifelse(is.na(UCURdb$FNA_F18D2),(UCURdb$X618/apply(UCURdb[468:487],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_polyfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F18D2)
UCURdb$FNA_F18D3CN3 <- ifelse(is.na(UCURdb$FNA_F18D3CN3),(UCURdb$X851/apply(UCURdb[468:487],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_polyfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F18D3CN3)
UCURdb$FNA_F18D3CN6 <- ifelse(is.na(UCURdb$FNA_F18D3CN6),(UCURdb$X685/apply(UCURdb[468:487],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_polyfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F18D3CN6)
UCURdb$FNA_F18D3 <- ifelse(is.na(UCURdb$FNA_F18D3),(UCURdb$X619/apply(UCURdb[468:487],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_polyfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F18D3)
UCURdb$FNA_F20D2CN6 <- ifelse(is.na(UCURdb$FNA_F20D2CN6),(UCURdb$X672/apply(UCURdb[468:487],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_polyfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F20D2CN6)
UCURdb$FNA_F20D3N3 <- ifelse(is.na(UCURdb$FNA_F20D3N3),(UCURdb$X852/apply(UCURdb[468:487],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_polyfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F20D3N3)
UCURdb$FNA_F20D3N6 <- ifelse(is.na(UCURdb$FNA_F20D3N6),(UCURdb$X853/apply(UCURdb[468:487],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_polyfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F20D3N6)
UCURdb$FNA_F20D3 <- ifelse(is.na(UCURdb$FNA_F20D3),(UCURdb$X689/apply(UCURdb[468:487],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_polyfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F20D3)
UCURdb$FNA_F20D4N6 <- ifelse(is.na(UCURdb$FNA_F20D4N6),(UCURdb$X855/apply(UCURdb[468:487],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_polyfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F20D4N6)
UCURdb$FNA_F20D4 <- ifelse(is.na(UCURdb$FNA_F20D4),(UCURdb$X620/apply(UCURdb[468:487],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_polyfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F20D4)
UCURdb$FNA_F20D5 <- ifelse(is.na(UCURdb$FNA_F20D5),(UCURdb$X629/apply(UCURdb[468:487],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_polyfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F20D5)
UCURdb$FNA_F22D5 <- ifelse(is.na(UCURdb$FNA_F22D5),(UCURdb$X631/apply(UCURdb[468:487],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_polyfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F22D5)
UCURdb$FNA_F22D6 <- ifelse(is.na(UCURdb$FNA_F22D6),(UCURdb$X621/apply(UCURdb[468:487],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_polyfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F22D6)
UCURdb$FNA_F183I <- ifelse(is.na(UCURdb$FNA_F183I),(UCURdb$X856/apply(UCURdb[468:487],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_polyfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F183I)
UCURdb$FNA_F182I <- ifelse(is.na(UCURdb$FNA_F182I),(UCURdb$X666/apply(UCURdb[468:487],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_polyfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F182I)
UCURdb$FNA_F18D2CN6 <- ifelse(is.na(UCURdb$FNA_F18D2CN6),(UCURdb$X675/apply(UCURdb[468:487],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_polyfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F18D2CN6)
UCURdb$FNA_F18D2CLA <- ifelse(is.na(UCURdb$FNA_F18D2CLA),(UCURdb$X670/apply(UCURdb[468:487],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_polyfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F18D2CLA)
#########################TRANS FAT###################
foodlabel <- UCURdb[,c("F16D1T_per_serving_g","F18D1T_per_serving_g","F18D1TN7_per_serving_g","F18D2X_per_serving_g","F18D2TT_per_serving_g","F22D1T_per_serving_g","FATRNM_per_serving_g","FATRNP_per_serving_g")]
UCURdb <- subset(UCURdb, select = -c(468:487))
#adds columns to allow for the calculation 
UCURdb$F16D1T_for_calc <- ifelse(!is.na(UCURdb$F16D1T_per_serving_g),NA,UCURdb$X662)
UCURdb$F18D1T_for_calc <- ifelse(!is.na(UCURdb$F18D1T_per_serving_g),NA,UCURdb$X663)
UCURdb$F18D1TN7_for_calc <- ifelse(!is.na(UCURdb$F18D1TN7_per_serving_g),NA,UCURdb$X859)
UCURdb$F18D2X_for_calc <- ifelse(!is.na(UCURdb$F18D2X_per_serving_g),NA,UCURdb$X665)
UCURdb$F18D2TT_for_calc <- ifelse(!is.na(UCURdb$F18D2TT_per_serving_g),NA,UCURdb$X669)
UCURdb$F22D1T_for_calc <- ifelse(!is.na(UCURdb$F22D1T_per_serving_g),NA,UCURdb$X664)
UCURdb$FATRNM_for_calc <- ifelse(!is.na(UCURdb$FATRNM_per_serving_g),NA,UCURdb$X693)
UCURdb$FATRNP_for_calc <- ifelse(!is.na(UCURdb$FATRNP_per_serving_g),NA,UCURdb$X695)


#creates the final columns and fills in some of the Na's 
UCURdb$FNA_F16D1T <- ifelse(!is.na(UCURdb$F16D1T_per_serving_g),UCURdb$F16D1T_per_serving_g,ifelse(dataframe$calc_transfat == 0, 0,UCURdb$F16D1T_per_serving_g))
UCURdb$FNA_F18D1T <- ifelse(!is.na(UCURdb$F18D1T_per_serving_g),UCURdb$F18D1T_per_serving_g,ifelse(dataframe$calc_transfat == 0, 0,UCURdb$F18D1T_per_serving_g))
UCURdb$FNA_F18D1TN7 <- ifelse(!is.na(UCURdb$F18D1TN7_per_serving_g),UCURdb$F18D1TN7_per_serving_g,ifelse(dataframe$calc_transfat == 0, 0,UCURdb$F18D1TN7_per_serving_g))
UCURdb$FNA_F18D2X <- ifelse(!is.na(UCURdb$F18D2X_per_serving_g),UCURdb$F18D2X_per_serving_g,ifelse(dataframe$calc_transfat == 0, 0,UCURdb$F18D2X_per_serving_g))
UCURdb$FNA_F18D2TT <- ifelse(!is.na(UCURdb$F18D2TT_per_serving_g),UCURdb$F18D2TT_per_serving_g,ifelse(dataframe$calc_transfat == 0, 0,UCURdb$F18D2TT_per_serving_g))
UCURdb$FNA_F22D1T <- ifelse(!is.na(UCURdb$F22D1T_per_serving_g),UCURdb$F22D1T_per_serving_g,ifelse(dataframe$calc_transfat == 0, 0,UCURdb$F22D1T_per_serving_g))
UCURdb$FNA_FATRNM <- ifelse(!is.na(UCURdb$FATRNM_per_serving_g),UCURdb$FATRNM_per_serving_g,ifelse(dataframe$calc_transfat == 0, 0,UCURdb$FATRNM_per_serving_g))
UCURdb$FNA_FATRNP <- ifelse(!is.na(UCURdb$FATRNP_per_serving_g),UCURdb$FATRNP_per_serving_g,ifelse(dataframe$calc_transfat == 0, 0,UCURdb$FATRNP_per_serving_g))

#these lines preform the calculation for the saturated fats
UCURdb$FNA_F16D1T <- ifelse(is.na(UCURdb$FNA_F16D1T),(UCURdb$X662/apply(UCURdb[488:495],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_transfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F16D1T)
UCURdb$FNA_F18D1T <- ifelse(is.na(UCURdb$FNA_F18D1T),(UCURdb$X663/apply(UCURdb[488:495],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_transfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F18D1T)
UCURdb$FNA_F18D1TN7 <- ifelse(is.na(UCURdb$FNA_F18D1TN7),(UCURdb$X859/apply(UCURdb[488:495],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_transfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F18D1TN7)
UCURdb$FNA_F18D2X <- ifelse(is.na(UCURdb$FNA_F18D2X),(UCURdb$X665/apply(UCURdb[488:495],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_transfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F18D2X)
UCURdb$FNA_F18D2TT <- ifelse(is.na(UCURdb$FNA_F18D2TT),(UCURdb$X669/apply(UCURdb[488:495],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_transfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F18D2TT)
UCURdb$FNA_F22D1T <- ifelse(is.na(UCURdb$FNA_F22D1T),(UCURdb$X664/apply(UCURdb[488:495],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_transfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_F22D1T)
UCURdb$FNA_FATRNM <- ifelse(is.na(UCURdb$FNA_FATRNM),(UCURdb$X693/apply(UCURdb[488:495],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_transfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_FATRNM)
UCURdb$FNA_FATRNP <- ifelse(is.na(UCURdb$FNA_FATRNP),(UCURdb$X695/apply(UCURdb[488:495],1, function(x) sum(x,na.rm=TRUE)))*(dataframe$calc_transfat-apply(foodlabel,1,function(X) sum(X,na.rm=TRUE))),UCURdb$FNA_FATRNP)
 
UCURdb <- subset(UCURdb, select = -c(488:495))