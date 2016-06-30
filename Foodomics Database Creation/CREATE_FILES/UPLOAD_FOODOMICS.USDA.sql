
#Load Table Functions

load data local infile 'C:/Users/dlennon/Documents/GitHub/foodomics/Foodomics Database Creation/Source Data/SR28_PROFILE_DATA.csv'
	into table USDA
	fields terminated by ','
	enclosed by '"'
	lines terminated by '\n'
	ignore 1 lines
	;