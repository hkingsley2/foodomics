##LOADS DATA INTO DATABASE
load data local infile 'G:/MySQL Database/Diet/CONVERTING_NFD_TO_LABEL_SOURCE.csv'
	into table LABEL_SOURCE
	fields terminated by ','
	enclosed by '"'
	lines terminated by '\n'
	ignore 1 lines
	;
    