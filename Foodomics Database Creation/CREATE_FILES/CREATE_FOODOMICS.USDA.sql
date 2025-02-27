####Creates MAKE_MEALS Dataset

#1) Download USDA database
#2) Query for foods and nutrient values and save as .xlsx file, then as csv
#3) Replace missing values by the word NULL
#4) Upload data into this table

#Create Table Functions
use borum_practice;
create table USDA(
NDB_No	VARCHAR (10),
Shrt_Desc	VARCHAR (120),
`203`	DEC(10,3),
`204`	DEC(10,3),
`205`	DEC(10,3),
`207`	DEC(10,3),
`208`	DEC(10,3),
`209`	DEC(10,3),
`210`	DEC(10,3),
`211`	DEC(10,3),
`212`	DEC(10,3),
`213`	DEC(10,3),
`214`	DEC(10,3),
`221`	DEC(10,3),
`255`	DEC(10,3),
`257`	DEC(10,3),
`262`	DEC(10,3),
`263`	DEC(10,3),
`268`	DEC(10,3),
`269`	DEC(10,3),
`287`	DEC(10,3),
`291`	DEC(10,3),
`301`	DEC(10,3),
`303`	DEC(10,3),
`304`	DEC(10,3),
`305`	DEC(10,3),
`306`	DEC(10,3),
`307`	DEC(10,3),
`309`	DEC(10,3),
`312`	DEC(10,3),
`313`	DEC(10,3),
`315`	DEC(10,3),
`317`	DEC(10,3),
`318`	DEC(10,3),
`319`	DEC(10,3),
`320`	DEC(10,3),
`321`	DEC(10,3),
`322`	DEC(10,3),
`323`	DEC(10,3),
`324`	DEC(10,3),
`325`	DEC(10,3),
`326`	DEC(10,3),
`328`	DEC(10,3),
`334`	DEC(10,3),
`337`	DEC(10,3),
`338`	DEC(10,3),
`341`	DEC(10,3),
`342`	DEC(10,3),
`343`	DEC(10,3),
`344`	DEC(10,3),
`345`	DEC(10,3),
`346`	DEC(10,3),
`347`	DEC(10,3),
`401`	DEC(10,3),
`404`	DEC(10,3),
`405`	DEC(10,3),
`406`	DEC(10,3),
`410`	DEC(10,3),
`415`	DEC(10,3),
`417`	DEC(10,3),
`418`	DEC(10,3),
`421`	DEC(10,3),
`428`	DEC(10,3),
`429`	DEC(10,3),
`430`	DEC(10,3),
`431`	DEC(10,3),
`432`	DEC(10,3),
`435`	DEC(10,3),
`454`	DEC(10,3),
`501`	DEC(10,3),
`502`	DEC(10,3),
`503`	DEC(10,3),
`504`	DEC(10,3),
`505`	DEC(10,3),
`506`	DEC(10,3),
`507`	DEC(10,3),
`508`	DEC(10,3),
`509`	DEC(10,3),
`510`	DEC(10,3),
`511`	DEC(10,3),
`512`	DEC(10,3),
`513`	DEC(10,3),
`514`	DEC(10,3),
`515`	DEC(10,3),
`516`	DEC(10,3),
`517`	DEC(10,3),
`518`	DEC(10,3),
`521`	DEC(10,3),
`573`	DEC(10,3),
`578`	DEC(10,3),
`601`	DEC(10,3),
`605`	DEC(10,3),
`606`	DEC(10,3),
`607`	DEC(10,3),
`608`	DEC(10,3),
`609`	DEC(10,3),
`610`	DEC(10,3),
`611`	DEC(10,3),
`612`	DEC(10,3),
`613`	DEC(10,3),
`614`	DEC(10,3),
`615`	DEC(10,3),
`617`	DEC(10,3),
`618`	DEC(10,3),
`619`	DEC(10,3),
`620`	DEC(10,3),
`621`	DEC(10,3),
`624`	DEC(10,3),
`625`	DEC(10,3),
`626`	DEC(10,3),
`627`	DEC(10,3),
`628`	DEC(10,3),
`629`	DEC(10,3),
`630`	DEC(10,3),
`631`	DEC(10,3),
`636`	DEC(10,3),
`638`	DEC(10,3),
`639`	DEC(10,3),
`641`	DEC(10,3),
`645`	DEC(10,3),
`646`	DEC(10,3),
`652`	DEC(10,3),
`653`	DEC(10,3),
`654`	DEC(10,3),
`662`	DEC(10,3),
`663`	DEC(10,3),
`664`	DEC(10,3),
`665`	DEC(10,3),
`666`	DEC(10,3),
`669`	DEC(10,3),
`670`	DEC(10,3),
`671`	DEC(10,3),
`672`	DEC(10,3),
`673`	DEC(10,3),
`674`	DEC(10,3),
`675`	DEC(10,3),
`676`	DEC(10,3),
`685`	DEC(10,3),
`687`	DEC(10,3),
`689`	DEC(10,3),
`693`	DEC(10,3),
`695`	DEC(10,3),
`696`	DEC(10,3),
`697`	DEC(10,3),
`851`	DEC(10,3),
`852`	DEC(10,3),
`853`	DEC(10,3),
`855`	DEC(10,3),
`856`	DEC(10,3),
`857`	DEC(10,3),
`858`	DEC(10,3),
`859`	DEC(10,3)

);


SELECT * FROM borum_practice.USDA;
DROP TABLE borum_practice.USDA;
SELECT 