####Creates MAKE_MEALS Dataset

use borum_practice;
create table MAKE_MEALS(
NFD_Key BIGINT (255) NOT NULL AUTO_INCREMENT PRIMARY KEY,
PHOTO_FILE_PATH VARCHAR (120),
NDID VARCHAR (10),
CATEGORY_1 VARCHAR (120),
CATEGORY_2 VARCHAR (120),
CATEGORY_3 VARCHAR (120),
PRODUCT_NAME VARCHAR (120),
FAT_FDB VARCHAR (5),
PROCNT_FDB VARCHAR (5),
CHOCDF_FDB VARCHAR (5),
ENERC_KJ_FDB VARCHAR (5),
PRICE_PER_PACKAGE VARCHAR (5),
SERVING_SIZE VARCHAR (120),
STORE VARCHAR (120),
USDA_FOOD_GROUP VARCHAR (120),
SUB_FOOD_GROUP VARCHAR (120),
SUB_SUB_FOOD_GROUP VARCHAR (120),
RAW_COOKED VARCHAR (120),
PACKAGE_TYPE VARCHAR (120),
WEIGH_INSTRUCTION VARCHAR (120)
);

