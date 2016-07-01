#FOODOMICS PACKAGE
#######Functions
          # readfromdatabase(table)
          # uploadtodatabase(table,dest)




readfromdatabase <- function(table) {
  
  if (!require("RMySQL")) {
    devtools::install_github("rstats-db/RMySQL")
  }
  library(RMySQL)
  
  all_cons <- dbListConnections(MySQL())
  for (con in all_cons) {
    dbDisconnect(con)
  }
  
  print("When prompted, please input the MySQL username, password, database name, and host that you wish to use")
  print("To leave any of these fields blank, press the enter key without typing anything else")
  #u <- readline(prompt="User: ")
  #p <- readline(prompt="Password: ")
  #d <- readline(prompt="Database Name: ")
  #h <- readline(prompt="Host: ")
  
  u <- "dlennon"
  p <- "cNrYqWax1"
  d <- "borum_practice"
  h <- "if-srvv-borum"
  connect <- dbConnect(MySQL(),user=u,password=p,dbname=d,host=h)
  
  table<-table

  MySQLstatement <- paste("SELECT * FROM",table,";")
  data.frame(suppressWarnings(dbGetQuery(connect,MySQLstatement)))
}







#Script created for one-time use to upload the historical database into MySQL

#where table is the table of the right format that you want to upload
#where dest is the name of the table in the database your are placing the data
#this script overwrites whatever data is present

uploadtodatabase <- function(table,dest) {
  
  if (!require("RMySQL")) {
    install.packages("RMySQL")
  }
  library(RMySQL)
  
  all_cons <- dbListConnections(MySQL())
  for (con in all_cons) {
    dbDisconnect(con)
  }
  
  data<-table
  namegiven<-dest
  
  print("When prompted, please input the MySQL username, password, database name, and host that you wish to use")
  print("To leave any of these fields blank, press the enter key without typing anything else")
  u <- "dlennon"
  p <- "cNrYqWax1"
  d <- "borum_practice"
  h <- "if-srvv-borum"
  connect <- dbConnect(MySQL(),user=u,password=p,dbname=d,host=h)
  
  
  dbWriteTable(connect,value=data,name=namegiven,overwrite=TRUE)
  
  
  
  print("Your table in MySQL have been updated")
}
   

