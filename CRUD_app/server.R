if(!require(shiny)){
  install.packages("shiny")
  library(shiny) 
}

if(!require(RMySQL)){
  install.packages("RMySQL")
  library(RMySQL) 
}

if(!require(DT)){
  install.packages("DT")
  library(DT) 
}

if(!require(shinydashboard)){
  install.packages("shinydashboard")
  library(shinydashboard) 
}

if(!require(shinyjs)){
  install.packages("shinyjs")
  library(shinyjs) 
}

if(!require(jpeg)){
  install.packages("jpeg")
  library(jpeg) 
}


fields <- c(Year="Year",
            NDID="NDID",
            Serving_Size="Serving_Size",
            Weight_per_serving_g="Weight_per_serving_g",
            Calories_per_serving_kcal="Calories_per_serving_kcal",
            Total_Fat_per_serving_g="Total_Fat_per_serving_g",
            Saturated_Fat_per_serving_g="Saturated_Fat_per_serving_g",
            Monounsaturated_Fat_per_serving_g="Monounsaturated_Fat_per_serving_g",
            Polyunsaturated_Fat_per_serving_g="Polyunsaturated_Fat_per_serving_g",
            Trans_Fat_per_serving_g="Trans_Fat_per_serving_g",
            Carbohydrate_per_serving_g="Carbohydrate_per_serving_g",
            Dietary_Fiber_per_serving_g="Dietary_Fiber_per_serving_g",
            Sugar_per_serving_g="Sugar_per_serving_g",
            Protein_per_serving_g="Protein_per_serving_g",
            Sodium_DV_per_serving="Sodium_DV_per_serving",
            Potassium_DV_per_serving="Potassium_DV_per_serving",
            Vitamin_A_DV_per_serving="Vitamin_A_DV_per_serving",
            Vitamin_C_DV_per_serving="Vitamin_C_DV_per_serving",
            Calcium_DV_per_serving="Calcium_DV_per_serving",
            Iron_DV_per_serving="Iron_DV_per_serving",
            Pend_Food = "Pend_Food",
            Active= "Active")


library(RMySQL)
options(mysql = list(
  user="dlennon",
  password="cNrYqWax1",
  host="if-srvv-borum",
  dbname="borum_practice"
)) 

TABLE_NAME <- "foodomics_database_test2"

save_data_mysql <- function(formdata) {
  query <- sprintf("INSERT INTO %s (%s) VALUES ('%s')", 
                   TABLE_NAME, paste(names(formdata), collapse = ", "), paste(formdata, collapse = "', '"))
  mysql_query(query)
}

delete_data_mysql <- function(selected, formdata) {
  key <- strsplit(selected," ")[[2]]
  query <- sprintf("DELETE FROM %s WHERE NFD_Key='%s'", 
                   TABLE_NAME, key)
  mysql_query(query)
}

edit_data_mysql <- function(selected, formdata) {
  key <- selected[[2]]
  query <- sprintf("UPDATE %s SET %s WHERE NFD_Key='%s'", 
                   TABLE_NAME, paste(names(formdata),"='",formdata,"'",sep = "",collapse = ", "), key)
  mysql_query(query)
}

load_data_mysql <- function() {
  query <- sprintf("SELECT * FROM %s WHERE Year='2001' ", TABLE_NAME)
  mysql_query(query)
}

load_data_makemeals <- function() {
  query <- sprintf("SELECT * FROM %s WHERE Year='2001' AND Pend_Food!='TRUE' AND Active=TRUE", TABLE_NAME)
  mysql_query(query)
}

load_data_auditmeals <- function() {
  query <- sprintf("SELECT * FROM %s WHERE Year='2001' AND Pend_Food='TRUE'", TABLE_NAME)
  mysql_query(query)
}

mysql_query <- function(query) {
  print(query)
  db <- dbConnect(MySQL(), dbname = options()$mysql$dbname, host = options()$mysql$host, 
                  user = options()$mysql$user, 
                  password = options()$mysql$password)
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}


server <- shinyServer(
  function(input, output, session) {


    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(
        fields, 
        function(x){
        input[[x]]
        }
      )

      data
    })
    
    observe({
      if (input$submit > 0 | input$edit > 0 | input$delete > 0) {
        shinyjs::info("Submitted")
      }
    })
    
    #If form is empty, do not allow edit,update,delete buttons to work
    
    observe({
      if (is.null(input$NDID) || input$NDID == "") {
        shinyjs::disable("submit")
        shinyjs::disable("edit")
        shinyjs::disable("delete")
      } else {
        shinyjs::enable("submit")
        shinyjs::enable("edit")
        shinyjs::enable("delete")
      }
    })
    
    
    observe({
      if (is.null(input$file1) || input$file1 == "") {
        shinyjs::disable("downloadFile1")

      } else {
        shinyjs::enable("downloadFile1")

      }
    })
    
    #Dealing with uploaded photos
    
    output$outputImage <- renderImage({
      ###This is to plot uploaded image###
      if (is.null(input$file1)){
        outfile <- "G:\\Data_D\\D18\\NFD\\Pictures for NFD\\2016\\Done\\NDID 00001-00100\\ND00001_C_232016_SH_PB.jpg"
        contentType <- "image/jpg"
        #Panda image is the default
      }else{
        outfile <- input$file1$datapath
        contentType <- input$file1$type
        #Uploaded file otherwise
      }
      
      list(src = outfile,
           contentType=contentType,
           width=100)
    }, deleteFile = FALSE)
    


    
    # When buttons are clicked, run the appropriate function with the form data
  #  observeEvent(input$downloadFile1, { 
      
    #  outfile <- input$file1$datapath
    #  jpg = readJPEG(outfile, native=T) # read the file
    #  res = dim(jpg)[1:2] # get the resolution

   #  plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
   #  rasterImage(jpg,1,1,res[2],res[1])
   #  file.copy(outfile, paste0("images\\", input$file1$name))

    #need photos named differently
    #need them saved in a differently
  #    })
    
    
    observeEvent(input$submit, { save_data_mysql(formData()) })
    observeEvent(input$delete, { delete_data_mysql(input$rows, formData()) })
    observeEvent(input$edit, { edit_data_mysql(input$rows, formData()) })
    observeEvent(input$rows, {
      updateTextInput(session,"Year",value=input$rows[[3]])
      updateTextInput(session,"NDID",value=input$rows[[4]])
      updateTextInput(session,"Serving_Size",value=input$rows[[5]])
      updateNumericInput(session,"Weight_per_serving_g",value=input$rows[[6]])
      updateNumericInput(session,"Calories_per_serving_kcal",value=input$rows[[7]])
      updateNumericInput(session,"Total_Fat_per_serving_g",value=input$rows[[8]])
      updateNumericInput(session,"Saturated_Fat_per_serving_g",value=input$rows[[9]])
      updateNumericInput(session,"Monounsaturated_Fat_per_serving_g",value=input$rows[[10]])
      updateNumericInput(session,"Polyunsaturated_Fat_per_serving_g",value=input$rows[[11]])
      updateNumericInput(session,"Trans_Fat_per_serving_g",value=input$rows[[12]])
      updateNumericInput(session,"Carbohydrate_per_serving_g",value=input$rows[[13]])
      updateNumericInput(session,"Dietary_Fiber_per_serving_g",value=input$rows[[14]])
      updateNumericInput(session,"Sugar_per_serving_g",value=input$rows[[15]])
      updateNumericInput(session,"Protein_per_serving_g",value=input$rows[[16]])
      updateNumericInput(session,"Sodium_DV_per_serving",value=input$rows[[17]])
      updateNumericInput(session,"Potassium_DV_per_serving",value=input$rows[[18]])
      updateNumericInput(session,"Vitamin_A_DV_per_serving",value=input$rows[[19]])
      updateNumericInput(session,"Vitamin_C_DV_per_serving",value=input$rows[[20]])
      updateNumericInput(session,"Calcium_DV_per_serving",value=input$rows[[21]])
      updateNumericInput(session,"Iron_DV_per_serving",value=input$rows[[22]])
      
      
    })
    
    
    # Show the previous test
    # (update with current response when Submit is clicked)
    output$test = renderDataTable({
      datatable(
        {
          input$submit
          input$delete
          input$edit
          load_data_mysql()
        },
        selection = 'none',
        callback = JS(
          "table.on('click.dt', 'tr', function() {
          $('tr').removeClass('selected');
          $(this).addClass('selected');
          Shiny.onInputChange('rows', table.rows('.selected').data().toArray());
    });"),
        options = list(scrollX = TRUE,pageLength = 5)
        
        )
  })
    
    
    
    

    
    # Show the previous test
    # (update with current response when Submit is clicked)
    output$makemeals = renderDataTable({
      datatable(
        {
          load_data_makemeals()
        },options = list(scrollX = TRUE, columnDefs = list(list(width = '200px', targets = "c(1)")),searchHighlight = TRUE,pageLength = 5,initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': 'black', 'color': 'white'});",
          "}")), class="stripe compact nowrap", rownames = FALSE
        
      ) %>%
        formatStyle(
          'Saturated_Fat_per_serving_g',
          color = styleInterval(c(25, 75), c('white', 'black', 'white')),
          backgroundColor = styleInterval(c(25, 75), c('blue', 'white', 'red'))
        ) %>%
        formatStyle(
          'Monounsaturated_Fat_per_serving_g',
          color = styleInterval(c(25, 75), c('white', 'black', 'white')),
          backgroundColor = styleInterval(c(25, 75), c('blue', 'white', 'red'))
        ) %>%
        formatStyle(
          'Polyunsaturated_Fat_per_serving_g',
          color = styleInterval(c(25, 75), c('white', 'black', 'white')),
          backgroundColor = styleInterval(c(25, 75), c('blue', 'white', 'red'))
        )
        
        
        
    }) 
    
    output$auditmeals = renderDataTable({
      datatable(
        {
          load_data_auditmeals()
        },options = list(scrollX = TRUE, columnDefs = list(list(width = '200px', targets = "c(1)")),searchHighlight = TRUE,pageLength = 5,initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': 'black', 'color': 'white'});",
          "}")), class="stripe compact nowrap", rownames = FALSE
        
      )})
    
    
    
    
    
  }
  

      )
