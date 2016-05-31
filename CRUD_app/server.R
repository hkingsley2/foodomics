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



fields <- c(Year="Year",
            NDID="NDID",
            Calories_per_serving_kcal="Calories_per_serving_kcal",
            Saturated_Fat_per_serving_g="Saturated_Fat_per_serving_g",
            Trans_Fat_per_serving_g="Trans_Fat_per_serving_g",
            Monounsaturated_Fat_per_serving_g="Monounsaturated_Fat_per_serving_g",
            Polyunsaturated_Fat_per_serving_g="Polyunsaturated_Fat_per_serving_g",
            Pend_Food = "Pend_Food")


library(RMySQL)
options(mysql = list(
  user="dlennon",
  password="cNrYqWax1",
  host="if-srvv-borum",
  dbname="borum_practice"
)) 

TABLE_NAME <- "foodomics_database_test"

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
  query <- sprintf("SELECT * FROM %s WHERE Year='2001'", TABLE_NAME)
  mysql_query(query)
}

load_data_makemeals <- function() {
  query <- sprintf("SELECT * FROM %s WHERE Year='2001' AND Pend_Food!='TRUE'", TABLE_NAME)
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
    }, deleteFile = TRUE)
    

    plotInput <- function(){
      list(src = input$file1$datapath,
           contentType=input$file1$type)
    }
    
    plotInput2 = function () {

      list(src = input$file1,
           outfile = input$file1$datapath,
           contentType = input$file1$type)

      
    }

    output$downloadFile1 <- downloadHandler(
      filename = function() { paste0(input$file1, sep='') },
      
      content = function(file) {
        jpeg(file)
        plotInput2()
        dev.off()

      },
      contentType=input$file1$type
    )
    
    
    # When buttons are clicked, run the appropriate function with the form data
    observeEvent(input$downloadFile1, { 


      })
    
    
    observeEvent(input$submit, { save_data_mysql(formData()) })
    observeEvent(input$delete, { delete_data_mysql(input$rows, formData()) })
    observeEvent(input$edit, { edit_data_mysql(input$rows, formData()) })
    observeEvent(input$rows, {
      updateTextInput(session,"Year",value=input$rows[[3]])
      updateTextInput(session,"NDID",value=input$rows[[4]])
      updateNumericInput(session,"Calories_per_serving_kcal",value=input$rows[[5]])
      updateNumericInput(session,"Saturated_Fat_per_serving_g",value=input$rows[[6]])
      updateNumericInput(session,"Trans_Fat_per_serving_g",value=input$rows[[7]])
      updateNumericInput(session,"Monounsaturated_Fat_per_serving_g",value=input$rows[[8]])
      updateNumericInput(session,"Polyunsaturated_Fat_per_serving_g",value=input$rows[[9]])
      updateTextInput(session,"Pend_Food",value=input$rows[[10]])
      
      
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
        options = list(scrollX = TRUE)
        
        )
  })
    
    
    
    

    
    # Show the previous test
    # (update with current response when Submit is clicked)
    output$makemeals = renderDataTable({
      datatable(
        {
          load_data_makemeals()
        },options = list(scrollX = TRUE, columnDefs = list(list(width = '200px', targets = "c(1)")),searchHighlight = TRUE,pageLength = 10,initComplete = JS(
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
    
    
    
    
    
  }
  

      )
