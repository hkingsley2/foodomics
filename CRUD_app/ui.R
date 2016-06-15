if(!require(shiny)){
  install.packages("shiny")
  library(shiny) #for formatting structure of data
}

if(!require(RMySQL)){
  install.packages("RMySQL")
  library(RMySQL) #for formatting structure of data
}

if(!require(DT)){
  install.packages("DT")
  library(DT) #for formatting structure of data
}

if(!require(shinydashboard)){
  install.packages("shinydashboard")
  library(shinydashboard) #for formatting structure of data
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2) #for formatting structure of data
}

#Create header for dashboard page
header<-dashboardHeader(title = HTML(paste(icon('globe'),'Foodomics Database')))

#Create body for dashboard page
body<-dashboardBody(
  shinyjs::useShinyjs(),
  tabItems(
    
    # First tab content
    tabItem(tabName = "Enter",
            #***app1***
            
            fluidRow(
        column(width = 8,
           DT::dataTableOutput("test"),
            br()
           ,
           box(
             title = "Macros", status = "success", solidHeader = TRUE,
             collapsible = TRUE,
             collapsed = TRUE,
             textInput("Serving_Size","Serving_Size",""),
             numericInput("Weight_per_serving_g","Weight_per_serving_g",""),
             numericInput("Calories_per_serving_kcal","Calories_per_serving_kcal",""),
             numericInput("Total_Fat_per_serving_g","Total_Fat_per_serving_g",""),
             numericInput("Saturated_Fat_per_serving_g","Saturated_Fat_per_serving_g",""),
             numericInput("Monounsaturated_Fat_per_serving_g","Monounsaturated_Fat_per_serving_g",""),
             numericInput("Polyunsaturated_Fat_per_serving_g","Polyunsaturated_Fat_per_serving_g",""),
             numericInput("Trans_Fat_per_serving_g","Trans_Fat_per_serving_g",""),
             numericInput("Carbohydrate_per_serving_g","Carbohydrate_per_serving_g",""),
             numericInput("Dietary_Fiber_per_serving_g","Dietary_Fiber_per_serving_g",""),
             numericInput("Sugar_per_serving_g","Sugar_per_serving_g",""),
             numericInput("Protein_per_serving_g","Protein_per_serving_g",""),
             numericInput("Sodium_DV_per_serving","Sodium_DV_per_serving",""),
             numericInput("Potassium_DV_per_serving","Potassium_DV_per_serving",""),
             numericInput("Vitamin_A_DV_per_serving","Vitamin_A_DV_per_serving",""),
             numericInput("Vitamin_C_DV_per_serving","Vitamin_C_DV_per_serving",""),
             numericInput("Calcium_DV_per_serving","Calcium_DV_per_serving",""),
             numericInput("Iron_DV_per_serving","Iron_DV_per_serving","")
             
           ),
           
           
           box(
             title = "Fats", status = "warning", solidHeader = TRUE,
             collapsible = TRUE,
             collapsed = TRUE,
             numericInput("Saturated_Fat_per_serving_g","Saturated_Fat_per_serving_g",""),
             numericInput("Monounsaturated_Fat_per_serving_g","Monounsaturated_Fat_per_serving_g",""),
             numericInput("Polyunsaturated_Fat_per_serving_g","Polyunsaturated_Fat_per_serving_g",""),
             numericInput("Trans_Fat_per_serving_g","Trans_Fat_per_serving_g","")
           ),
           
           
           box(
             title = "Daily Values", status = "primary", solidHeader = TRUE,
             collapsible = TRUE,
             collapsed = TRUE,
             numericInput("Calories_per_serving_kcal","Calories_per_serving_kcal",""),
             numericInput("Saturated_Fat_per_serving_g","Saturated_Fat_per_serving_g",""),
             numericInput("Trans_Fat_per_serving_g","Trans_Fat_per_serving_g","")
           ),
           
           
           box(
             title = "Vitamins & Minerals", status = "info", solidHeader = TRUE,
             collapsible = TRUE,
             collapsed = TRUE,
             numericInput("Calories_per_serving_kcal","Calories_per_serving_kcal",""),
             numericInput("Saturated_Fat_per_serving_g","Saturated_Fat_per_serving_g",""),
             numericInput("Trans_Fat_per_serving_g","Trans_Fat_per_serving_g","")
           )
           
           ), tags$hr(),
        
        column(
            width=4,
          numericInput("Year","Year",""),
          textInput("NDID","NDID",""),
          
          checkboxInput("Pend_Food","Pend this Food",value=TRUE, width=NULL),
          checkboxInput("Active","Activate this Food",value=TRUE, width=NULL),
          
          
          actionButton("submit", "Submit New Food"),
          actionButton("edit", "Update Food"),
          actionButton("delete", "Delete Food"),
          fileInput('file1', '',accept = c('.jpg','.jpeg')),
          actionButton("downloadFile1", "Send Photo"),
          imageOutput('outputImage')

      
          )
      )
    ),
    
    
    # Second tab content
    tabItem(tabName = "Audit",
            
            fluidRow(
              #***Meal Making Table***
              column(width = 9,
                     DT::dataTableOutput("auditmeals")),
              #***Help Text***
              column(width = 3,
                     helpText("Make PKT meals using these data."),
                     helpText("Blue is lower than 25% of that nutrient"),
                     helpText("White is between 25 and 75% of that nutrient"),
                     helpText("Red is more than 75% of that nutrient"))
            )
    ),
   
    
    # Second tab content
    tabItem(tabName = "Create",
           
            fluidRow(
              #***Meal Making Table***
              column(width = 9,
                     DT::dataTableOutput("makemeals")),
              #***Help Text***
              column(width = 3,
                     helpText("Make PKT meals using these data."),
                     helpText("Blue is lower than 25% of that nutrient"),
                     helpText("White is between 25 and 75% of that nutrient"),
                     helpText("Red is more than 75% of that nutrient"))
             )
    )
  )
)
  
  

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Enter", tabName = "Enter", icon = icon("dashboard")),
    menuItem("Audit", tabName = "Audit", icon = icon("dashboard")),
    menuItem("Create", tabName = "Create", icon = icon("dashboard"))
  )
  )
  
  
  dashboardPage(skin = "blue",
                header,
                sidebar,
                body
  )
