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

#Create header for dashboard page
header<-dashboardHeader(title = HTML(paste(icon('globe'),'Foodomics Database')))

#Create body for dashboard page
body<-dashboardBody(
  
  tabItems(
    
    # First tab content
    tabItem(tabName = "Data",
            #***app1***
            
            fluidRow(
        column(width = 9,
           DT::dataTableOutput("test")
           
           ), tags$hr(),
        
        column(
            width=3,
          numericInput("Year","Year",""),
          textInput("NDID","NDID",""),
          numericInput("Calories_per_serving_kcal","Calories_per_serving_kcal",""),
          numericInput("Saturated_Fat_per_serving_g","Saturated_Fat_per_serving_g",""),
          numericInput("Trans_Fat_per_serving_g","Trans_Fat_per_serving_g",""),
          numericInput("Monounsaturated_Fat_per_serving_g","Monounsaturated_Fat_per_serving_g",""),
          numericInput("Polyunsaturated_Fat_per_serving_g","Polyunsaturated_Fat_per_serving_g",""),
          checkboxInput("Pend_Food","Pend this Food",value=TRUE, width=NULL),
        
          actionButton("submit", "Submit New Food"),
          actionButton("edit", "Update Food"),
          actionButton("delete", "Delete Food")
      
          )
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
    menuItem("Data", tabName = "Data", icon = icon("dashboard")),
    menuItem("Create", tabName = "Create", icon = icon("dashboard"))
  )
  )
  
  
  dashboardPage(skin = "blue",
                header,
                sidebar,
                body
  )
