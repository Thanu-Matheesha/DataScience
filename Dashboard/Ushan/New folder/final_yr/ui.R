setwd("/home/kpmg/Desktop/final_yr/")
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyalert)
library(flexdashboard)
library(dashboardthemes)
library(DT)

load("data_file.RData")

dashboardPage(
  dashboardHeader(title = "Raiting Model"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "widgets_1", icon = icon("star"))
      
    )
  ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "poor_mans_flatly"
    ),
    useShinyalert(),
    tabItems(
      
      tabItem(tabName = "widgets_1",
              column(width=12,
                     
                     fluidRow(
                       column(12,
                              selectizeInput("selectInput",label ="Filter", choices= NULL, selected = NULL)
                       ),
                       column(12,
                              DT::dataTableOutput('tab1')
                       )
                     )
                     
                     
              ),
              fluidRow(box(
                width = 12,
                column(width=8,offset = 4,tags$b('Powered by Geeshani Amarawansha',style = "color:#2c3e50;"))
              ))
              
              
      )
      
      
      
      
      
      
      
    )
    
    
  )
  
  
)

