setwd("~/KPMG/Dashboard/Ushan/final_yr/")
library(shiny)
library(shinydashboard)
library(plotly)
library(plyr)
library(dplyr)
library(DT)
library(readxl)
library(ggplot2)
library(plotrix)
library(shinycssloaders)
library(flexdashboard)
# library(dashboardthemes)
library(DT)

load("data_file.RData")

dashboardPage(
  dashboardHeader(title = "Raiting Model"),
  dashboardSidebar(
    sidebarMenu( id="inTabset",
      menuItem("Home", tabName = "widgets_1", icon = icon("star")),
      menuItem("Test", tabName = "wi", icon = icon("book"))
      
    )
  ),
  dashboardBody(
    # shinyDashboardThemes(
    #   theme = "poor_mans_flatly"
    # ),
    # useShinyalert(),
    tabItems(
      
      tabItem(tabName = "widgets_1",
              fluidRow(
                column(width=12,
                       box(width = "100%",withSpinner(dataTableOutput('data'),type = 6)
                       ))
                
              ),
                     fluidRow(
                       column(width=12,
                              selectizeInput("selectInput",label ="Filter", choices= NULL, selected = NULL)
                       )),
                     # fluidRow(
                     #   column(width=10,
                     #          box(width = "100%", 
                     #          dataTableOutput('data')
                     #   ))
                     
                     
                     
                     
              
              fluidRow(box(
                width = 12,
                column(width=8,offset = 4,tags$b('Powered by Geeshani Amarawansha',style = "color:#2c3e50;"))
              ))),
      
      tabItem(tabName = "wi",
              fluidRow(
                column(width=3,
                       box(width = "100%", 
                           textOutput('myText')
                       ))))
              
              
      )
      
      
      
      
      
      
      
    )
    
    
  )
  
  


