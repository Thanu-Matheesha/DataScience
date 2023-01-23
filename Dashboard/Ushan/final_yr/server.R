setwd("~/KPMG/Dashboard/Ushan/final_yr/")
load("data_file.RData")
library(shiny)
library(shinydashboard)
library(plotly)
library(plyr)
library(dplyr)
library(DT)
library(readxl)
library(ggplot2)
library(plotrix)
library(shinythemes)
library(plotrix)
library(flexdashboard)


function(input, output,session) {
  
  updateSelectizeInput(session, 'selectInput', choices = c('Migraine','Kidney disease','Diabetes','Depression','Cancer','Muscular dystrophy','Wheeze','Heart attack','Arthritis'), server = TRUE)
  
  # filterdata <- reactive({
  #   data_1[which(data_1$Name_Of_disease == input$selectInput),]
  # 
  # })
  
  myValue <- reactiveValues(employee = '')
  
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  df <- reactiveValues(data = data.frame(Age=data_1$Age,
                                         # Consulting_Period = data_1$Consulting_period,
                                         Name_of_disease = data_1$Name_Of_disease,
                                         # Communication = data_1$Communication,
                                         # Friendliness = data_1$Friendliness,
                                         # waiting_Time = data_1$Wating_Time,
                                         # Helpfulness=data_1$Helpfullness,
                                         Cleanliness=data_1$Cleanillness,
                                         Billing=data_1$Billing,
                                         Doctor_Rating=data_1$Doctor_Rating,
                                         Actions = shinyInput(actionButton, 249, 'button_', label = "Detail", onclick = 'Shiny.setInputValue(\"select_button\", this.id, {priority: \"event\"})',icon=icon("info"),
                                                              class = "btn action-button",width = '100%',
                                                              style = "color: white;
                                                              background-color: blue"),
                                         stringsAsFactors = FALSE,
                                         row.names = 1:249
                                         
  ))
  
  # updateSelectizeInput(session, 'selectInput', choices = c('Migraine','Kidney disease','Diabetes','Depression','Cancer','Muscular dystrophy','Wheeze','Heart attack','Arthritis'), server = TRUE)
  # 
  # filterData <- reactive({
  #   df$data[which(df$data$Name_Of_disease == input$selectInput),]
  #   
  # })
  
  # vals<-reactiveValues()
  # vals$Data<-data.table(
  #   ag=data_1$Age,
  #   find=data_1$Friendliness,
  #   hope=data_1$Helpfullness,
  #   rate=data_1$Doctor_Rating,
  #   rating= actionButton("manage_1", "Override Risk")
  #   
  # )
  # 
  observe({
  output$data <- DT::renderDataTable({if(is.null(input$selectInput))
    return()
    ifelse(input$selectInput == df$data$Name_Of_disease,
    return(df$data[which(df$data$Name_Of_disease == input$selectInput),]),
    return(NULL)
    )
    # df$data
  
    

    datatable(
      df$data, escape = FALSE, selection = 'none')
    # datatable(filterData(),selection="single",rownames = F)
    # DT=vals$Data
   # editable = 'cell'
    # 
    # options = list(
    #   pageLength = 5, autoWidth = TRUE
    # )
    # datatable(DT, escape=F)
  })
  })
  observeEvent(input$select_button, {
    updateTabsetPanel(session, "inTabset",selected = "wi")
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    myValue$employee <<- paste('click on ',df$data[selectedRow,1])
    # myValue$employee <<- gauge(df$data[selectedRow,6],
    #                            min = -3.202, 
    #                            max = 5.172,
    #                            sectors = gaugeSectors(success = c(3, 5.172), 
    #                                                   warning = c(0, 3),
    #                                                   danger = c(-3.202, 0)))
    # 
  })
  
  output$myText <- renderText({

    myValue$employee

  })
  
  
  
  observeEvent(input$manage_1, {
   print('ushan')
    
  })

  
  
  
}
