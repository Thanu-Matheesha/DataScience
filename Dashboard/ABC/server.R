setwd("~/KPMG/Dashboard/ABC")
library(shiny)
library(ABCanalysis)
library(dplyr)
library(DT)
library(data.table)
library(htmltools)

function(input, output, session){
  
  getData <- reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile))
    {
      returnValue()
    }
    else
    {
      data<-read.csv(inFile$datapath)
      if(nrow(data) > 1000)
      {
        shinyalert("Column Error","Uploaded Data has more than 20 locations",type="error")
        returnValue()
        # reset("file1")
      }
      
      else
      {
        return(data)
      }
    }
    
  })
  
  ########################################################################################################## 
  observe({
    if (!is.null(input$file1)) {
      namez <- getData()
      updateSelectInput(
        session,
        "y_input1",
        choices=unique(as.character(colnames(namez))))
      
      updateSelectInput(
        session,
        "y_input2",
        choices=unique(as.character(colnames(namez))))
      
      updateSelectInput(
        session,
        "y_input3",
        choices=unique(as.character(colnames(namez))))
      
      # updateSelectInput(
      #   session,
      #   "y_input4",
      #   choices=unique(as.character(colnames(namez))))
      
      updateSelectInput(
        session,
        "y_input5",
        choices=unique(as.character(colnames(namez))))
      
      updateSelectInput(
        session,
        "y_input6",
        choices=unique(as.character(colnames(namez))))
    }
    else {
      return(NULL)
    }
  })
  ##############################file2###############################################################
  coordvals <- eventReactive(input$goButton2, {
    if (is.null(input$file1))
    {
      returnValue()
    }
    else
    {
      namez <- getData()
      kml.coordinatest <- namez %>%
        rename(Date=input$y_input5,
               Item_Code=input$y_input6,
               Item_Des=input$y_input1,
               Quantity=input$y_input2,
               Value=input$y_input3)
               # Link=input$y_input4)
      if(is.numeric(kml.coordinatest$Quantity)==F)
      {
        shinyalert("Column Error","Quantity Column is not numeric",type="error")
        reset("file1")
        returnValue()
      }
      
      else
      {
        return(kml.coordinatest)
      }
    }

  })
  
  output$intable <- renderDataTable(coordvals(),options = list(
    autoWidth = FALSE, scrollX = T))
  output$val1 <- renderText(sum(is.na(coordvals())))
  output$val2 <- renderText(sum(is.na(coordvals()$Link)))
  output$natab <- renderDataTable(coordvals()[rowSums(is.na(coordvals())) > 0,],options = list(
    autoWidth = FALSE, scrollX = T))
  
  
}