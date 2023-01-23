#setwd("~/TimeSheet")
library(tidyverse)
library(readxl)
library(xlsx)
library(lubridate)
library(rsconnect)


function(input, output, session){
  
  
 # values <- reactiveValues() 
 # values$df <- read.csv("Document.csv")
 
 test <- read.csv("Document.csv")
 
 #df$input.InDate <- as.Date(df$input.InDate, format = "%Y-%m-%d")
 
 output$selectComp <- renderUI(
   selectInput("comp_sel","Company Name:", choices= as.character(unique(test$Company))))
 
 output$selectser <- renderUI(
   selectInput("ser_sel","Service Type:", choices= as.character(unique(test$Service))))
 
 df2 <- reactive({ 

  userIn <- data.frame(as.character(input$InDate), as.character(input$OutDate), strftime(input$time1), strftime(input$time2))
  company <- data.frame(ifelse(input$caption == "", as.character(input$comp_sel), input$caption ))
  service <- data.frame(ifelse(input$Ser1 == "", as.character(input$ser_sel), input$Ser1 ))
  userIn2 <- cbind(userIn,company,service)
  colnames(userIn2) <- c("In.Date", "Out.Date", "In.Time", "Out.Time","Company","Service")
  
  
  return(userIn2)
 })
  
  
  observeEvent(input$test, {
    showModal(modalDialog(
      title = "Successfully Saved",
      "Navigate to Analytics tab to see the results"
    ))
    
    test <- read.csv("Document.csv")
    
    # values <- reactiveValues() 
    # values$df <- read.csv("Document.csv")
    
    
    #df <- read.csv("TimeSheet.csv") 
    # wb = loadWorkbook("TimeSheet2.xlsx")
    # removeSheet(wb, sheetName = "Sheet1")
    # saveWorkbook(wb, "TimeSheet2.xlsx")
    
    df3 <- rbind(test,df2())
    df4 <- df3
    df4$In.Time <- ymd_hms(df4$In.Time)
    df4$Out.Time <- ymd_hms(df4$Out.Time)
    #print(class(df4$In.Time))
   
    
    #df4$Duaration <- ifelse(is.null(df3) == T, print(""), (df4$Out.Time-df4$In.Time)/3600)
    df4$Duaration <- round((df4$Out.Time-df4$In.Time),2)
    #df4 <- ifelse(input$caption == "", subset(df4, Company == input$comp_sel),subset(df4, Company == input$caption))
    
    if (input$caption == "") {
      df4 <- subset(df4, Company == input$comp_sel)
      #return(df4)
    } else {
      
      df4 <- subset(df4, Company == input$caption)
      #return(df4)
      
    }
    
    write_csv(df3,"Document.csv", append = F)
    output$dff <- renderDataTable(df2())
    output$dfff <- renderDataTable(df4)
    output$Tot <- renderText(round(sum(df4$Duaration),2))
    output$Rem <- renderText(input$obs-(sum(df4$Duaration)))
    output$gauge = renderGauge({
      gauge(round(input$obs-(sum(df4$Duaration)),2), 
            min = 0, 
            max = 200, 
            sectors = gaugeSectors(success = c(100, 200), 
                                   warning = c(25, 100),
                                   danger = c(0, 25)))
    })
    })
  
  
  
}
