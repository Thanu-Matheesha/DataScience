setwd("~/KPMG/Dashboard/Waste")
library(dplyr)
library(purrr)
library(shiny)
library(dygraphs)
library(forecast)
library(shinyjs)
library(reshape2)
library(DT)
library(readxl)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)
# library(lpSolve)
# library(lpSolveAPI)
# library(XLConnect)
function(input, output, session){
  
  sheets_name <- reactive({
    if (!is.null(input$file1)) {
      return(excel_sheets(path = input$file1$datapath))  
    } else {
      return(NULL)
    }
  })
  
  
  # output$value <- renderTable({
  #   if (!is.null(input$file1) && 
  #       (input$file1sheet %in% sheets_name())) {
  #     return(read_excel(input$file1$datapath, 
  #                       sheet = input$file1sheet))
  #   } else {
  #     return(NULL)
  #   }
  # })
  
    output$selectname <- renderUI({ if(is.null(input$file1))
      return()
      selectInput('name','Select Name',choices=c(as.character(unique(excel_sheets(path = input$file1$datapath)),selected = "Sabee"))) 
    })
    
    observe({
      output$value <- renderTable({if(is.null(input$name))
        return()
      ifelse(input$name == as.character(excel_sheets(path = input$file1$datapath)),
                 
              return(read_excel(input$file1$datapath,
                                sheet = input$name)),
            return(NULL)
      )
      })
    })
    
    sheet = reactive(excel_sheets(path = input$file1$datapath))
    df = reactive(lapply(setNames(sheet(), sheet()), function(x) read_excel(path = input$file1$datapath, sheet=x)))
    df1 = reactive(bind_rows(df(), .id="Sheet"))
    df5 = reactive({
      df6 <- df1()
      df6$shift <- as.numeric(as.factor(df6$Shift))
      df6$sheet <- as.numeric(as.factor(df6$Sheet))
    })
##############################################################################################################################    
##############################################################################################################################   
    df3 <- reactive({
      df2 <- df1()
      df2$shift <- as.numeric(as.factor(df2$Shift))
      df2$sheet <- as.numeric(as.factor(df2$Sheet))
    })
      df4 <- reactive(df3())
  
    # fac1 <- reactive(as.numeric(as.factor(df1()$Shift)))
    # fac2 <- reactive(as.numeric(as.factor(df1()$Sheet)))
    # df3 <- reactive({
    #   ftt <- cbind(df1(),fac1(),fac2())
    #   })
    # shifft <- length(unique(df1()$Shift))
    # labor <- length(unique(df1()$sheetname))
    # day1 <- ncol(df1())  
   
    output$dd <- renderTable(df1())
    
}