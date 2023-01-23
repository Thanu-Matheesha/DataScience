setwd("~/KPMG/Dashboard/Labor/Labor 2")
library(dplyr)
library(shiny)
library(forecast)
library(shinyjs)
library(reshape2)
library(DT)
library(readxl)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)

function(input, output) {
  
  getData <- reactive({
    
    inFile <- input$file1
    
    if (is.null(input$file1))
      return(NULL)
    
    read_xlsx(inFile$datapath)
    
  })
  
  
  output$contents <- renderTable(getData())
  
                
    df <- reactive(read_xlsx(input$file1$datapath))
    la <- reactive(max(ncol(df())))
    lb <- reactive(max(df()$Shift))
    
    scor <- function(labor, shift)  {
      out <- df()[df()$Shift==shift,][labor]
      print(as.numeric(out))  }
      
    score <- reactive({scor})
    
    modell <- reactive({
      
      la <- la()
      lb <- lb()
      score <- score()
      
      MIPModel() %>%
      
      add_variable(x[i, j], i = 2:la, j = 1:lb, type = "binary") %>%
      
      
      set_objective(sum_expr(score(i,j) * x[i,j], i = 2:la, j = 1:lb)) %>%
      
      
      add_constraint(sum_expr(x[i,j], j = 1:lb) <= input$shi, i = 2:la) %>%
      
      
      add_constraint(sum_expr(x[i,j], i = 2:la) <= input$lab, j = 1:lb)
      })
    
    resultt <- reactive({
      solve_model(modell(), with_ROI(solver = "glpk", verbose = TRUE))
      })
    
    val1 <- reactive(objective_value(resultt()))
    val2 <- reactive(solver_status(resultt()))
    
    matchingx <- reactive({
      
      score <- score()
      
      get_solution(resultt(), x[i,j])%>%
      filter(value > .9) %>%
      select(i, j) %>%
      rowwise() %>%
      mutate(score = score(as.numeric(i), as.numeric(j))) %>%
      rename(labor_number = i, shift = j)
      })
    
                        
    
#     Fmodel <- reactive({
#       matchingx <- matchingx()
#       dfff <- df()
# 
#         mutate(matchingx, labor=ifelse(matchingx$labor_name==2,colnames(dfff)[2],
#                                      ifelse(matchingx$labor_name==3,colnames(dfff)[3],
#                                             ifelse(matchingx$labor_name==4,colnames(dfff)[4],
#                                                    ifelse(matchingx$labor_name==5, colnames(dfff)[5],ifelse(matchingx$labor_name==6, colnames(dfff)[6],colnames(dfff)[7]))))) 
# )
#                       })
    
    labor_name<-reactive({
      df <- df()
      matchingx <- matchingx()
    labor <- ifelse(matchingx$labor_number==2,colnames(df)[2],
                                   ifelse(matchingx$labor_number==3,colnames(df)[3],
                                          ifelse(matchingx$labor_number==4,colnames(df)[4],
                                                 ifelse(matchingx$labor_number==5, colnames(df)[5],ifelse(matchingx$labor_number==6, colnames(df)[6],colnames(df)[7])))))

})
    
    fff <- reactive({
      labor_name <- labor_name()
      matchingx <- matchingx()
      
      ftt <- cbind(matchingx, labor_name)
    })
  
  observeEvent(input$goButton, {
    if (is.null(input$file1)) return()
   output$model <- renderTable(fff())
   output$val1  <- renderText(val1())
   output$val2  <- renderText(val2())
                              })
  
  observeEvent(input$go, {
    if (is.null(input$file1)) return()
    output$model <- renderTable({NULL})
    output$val1  <- renderText({NULL})
    output$val2  <- renderText({NULL})
  })
  # observeEvent(input$reset, {
  #   if (is.null(input$file1)) return()
  #   removeUI("#model")
  # })
  
}