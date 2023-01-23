setwd("~/UiPath/R_Dash/Quarantine")
library(dplyr)
library(purrr)
library(shiny)
library(shinyjs)
library(reshape2)
library(DT)
library(readxl)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)
library(writexl)

function(input, output, session){
  
  sheets_name <- reactive({
    if (!is.null(input$file1)) {
      return(excel_sheets(path = input$file1$datapath))  
    } else {
      return(NULL)
    }
  })
  
  output$selectname <- renderUI({ if(is.null(input$file1))
    return()
    selectInput('name','Select Name',choices=c(as.character(unique(excel_sheets(path = input$file1$datapath)),selected = "Sabee"))) 
  })
  
  output$text1 <- renderText("Employee Preference")
  
  observe({
    output$value <- renderDataTable({if(is.null(input$name))
      return()
      ifelse(input$name == as.character(excel_sheets(path = input$file1$datapath)),
             
             return(read_excel(input$file1$datapath,
                               sheet = input$name)),
             return(NULL)
      )
    })
  })
  
  dataset<-reactive({
    if (!is.null(input$file1)) {
    mysheets_fromexcel <- list()
    
    mysheetlist <- excel_sheets(path=input$file1$datapath)
    i=1
    for (i in 1:length(mysheetlist)){
      tempdf <- read_excel(path=input$file1$datapath, sheet = mysheetlist[i])
      tempdf$sheetname <- mysheetlist[i]
      mysheets_fromexcel[[i]] <- tempdf 
    }
    df1 <- do.call(rbind.data.frame, mysheets_fromexcel)
    return(df1)
    }
    else {
      return(NULL)
    }
  })
  
  dataset2<-reactive({
    if (!is.null(input$file1)) {
      mysheets_fromexcel <- list()
      
      mysheetlist <- excel_sheets(path=input$file1$datapath)
      i=1
      for (i in 1:length(mysheetlist)){
        tempdf <- read_excel(path=input$file1$datapath, sheet = mysheetlist[i])
        tempdf$sheetname <- mysheetlist[i]
        mysheets_fromexcel[[i]] <- tempdf 
      }
      df1 <- do.call(rbind.data.frame, mysheets_fromexcel)
      return(mysheets_fromexcel)
    }
    else {
      return(NULL)
    }
  })
  
  getData <- reactive({
    
    inFile3 <- input$file3
    
    if (is.null(inFile3))
    {
      returnValue()
    }
    else
    {
      data<-read_xlsx(inFile3$datapath)
      if(nrow(data) > 22)
      {
        shinyalert("Column Error","Uploaded Data has more than 20 locations",type="error")
        returnValue()
        reset("file3")
      }
      
      else
      {
        return(data)
      }
    }
    
  })
  
  output$intable <- renderDataTable(getData(),options = list(
    autoWidth = FALSE, scrollX = T))
  # output$dd <- renderDataTable(dataset())
  
  observe({
    if (!is.null(input$file1)) {
    df_email <- getData()
    df1 <- dataset()
    list_df <- dataset2()
    df1$Shift <- as.numeric(as.factor(df1$Shift))
    df1$sheetname <- as.numeric(as.factor(df1$sheetname))
    shifft <- length(unique(df1$Shift))
    labor <- length(unique(df1$sheetname))
    day1 <- ncol(df1)
    
    score <- function(lab, shi, day) {
      out <- df1[df1$sheetname==lab & df1$Shift==shi,][day]
      print(as.integer(out))
    }
    
    model <- MIPModel() %>%
      
      # 1 iff labor i is assigned to shift j in k day
      add_variable(x[i, j, k], i = 1: labor, j = 1:shifft, k = 2:8, type = "binary") %>%
      
      # maximize the preferences
      set_objective(sum_expr(score(i, j, k) * x[i, j, k], i = 1:labor, j =1:shifft, k = 2:8)) %>%
      
      
      add_constraint(sum_expr(x[i, j, k], j =1:shifft) <= input$shi, i = 1:labor, k = 2:8)  %>%
      
      
      add_constraint(sum_expr(x[i,j,k], k = 2:8) <= 2, j = 1:shifft, i = 1:labor) %>%
      
      
      add_constraint(sum_expr(x[i,j,k], i = 1:labor) <= input$lab, k = 2:8, j = 1:shifft)
    
    result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))
    
    matching <- result %>% 
                get_solution(x[i,j,k]) %>%
                filter(value > .9) %>%
                select(i, j, k)
                                # rowwise() %>%
                                # mutate(score = score(as.numeric(i), as.numeric(j), as.numeric(k))))
                                
    
    df2 <- do.call(rbind.data.frame,list_df)
    df2$Shiftn <- as.numeric(as.factor(df2$Shift))
    df2$sheetnamen <- as.numeric(as.factor(df2$sheetname))

    df3 <- df2 %>%
      select(sheetname,sheetnamen) %>%
      rename(i=sheetnamen)

    df4 <- df2 %>%
      select(Shift,Shiftn) %>%
      rename(j=Shiftn)

    df5 <- merge(matching, unique(df3), by="i", all.x = T)
    df5 <- merge(df5, unique(df4), by="j", all.x = T)
    df5$Day <- ifelse(df5$k==2, "Monday",
                      ifelse(df5$k==3, "Tuesday",
                             ifelse(df5$k==4, "Wednesday",
                                    ifelse(df5$k==5, "Thursday",
                                           ifelse(df5$k==6, "Friday",
                                                  ifelse(df5$k==7, "Saturday", "Sunday"))))))
    
    df6 <- df5 %>% 
        rename(
        Name = sheetname
      )
    
    # df7 <- merge(df6, df_email,by="Name", all.x=T)
    if (!is.null(input$file3)) {
      
      df7 <- aggregate(Day ~ Name, data = df6, toString)
      df8 <- aggregate(Shift ~ Name, data = df6, toString)
      df9 <- merge(df7, df8, by="Name", all.x=T)
      df10 <- merge(df9, df_email,by="Name", all.x=T)

    }
    else {
      return(NULL)
    }

    }
    
    else {
      return(NULL)
    }
    
    
    
    observeEvent(input$goButton, {
      if (is.null(input$file1)) return()
      output$dd <- renderDataTable(df10)
      output$val1  <- renderText(objective_value(result))
      output$val2  <- renderText(solver_status(result))
    })
   
    
    output$download <- downloadHandler(
      filename = function(){"Optimized Plan.xlsx"},
      content = function(fname){
        write_xlsx(df10,  path = fname)
      }
    )
    
  })
  
  
}