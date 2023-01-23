#setwd("~/KPMG/Dashboard/ABC2")
library(sp)
library(maptools)
library(dplyr)
library(shiny)
library(leaflet)
library(tidyverse)
library(DT)
library(dplyr)
library(readxl)
library(data.table)
library(leaftime)
library(htmltools)
library(ggplot2)

options(shiny.maxRequestSize=500*1024^2) #data file upload size

# kml.coordinates <- read_xlsx("Test1_2.xlsx")

function(input, output, session){
  
  
  output$selectlevel <- renderUI({
    radioButtons('selectlevel',"Select Analysis Type", choices = c("Value Wise (ABC)"="cat1",
                                                           "Volume Wise (XYZ)"="cat2",
                                                           "Frequency Wise (FMS)"="cat3"),selected = "cat1",inline = T)
  })
  
  getData <- reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile))
    {
      returnValue()
    }
    else
    {
      data<-read.csv(inFile$datapath)
      # if(nrow(data) > 150)
      # {
      #   shinyalert("Column Error","Uploaded Data has more than 150 locations",type="error")
      #   returnValue()
      #   reset("file1")
      # }
      # 
      # else
      # {
      #   return(data)
      # }
    }
   

    
  })
  
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
              Description=input$y_input1,
                   value=input$y_input2,
                   volume=input$y_input3)%>% select(Date, Item_Code, Description, value, volume)
              # Link=input$y_input4)
          if(is.numeric(kml.coordinatest$value)==F | is.numeric(kml.coordinatest$volume)==F)
          {
            shinyalert("Column Error","Value or Volume Column is not numeric",type="error")
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
      output$val1 <- renderText(sum(is.na(coordvals()$value)))
      output$val2 <- renderText(sum(is.na(coordvals()$volume)))
      output$natab <- renderDataTable(coordvals()[rowSums(is.na(coordvals())) > 0,],options = list(
        autoWidth = FALSE, scrollX = T))
    

  
  observe({
    if(is.null(input$selectlevel))
      return()
    
    
  if(input$selectlevel == "cat1"){  
  if (!is.null(coordvals())) {
      
      df1 <- coordvals()
     
   
    
    observeEvent(input$goButton1, {
      if (is.null(input$file1)) return()
      showModal(modalDialog(title = 'Calculating',withSpinner(tagList(renderText("")),type=5,color.background = '#FFFFFF',size = .25,proxy.height = '50px'),footer = NULL,size = 's'))
     
      df2 <- setNames(aggregate(df1$value,by=list(df1$Item_Code),FUN=sum),c("Item_Code", "value"))   
      df2_ABC <- ABCanalysis(df2$value, PlotIt = T)
      df2_ABC2 <- ABCanalysisPlot(df2$value)
      df2$percentage <-df2$value/sum(df2$value)
      df2 <- df2[order(-df2$percentage), ]
      df2$cum.sum <- cumsum(df2$percentage)
      df2$ABC <- ifelse( df2$cum.sum < df2_ABC$smallestAData, "A", 
                           ifelse(df2$cum.sum > df2_ABC$smallestBData, "C", "B" ))
      
      output$Ftable <- renderDataTable(df2)
      
      output$ABCpl <- renderPlot({
        ABCanalysisPlot(df2$value)
    
        })
      
    
      output$download <- downloadHandler(
        filename = function(){"Analysis.csv"},
        content = function(fname){
          write.csv(df2, fname, row.names = F)
        }
      )
      
      
      output$down <- downloadHandler(
        filename = function(){paste("plot",".pdf",sep=".") },
        content = function(file) {
          pdf(file)

          print(ABCanalysisPlot(df2$value))
          dev.off()
        }
      )
      
      # output$myImage <- renderImage({
      #   
      #   # outfile <- tempfile(fileext = '.png')
      #   
      #   png(df2_ABC, width = 400, height = 300)
      #   
      #   list(src = df2_ABC,
      #        contentType = 'image/png',
      #        width = 400,
      #        height = 300,
      #        alt = "This is alternate text")
      #   
      # }, deleteFile = TRUE)
      
      removeModal()
    })
    
  }

    
    else {
      return(NULL)
    }
  }
    
    ##XYZ###
    
    else if(input$selectlevel == "cat2"){  
      if (!is.null(coordvals())) {
        
        df1 <- coordvals()
        
        
        
        observeEvent(input$goButton1, {
          if (is.null(input$file1)) return()
          showModal(modalDialog(title = 'Calculating',withSpinner(tagList(renderText("")),type=5,color.background = '#FFFFFF',size = .25,proxy.height = '50px'),footer = NULL,size = 's'))
          
          df2 <- setNames(aggregate(df1$volume,by=list(df1$Item_Code),FUN=sum),c("Item_Code", "volume"))   
          df2_ABC <- ABCanalysis(df2$volume, PlotIt = T)
          df2_ABC2 <- ABCanalysisPlot(df2$volume)
          df2$percentage <-df2$volume/sum(df2$volume)
          df2 <- df2[order(-df2$percentage), ]
          df2$cum.sum <- cumsum(df2$percentage)
          df2$XYZ <- ifelse( df2$cum.sum < df2_ABC$smallestAData, "X", 
                             ifelse(df2$cum.sum > df2_ABC$smallestBData, "Z", "Y" ))
          
          output$Ftable <- DT::renderDataTable(datatable(df2, options = list(rownames= FALSE)))
          
          output$ABCpl <- renderPlot({
            ABCanalysisPlot(df2$volume)
            
          })
          
          output$download <- downloadHandler(
            filename = function(){"Analysis.csv"},
            content = function(fname){
              write.csv(df2, fname, row.names = F)
            }
          )
          
          output$down <- downloadHandler(
            filename = function(){paste("plot",".pdf",sep=".") },
            content = function(file) {
              pdf(file)
              
              print(ABCanalysisPlot(df2$volume))
              dev.off()
            }
          )
          
          removeModal()
        })
        
      }
      
      
      else {
        return(NULL)
      }
    }
    
    ####FMS#####
    
    else if(input$selectlevel == "cat3"){  
      if (!is.null(coordvals())) {
        
        df1 <- coordvals()
        
        
        
        observeEvent(input$goButton1, {
          if (is.null(input$file1)) return()
          showModal(modalDialog(title = 'Calculating',withSpinner(tagList(renderText("")),type=5,color.background = '#FFFFFF',size = .25,proxy.height = '50px'),footer = NULL,size = 's'))
          
          df2 <- setNames(aggregate(df1$Item_Code,by=list(df1$Item_Code),FUN=length),c("Item_Code", "frequency"))   
          df2_ABC <- ABCanalysis(df2$frequency, PlotIt = T)
          df2_ABC2 <- ABCanalysisPlot(df2$frequency)
          df2$percentage <-df2$frequency/sum(df2$frequency)
          df2 <- df2[order(-df2$percentage), ]
          df2$cum.sum <- cumsum(df2$percentage)
          df2$FMS <- ifelse( df2$cum.sum < df2_ABC$smallestAData, "F", 
                             ifelse(df2$cum.sum > df2_ABC$smallestBData, "S", "M" ))
          
          output$Ftable <- renderDataTable(df2)
          
          output$ABCpl <- renderPlot({
            ABCanalysisPlot(df2$frequency)
            
          })
          
          
          output$download <- downloadHandler(
            filename = function(){"Analysis.csv"},
            content = function(fname){
              write.csv(df2, fname, row.names = F)
            }
          )
          
          output$down <- downloadHandler(
            filename = function(){paste("plot",".pdf",sep=".") },
            content = function(file) {
              pdf(file)
              
              print(ABCanalysisPlot(df2$frequency))
              dev.off()
            }
          )

          removeModal()
        })
        
      }
      
      
      else {
        return(NULL)
      }
    }
    
    else
      return()
    
  })
  
    

 

}