setwd("~/DBL")
load("~/DBL/hts2.RData")
library(shiny)
library(dygraphs)
library(forecast)
library(shinyjs)
library(reshape2)
library(DT)

function(input, output, session){

  # create the forcasting heirachy
  output$selectlevel <- renderUI({
    radioButtons('selectlevel',"Select Level", choices = c("Category 1"="cat1",
                                                           "Category 2"="cat2",
                                                           "Item"="item",
                                                           "Area"="area",
                                                           "Dealer"="dealer"),selected = "cat2",inline = T)    
  })
  #lowest level category dealer
  output$selectcat_1 <- renderUI({
    selectInput('cat1','Select Category 1',choices=c(as.character(unique(Sales_Final$Sub.Category.1.y))),selected = "DBL") 
  })
  
  output$selectcat_2 <- renderUI({
    selectInput('cat2','Select Category 2',choices=c(as.character(unique(Sales_Final$Sub.Category.2.y)))) 
  })
  
  output$selectitem <- renderUI({
    selectInput('item','Select Item',choices=c(as.character(unique(Sales_Final$Item.Description.x)))) 
  })
  
  output$selectarea <- renderUI({
    selectInput('area','Select Area',choices=c(as.character(unique(Sales_Final$Area)))) 
  })
  
  output$selectdealer <- renderUI({
    selectInput('dealer','Select Dealer',choices=c(as.character(unique(Sales_Final$Name)))) 
  })
  
  #level category area
  output$selectcat_1_area <- renderUI({
    selectInput('cat1_area','Select Category 1',choices=c(as.character(unique(Sales_Final$Sub.Category.1.y))),selected = "DBL") 
  })
  
  output$selectcat_2_area <- renderUI({
    selectInput('cat2_area','Select Category 2',choices=c(as.character(unique(Sales_Final$Sub.Category.2.y)))) 
  })
  
  output$selectitem_area <- renderUI({
    selectInput('item_area','Select Item',choices=c(as.character(unique(Sales_Final$Item.Description.x)))) 
  })
  
  output$selectarea_area <- renderUI({
    selectInput('area_area','Select Area',choices=c(as.character(unique(Sales_Final$Area)))) 
  })
  
  #level category item
  output$selectcat_1_item <- renderUI({
    selectInput('cat1_item','Select Category 1',choices=c(as.character(unique(Sales_Final$Sub.Category.1.y))),selected = "DBL") 
  })
  
  output$selectcat_2_item <- renderUI({
    selectInput('cat2_item','Select Category 2',choices=c(as.character(unique(Sales_Final$Sub.Category.2.y)))) 
  })
  
  output$selectitem_item <- renderUI({
    selectInput('item_item','Select Item',choices=c(as.character(unique(Sales_Final$Item.Description.x)))) 
  })
  
  #level category cat_2
  output$selectcat_1_cat2 <- renderUI({
    selectInput('cat1_cat2','Select Category 1',choices=c(as.character(unique(Sales_Final$Sub.Category.1.y))),selected = "DBL") 
  })
  
  output$selectcat_2_cat2 <- renderUI({
    selectInput('cat2_cat2','Select Category 2',choices=c(as.character(unique(Sales_Final$Sub.Category.2.y)))) 
  })
  #level category cat_1
  output$selectcat_1_cat1 <- renderUI({
    selectInput('cat1_cat1','Select Category 1',choices=c(as.character(unique(Sales_Final$Sub.Category.1.y))),selected = "DBL") 
  })
  
  #overrides
  output$addoverride <- renderUI({
    radioButtons('addoverride','Override',c("Yes"="yes","No"="no"),inline=T) 
    
  })
  
  output$override1 <- renderUI({
    numericInput('override1','Month 1',min = 0,value = 0) 
    
  })
  output$override2 <- renderUI({
    numericInput('override2','Month 2',min = 0,value = 0) 
  })
  output$override3 <- renderUI({
    numericInput('override3','Month 3',min = 0,value = 0) 
  })
  output$override <- renderUI({
    actionButton("override","Submit Override", icon = NULL, width = "100%")
  })
  output$replenishment <- renderUI({
    actionButton("replenishment","Replenishment Planning", icon = NULL, width = "100%")
  })
  
  
  ## update inputs with the heirachy
  observe({
      cat1 <- as.character(unique(Sales_Final[as.character(Sales_Final$Sub.Category.1.y) == input$cat1_cat2,]$Sub.Category.1.x))
  })
  
  get_cat1_cat2 <- reactive({ Sales_Final[Sales_Final$Sub.Category.1.y==input$cat1_cat2,] }) 
  get_cat1_item <- reactive({ Sales_Final[Sales_Final$Sub.Category.1.y==input$cat1_item,] }) 
  get_cat1_area <- reactive({ Sales_Final[Sales_Final$Sub.Category.1.y==input$cat1_area,] }) 
  get_cat1_dealer <- reactive({ Sales_Final[Sales_Final$Sub.Category.1.y==input$cat1,] }) 
  
  
  observe({
      updateSelectInput(session,"cat2_cat2",choices = unique(get_cat1_cat2()$Sub.Category.2.y))
      updateSelectInput(session,"cat2_item",choices = unique(get_cat1_item()$Sub.Category.2.y))
      updateSelectInput(session,"cat2_area",choices = unique(get_cat1_area()$Sub.Category.2.y))
      updateSelectInput(session,"cat2",choices = unique(get_cat1_dealer()$Sub.Category.2.y))
  })
  
  get_cat2_item <- reactive({ Sales_Final[Sales_Final$Sub.Category.2.y==input$cat2_item,] }) 
  get_cat2_area <- reactive({ Sales_Final[Sales_Final$Sub.Category.2.y==input$cat2_area,] }) 
  get_cat2_dealer <- reactive({ Sales_Final[Sales_Final$Sub.Category.2.y==input$cat2,] })
  
  observe({
    updateSelectInput(session,"item_item",choices = unique(get_cat2_item()$Item.Description.x))
    updateSelectInput(session,"item_area",choices = unique(get_cat2_area()$Item.Description.x))
    updateSelectInput(session,"item",choices = unique(get_cat2_dealer()$Item.Description.x))
  })
  
  get_item_area <- reactive({Sales_Final[Sales_Final$Item.Description.x == input$item_area,] }) 
  get_item_dealer <- reactive({ Sales_Final[Sales_Final$Item.Description.x == input$item,] })
  
  observe({
    updateSelectInput(session,"area_area",choices = unique(get_item_area()$Area))
    updateSelectInput(session,"area",choices = unique(get_item_dealer()$Area))
  })
  
  get_area_dealer <- reactive({ Sales_Final[Sales_Final$Area == input$area,] })
  
  observe({
    updateSelectInput(session,"dealer",choices = unique(get_area_dealer()$Name))
  })
  
  
   observe({
     if(is.null(input$selectlevel))
     return()
     if(input$selectlevel == "cat1"){
     key <- paste("G1/",as.character(unique(Sales_Final[as.character(Sales_Final$Sub.Category.1.y)== input$cat1_cat1,]$Sub.Category.1)),sep="")
     }
     else if(input$selectlevel == "cat2"){
     key <- paste("G2/",as.character(unique(Sales_Final[Sales_Final$Sub.Category.1.y== input$cat1_cat2,]$Sub.Category.1.x)),
                       as.character(unique(Sales_Final[as.character(Sales_Final$Sub.Category.2.y)==input$cat2_cat2,]$Sub.Category.2)),sep = "")
     }
     else if(input$selectlevel == "item"){
     key <- paste("G3/",as.character(unique(Sales_Final[as.character(Sales_Final$Sub.Category.1.y)== input$cat1_item,]$Sub.Category.1)),
                       as.character(unique(Sales_Final[as.character(Sales_Final$Sub.Category.2.y)==input$cat2_item,]$Sub.Category.2)),
                       as.character(unique(Sales_Final[as.character(Sales_Final$Item.Description)==input$item_item,]$Item.Code)),sep = "")
     }
     else if(input$selectlevel == "area"){
     key <- paste("G10/",as.character(unique(Sales_Final[as.character(Sales_Final$Sub.Category.1.y)== input$cat1_area,]$Sub.Category.1)),
                       as.character(unique(Sales_Final[as.character(Sales_Final$Sub.Category.2.y)==input$cat2_area,]$Sub.Category.2)),
                       as.character(unique(Sales_Final[as.character(Sales_Final$Item.Description)==input$item_area,]$Item.Code)),
                       as.character(unique(Sales_Final[as.character(Sales_Final$Area)==input$area_area,]$Area)),sep = "")
     }
     else if(input$selectlevel == "dealer"){
     key <- paste(as.character(unique(Sales_Final[as.character(Sales_Final$Sub.Category.1.y)== input$cat1,]$Sub.Category.1)),
                         as.character(unique(Sales_Final[as.character(Sales_Final$Sub.Category.2.y)==input$cat2,]$Sub.Category.2)),
                         as.character(unique(Sales_Final[as.character(Sales_Final$Item.Description.x)==input$item,]$Item.Code)),
                         as.character(unique(Sales_Final[as.character(Sales_Final$Area)==input$area,]$Area)),
                         as.character(unique(Sales_Final[as.character(Sales_Final$Name)==input$dealer,]$new.code)),sep = "")
     
     }
     else
       return()
    output$key <- renderText({key})
    if(key %in% colnames(ally)){
      a <- ally[,key]
      f <- all_f_hts[,key]
      t <- decompose(a)$trend
      s <- decompose(a)$seasonal
      e <- auto.arima(a)
      e <- forecast(e,h = 6)
      e <- accuracy(e)
      overrides <- read.csv("override.csv")
      if(key %in% overrides$key){
        overrides <- overrides[overrides$key==key,]
        overrides <- overrides[order(overrides$Year_mon),] 
        ots <- ts(overrides$override,start=c(overrides$year[1],overrides$month[1]),frequency=12)
      }
      else
        ots <- NULL
      
      ft <- setNames(data.frame(time(f)),c("Year_mon"))
      ft$override <- c(input$override1,input$override2,input$override3,0,0,0)
      ft$year <- floor(ft$Year_mon/1)*1
      ft$month <- round(((ft$Year_mon - ft$year)*12)+1,digits = 0)
      ft$key <- key
      o <- ts(ft$override,start =c(ft$year[1],ft$month[1]),frequency = 12)
      observeEvent(input$override,{
        if(is.null(ots)){
          overrides <- rbind(overrides,ft)
        }
        else{
          overrides <- overrides[overrides$key!=key,]
          overrides <- rbind(overrides,ft)  
        }
        write.csv(overrides,"override.csv", row.names = F)
        session$reload()
      })
    }
    else
      return()
    if(is.null(ots)){
      fcast <- cbind(a,f,t,s,o)
      output$dygraph<- renderDygraph({
        dygraph(fcast)%>%
          dySeries("a",label="Actual",fillGraph = TRUE)%>%
          dySeries("f",label="Forecast",fillGraph = TRUE)%>%
          dySeries("t",label="Trend",fillGraph = TRUE)%>%
          dySeries("s",label="Seasonal",fillGraph = TRUE)%>%
          dySeries("o",label="Override",fillGraph = TRUE)%>%
          dyRangeSelector(height = 20)%>%
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))
        
      })
    }
    else{
      fcast <- cbind(a,f,t,s,o,ots)
      output$dygraph<- renderDygraph({
        dygraph(fcast)%>%
          dySeries("a",label="Actual",fillGraph = TRUE)%>%
          dySeries("f",label="Forecast",fillGraph = TRUE)%>%
          dySeries("t",label="Trend",fillGraph = TRUE)%>%
          dySeries("s",label="Seasonal",fillGraph = TRUE)%>%
          dySeries("o",label="Override",fillGraph = TRUE)%>%
          dySeries("ots",label="Manual Override",fillGraph = TRUE)%>%
          dyRangeSelector(height = 20)%>%
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))
        
      })
    }
    output$mae <-renderInfoBox({infoBox(title = tags$b("Absolute Error"),value=format(round(e[3],digits = 2),big.mark = ","),color = "yellow")})
    output$mase <- renderInfoBox({infoBox(title = tags$b("Scaled Error"),format(round(e[6],digits = 2),big.mark = ","),color = "yellow")})
    output$mape <- renderInfoBox({infoBox(title = tags$b("Percentage Error"),paste(format(round(e[5]),big.mark = ","),"%",sep = ""),color = "yellow")})
    
 })
   
 #### inventory analysis
   output$selectcat_inventory_1 <- renderUI({
     selectInput('cat1_inventory','Select Category 1',choices=c(as.character(unique(abc$Sub.Category.1.y)))) 
   })
   
   output$selectcat_inventory_2 <- renderUI({
     selectInput('cat2_inventory','Select Category 2',choices=c(as.character(unique(abc$Sub.Category.2.y)))) 
   })   
 
   get_cat1 <- reactive({
      abc[abc$Sub.Category.1.y == input$cat1_inventory,]
   })
   observe({
     if(is.null(input$cat1_inventory))
       return()
     updateSelectInput(session,"cat2_inventory",choices = unique(get_cat1()$Sub.Category.2.y))
   })
   observe({
     if(is.null(input$cat1_inventory))
       return()
     #abc_subset <- abc[as.character(abc$Sub.Category.2.y)==input$cat2_inventory,]
     abc_subset <- abc
     abc_table <- abc_subset
     abc_number <- setNames(aggregate(abc_subset$Item.Code,by=list(abc_subset$ABC),length),c("ABC","Number_of_Items"))
     abc_subset <- abc_subset[,c("ABC","Quantity","Value","Volume","Inventory")]
     abc_subset <- melt(abc_subset,id="ABC")
     abc_subset <- dcast(abc_subset, ABC  ~ variable ,sum)
     abc_subset <- merge(abc_number,abc_subset,by="ABC",all=T)
     abc_subset$Number_of_Items <- abc_subset$Number_of_Items*100/sum(abc_subset$Number_of_Items)
     abc_subset$Quantity <- abc_subset$Quantity*100/sum(abc_subset$Quantity)
     abc_subset$Value <- abc_subset$Value*100/sum(abc_subset$Value)
     abc_subset$Volume <- abc_subset$Volume*100/sum(abc_subset$Volume)
     abc_subset$Inventory <- abc_subset$Inventory*100/sum(abc_subset$Inventory)
     colnames(abc_subset) <- c("ABC","Number_of_Items","Sales_Volume","Sales_Value","Inventory_Volume","Inventory_Value")
     abc_subset <- melt(abc_subset,id="ABC")
     
     output$inventory <- renderPlotly({
       plot_ly(abc_subset,
                x= ~value,
                y= ~variable,
                type="bar",
                orientation = 'h',
                name=~ABC,
                color=~ABC
              )%>%
         layout(yaxis = list(title = 'ABC Analysis'), barmode = 'stack')
     })
     abc_table <- abc[as.character(abc$Sub.Category.2.y)==input$cat2_inventory,]
     abc_table <- abc_table[,c("Item.Code","Item.Description","ABC","Value","Quantity","Volume","Inventory")]
     output$inventory_table <- DT::renderDataTable({
       datatable(abc_table,
                 colnames = c("Item.Code","Item.Description","ABC","Sales Value","Sales Volume","Inventory Volume","Inventory Value") ,
                 options = list(scrollX = TRUE, searching=FALSE,
                                pageLength = 25, bLengthChange=FALSE
                 ))%>%
         formatRound('Quantity',2)%>%
         formatRound('Volume',2)%>%
         formatCurrency('Value',currency="LKR ")%>%
         formatCurrency('Inventory',currency="LKR ")
     })
     
   })
 
}