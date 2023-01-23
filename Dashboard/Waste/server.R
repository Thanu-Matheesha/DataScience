setwd("~/KPMG/Dashboard/Waste")
Man <-read.csv("Manufacturing.csv", na.strings = c("","NA"), stringsAsFactors = FALSE)
Sou <-read.csv("Sourcing.csv", na.strings = c("","NA"), stringsAsFactors = FALSE)
Spe <- read.csv("Unit Spec.csv", na.strings = c("","NA"), stringsAsFactors = FALSE)
Man <- Man[-c(35),]
library(shiny)
library(dygraphs)
library(forecast)
library(shinyjs)
library(reshape2)
library(DT)

function(input, output, session){
  
  # create the forcasting heirachy
  output$selectlevel <- renderUI({
    radioButtons('selectlevel',"Select Level", choices = c("Main Category"="cat1",
                                                           "Sub Category"="cat2",
                                                           "Article"="item"),selected = "cat2",inline = T)
    output$selectcat_1 <- renderUI({
      selectInput('cat1','Select Category 1',choices=c(as.character(unique(Man$Main.Category))),selected = "M1") 
    })
    
    output$selectcat_2 <- renderUI({
      selectInput('cat2','Select Category 2',choices=c(as.character(unique(Man$Sub.Category)))) 
    })
    
    output$selectitem <- renderUI({
      selectInput('item','Select Item',choices=c(as.character(unique(Man$Material.ID)))) 
    })
    
  })

}