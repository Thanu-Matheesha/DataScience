
library(shiny)
library(shinydashboard)


shinyServer(function(input,output){
  
  output$contents <- renderTable({
    inFile <- input$file1
    
    if (is.null(inFile))
    {
      returnValue()
    }
    else
    {
      data<-read.csv(inFile$datapath)
      if(ncol(data)<5)
      {
        shinyalert("Column Error","Uploaded Data has less than 5 Col",type="error")
        returnValue()
      }
      else if(ncol(data)>5)
      {
        shinyalert("Column Error","Uploaded Data has more than 5 Col",type = "error")
        returnValue()
      }
      else
      {
        return(data)
      }
    }
    
  })
  
})