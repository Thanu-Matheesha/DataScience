load("data_file.RData")
library(DT)

function(input, output,session) {
  
  updateSelectizeInput(session, 'selectInput', choices = c('Migraine','Kidney disease','Diabetes','Depression','Cancer','Muscular dystrophy','Wheeze','Heart attack','Arthritis'), server = TRUE)
 
  filterData <- reactive({
    data_1[which(data_1$Name_Of_disease == input$selectInput),]
    
  })
  
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
  
  output$tab1 <- DT::renderDataTable({
    DT::datatable(filterData(),selection="single",rownames = F)
    # DT=vals$Data
   # editable = 'cell'
    # 
    # options = list(
    #   pageLength = 5, autoWidth = TRUE
    # )
    # datatable(DT, escape=F)
  })
  
  observeEvent(input$manage_1, {
   print('ushan')
    
  })

  
  
  
}
