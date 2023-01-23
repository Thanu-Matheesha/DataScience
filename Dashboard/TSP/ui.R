library(shiny)
library(shinydashboard)
library(dygraphs)
library(shinydashboardPlus)
library(leaflet)
library(DT)
library(shinycssloaders)
# library(plotly)

dashboardPage(dashboardHeader(title="Route Optimization"),
              dashboardSidebar(div(style="overflow-y: scroll"),
                               sidebarMenu(
                                 menuItem("Route Optimization", icon = icon("bar-chart-o"), tabName = "Optim1")
                                
                               )),
              dashboardBody(
                tabItems(
                  tabItem(tabName = "Optim1",
                          fluidPage(
                          column(width=12,
                                 fluidRow(
                                   fileInput('file1', 'Insert File', accept = c(".csv")),
                                   actionButton("goButton", "Optimize!",icon("paper-plane"), 
                                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                   hr(),
                                   # textInput('file1sheet','Name of Sheet (Case-Sensitive)'),
                                   leafletOutput("map1")
                                        ),
                                
                                 hr(),
                                 fluidRow(
                                   column(width=3,
                                                 gradientBox(
                                                   title = "Total Distance",
                                                   width = 12,
                                                   icon = "fa fa-road",
                                                   gradientColor = "blue", 
                                                   boxToolSize = "xs", 
                                                   closable = FALSE,
                                                   h2(textOutput("val1"),"KM")
                                                 )),
                                          column(width=3,
                                                 gradientBox(
                                                   title = "Total Time Spend",
                                                   width = 12,
                                                   icon = "fa fa-clock",
                                                   gradientColor = "green", 
                                                   boxToolSize = "xs", 
                                                   closable = FALSE,
                                                   h2(textOutput("val2"),"Hours")
                                                 )),
                                   column(width=3,
                                          gradientBox(
                                            title = "Number of locations",
                                            width = 12,
                                            icon = "fa fa-city",
                                            gradientColor = "yellow", 
                                            boxToolSize = "xs", 
                                            closable = FALSE,
                                            h2(textOutput("val3"))
                                          )),
                                   column(width=3,
                                          gradientBox(
                                            title = "Quantity in CBM",
                                            width = 12,
                                            icon = "fa fa-box",
                                            gradientColor = "purple", 
                                            boxToolSize = "xs", 
                                            closable = FALSE,
                                            h2(textOutput("val4"))
                                          ))
                                   
                                 
                                 ),
                                 
                                 hr(),
                                 
                                 fluidRow(
                                   column(width=9,
                                          downloadButton('download',"Download the file"),
                                          hr(),
                                          dataTableOutput("table")
                                     
                                   )
                                 )
                          
                          )
                          )
                          )
                  )
                )
              )