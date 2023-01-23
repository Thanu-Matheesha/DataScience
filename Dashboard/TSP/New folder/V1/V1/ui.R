library(shiny)
library(shinydashboard)
library(dygraphs)
library(shinydashboardPlus)
library(leaflet)
library(DT)
library(shinycssloaders)
library(shinyalert)
library(shinyjs)
library(leaftime)
library(htmltools)
# library(plotly)

dashboardPage(dashboardHeader(title="Route Optimization"),
              dashboardSidebar(div(style="overflow-y: scroll"),
                               sidebarMenu(
                                 menuItem("Data Input", icon = icon("table"), tabName = "Optim1"),
                                 menuItem("Distance Matrix Calculation", icon = icon("route"), tabName = "Optim2"),
                                 menuItem("Route Optimization", icon = icon("map-marked-alt"), tabName = "Optim3"),
                                 menuItem("Route Details", icon = icon("map-marker-alt"), tabName = "Optim4")
                                
                               )),
              dashboardBody(
                tabItems(
                  tabItem(tabName = "Optim1",
                          fluidPage(
                          column(width=12,
                                 fluidRow(
                                   fileInput('file1', 'Insert File', accept = c(".xlsx"))),
                                 hr(),
                                 fluidRow(
                                   column(width=12,
                                          flipBox(
                                            id = 1,
                                            main_img = "https://image.flaticon.com/icons/svg/1077/1077114.svg",
                                            header_img = "https://image.flaticon.com/icons/svg/1908/1908681.svg",
                                            front_title = "Input Data",
                                            back_title = "Summary",
                                            column(width=12,
                                            dataTableOutput("intable", width = "100%")),
                                            
                                            back_content = tagList(
                                              column(width=4,
                                                     gradientBox(
                                                       title = "Total Missing Values",
                                                       width = 12,
                                                       icon = "fa fa-exclamation-circle",
                                                       gradientColor = "red", 
                                                       boxToolSize = "xs", 
                                                       closable = FALSE,
                                                       h2(textOutput("val1"))
                                                     )
                                              ),
                                              column(width=4,
                                                     gradientBox(
                                                       title = "Missing Values in Link column",
                                                       width = 12,
                                                       icon = "fa fa-exclamation",
                                                       gradientColor = "blue", 
                                                       boxToolSize = "xs", 
                                                       closable = FALSE,
                                                       h2(textOutput("val2"))
                                                     )
                                              ),
                                              column(width=4,
                                                     gradientBox(
                                                       title = "Missing Values in Quantity column",
                                                       width = 12,
                                                       icon = "fa fa-exclamation-triangle",
                                                       gradientColor = "green", 
                                                       boxToolSize = "xs", 
                                                       closable = FALSE,
                                                       h2(textOutput("val3"))
                                                     )
                                              )
                                              # column(width=12,
                                              #        gradientBox(
                                              #          title = "Number of locations",
                                              #          width = 12,
                                              #          icon = "fa fa-city",
                                              #          gradientColor = "yellow", 
                                              #          boxToolSize = "sm", 
                                              #          closable = FALSE,
                                              #          footer = withSpinner(dataTableOutput("subdata"), type = 6)
                                              #        )
                                              # )
                                            )
                                          )
                                        ))
                               
                              )
                              )
                        ),
                  ######TAB2
                  tabItem(tabName = "Optim2",
                          fluidPage(
                            column(width=12,
                                   fluidRow(
                                     actionButton("goButton1", "Calculate Distance Matrix",icon("cogs"), 
                                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                     hr(),
                                     gradientBox(
                                       title = "Distance Matrix",
                                       width = 12,
                                       icon = "fa fa-th",
                                       gradientColor = "teal", 
                                       boxToolSize = "sm", 
                                       closable = F,
                                       footer = withSpinner(dataTableOutput("mat1"), type = 6)
                                      
                                     )
                                     
                                    
                                   )
                            )
                          )),
                  ####TAB3
                  tabItem(tabName = "Optim3",
                          fluidPage(
                            column(width=12,
                                   fluidRow(
                                     actionButton("goButton2", "Optimize!",icon("cogs"), 
                                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                     hr(),
                                     box(
                                     title = "Map", background = "maroon", solidHeader = TRUE, width = 12,
                                     leafletOutput("map1")
                                        ),
                                     
                                     hr(),
                                     downloadButton('download',"Download the file"),
                                     hr(),
                                     gradientBox(
                                       title = "Optimize Route Details",
                                       width = 12,
                                       icon = "fa fa-th",
                                       gradientColor = "teal", 
                                       boxToolSize = "sm", 
                                       closable = F,
                                       footer = dataTableOutput("dwtable", width = "100%")
                                     )
                                     
                                   )
                            )
                          )),
                  #TAB4########################################################
                  tabItem(tabName = "Optim4",
                          fluidPage(
                            column(width=12,
                                   fluidRow(
                                     box(
                                       title = "Map", background = "blue", solidHeader = TRUE, width = 12,
                                       leafletOutput("map2")
                                     )
                                     
                                     
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
                                              h2(textOutput("val11"),"KM")
                                            )),
                                     column(width=3,
                                            gradientBox(
                                              title = "Total Time Spend",
                                              width = 12,
                                              icon = "fa fa-clock",
                                              gradientColor = "green", 
                                              boxToolSize = "xs", 
                                              closable = FALSE,
                                              h2(textOutput("val12"),"Hours")
                                            )),
                                     column(width=3,
                                            gradientBox(
                                              title = "Number of locations",
                                              width = 12,
                                              icon = "fa fa-city",
                                              gradientColor = "yellow", 
                                              boxToolSize = "xs", 
                                              closable = FALSE,
                                              h2(textOutput("val13"))
                                            )),
                                     column(width=3,
                                            gradientBox(
                                              title = "Quantity in CBM",
                                              width = 12,
                                              icon = "fa fa-box",
                                              gradientColor = "purple", 
                                              boxToolSize = "xs", 
                                              closable = FALSE,
                                              h2(textOutput("val14"))
                                            ))
                                     
                                     
                                   )
                  
                                   
                            )
                          )
                  )
                  
                )
              )
)