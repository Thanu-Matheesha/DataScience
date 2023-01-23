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

dashboardPage(dashboardHeader(title="Route Optimization", disable = T),
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
                                   column(width=12,
                                          flipBox(
                                            id = 1,
                                            main_img = "https://image.flaticon.com/icons/svg/630/630746.svg",
                                            header_img = "https://image.flaticon.com/icons/svg/630/630746.svg",
                                            front_title = "Please upload order details in .xlsx format",
                                            back_title = "Data Quality Analysis",
                                            fluidRow(
                                              fileInput('file1', '', accept = c(".xlsx")),
                                              align = "center"),
                                            column(width=12,
                                                   column(width=12,
                                            dataTableOutput("intable", width = "100%"))),
                                            
                                            back_content = tagList(
                                              box(
                                                solidHeader = FALSE,
                                                title = "",
                                                background = NULL,
                                                width = 12,
                                                status = "danger",
                                                footer = fluidRow(
                                                  column(
                                                    width = 6,
                                                    descriptionBlock(
                                                      number = h3(textOutput("val1")), 
                                                      number_color = "red", 
                                                      number_icon = "fa fa-caret-up",
                                                      header = "", 
                                                      text = h3("Total Missing Values"), 
                                                      right_border = TRUE,
                                                      margin_bottom = FALSE
                                                    )
                                                  ),
                                                  column(
                                                    width = 6,
                                                    descriptionBlock(
                                                      number = h3(textOutput("val2")), 
                                                      number_color = "red", 
                                                      number_icon = "fa fa-caret-up",
                                                      header = "", 
                                                      text = h3("TOTAL MISSING LOCATIONS"), 
                                                      right_border = FALSE,
                                                      margin_bottom = FALSE
                                                    )
                                                  )
                                                )
                                              ),
                                              hr(),
                                              fluidRow(
                                                column(
                                                  width=12,
                                                  column(width=12,
                                                         dataTableOutput("natab", width = "100%")
                                                  )
                                                )
                                              )
                            
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
                                     column(width=12,"")),
                                     hr(),
                                   fluidRow(
                                     gradientBox(
                                       title = "Distance Matrix",
                                       width = 12,
                                       icon = "fa fa-th",
                                       gradientColor = "teal", 
                                       boxToolSize = "sm", 
                                       closable = F,
                                       footer = column(width=12,align="center",
                                                       fluidRow(actionButton("goButton1", "Press here to calculate Distance Matrix",icon("cogs"), 
                                                                             style="color: #fff; background-color: #FFDF00; border-color: #FFDF00")),
                                                       fluidRow(withSpinner(dataTableOutput("mat1")), type=6)
                                       )
                                      
                                     )
                                   )
                                    
                                   )
                            )
                          ),
                  ####TAB3
                  tabItem(tabName = "Optim3",
                          fluidPage(
                            column(width=12,
                                   fluidRow(
                                     # actionButton("goButton2", "Optimize!",icon("cogs"), 
                                     #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                     # hr(),
                                     box(
                                       actionButton("goButton2", "Press here to calculate the optimum route",icon("cogs"), 
                                                    style="color: #fff; background-color: #FFDF00; border-color: #FFDF00"),
                                       hr(),
                                     title = "Click play button to generate the optimum route", background = "teal", solidHeader = TRUE, width = 12,
                                     leafletOutput("map1")
                                        ),
                                     
                                    
                                     hr(),
                                     gradientBox(
                                       title = "Optimised Route Details",
                                       width = 12,
                                       icon = "fa fa-th",
                                       gradientColor = "teal", 
                                       boxToolSize = "sm", 
                                       closable = F,
                                       footer = dataTableOutput("dwtable", width = "100%")
                                     ),
                                     hr(),
                                     downloadButton('download',"Download the file")
                                     
                                   )
                            )
                          )),
                  #TAB4########################################################
                  tabItem(tabName = "Optim4",
                          fluidPage(
                            column(width=12,
                                   fluidRow(
                                     box(
                                       title = "Map", background = "teal", solidHeader = TRUE, width = 12,
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
                                              gradientColor = "teal", 
                                              boxToolSize = "xs", 
                                              closable = FALSE,
                                              h2(textOutput("val11"))
                                            )),
                                     column(width=3,
                                            gradientBox(
                                              title = "Total Time Spend",
                                              width = 12,
                                              icon = "fa fa-clock",
                                              gradientColor = "teal", 
                                              boxToolSize = "xs", 
                                              closable = FALSE,
                                              h2(textOutput("val12"))
                                            )),
                                     column(width=3,
                                            gradientBox(
                                              title = "Number of locations",
                                              width = 12,
                                              icon = "fa fa-city",
                                              gradientColor = "teal", 
                                              boxToolSize = "xs", 
                                              closable = FALSE,
                                              h2(textOutput("val13"))
                                            )),
                                     column(width=3,
                                            gradientBox(
                                              title = "Quantity in CBM",
                                              width = 12,
                                              icon = "fa fa-box",
                                              gradientColor = "teal", 
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