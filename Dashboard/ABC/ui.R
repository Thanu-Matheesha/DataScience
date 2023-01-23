library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(shinycssloaders)
library(shinyalert)
library(shinyjs)
library(htmltools)

dashboardPage(dashboardHeader(title="Route Optimization", disable = T),
              dashboardSidebar(div(style="overflow-y: hidden"),
                               sidebarMenu(
                                 menuItem("Data Input", icon = icon("table"), tabName = "Optim1"),
                                 menuItem("Distance Matrix Calculation", icon = icon("route"), tabName = "Optim2"),
                                 menuItem("Route Optimization", icon = icon("map-marked-alt"), tabName = "Optim3"),
                                 menuItem("Route Details", icon = icon("map-marker-alt"), tabName = "Optim4")
                                 
                               )),
              dashboardBody(tags$head(tags$style(
                # HTML('.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}')
                # type="text/css", "label.control-label, .selectize-control.single{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;},"
                
              )),
              # useShinyjs(),
              useShinyalert(),
              
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
                                            back_title = "Data Files Summary",
                                            fluidRow(
                                              fileInput('file1', '', accept = c(".csv")),
                                              align = "center"),
                                            column(width=12,
                                                   column(width=6,
                                                          fluidRow(selectInput("y_input5", label = ("Select Your Date Column"),"")),
                                                          fluidRow(selectInput("y_input6", label = ("Select Your Item Code Column"),"")),
                                                          fluidRow(selectInput("y_input1", label = ("Select Your Item Description Column"),""))
                                                          # dataTableOutput("intable", width = "100%"))
                                                   ),
                                                   column(width=6,
                                                          fluidRow(selectInput("y_input2", label = ("Select Your Value Column"),"")),
                                                          fluidRow(selectInput("y_input3", label = ("Select Your Voume Column"),""))
                                                          # fluidRow(selectInput("y_input4", label = ("Select Your  Column"),""))
                                                   )),
                                            
                                            column(width=6,
                                                   column(width=6,
                                                          fluidRow(actionButton("goButton2", "Map Your Column",icon("cogs"),
                                                                                style="color: #fff; background-color: #FFDF00; border-color: #FFDF00"),align = "left"))),
                                            
                                            
                                            
                                            back_content = tagList(
                                              gradientBox(
                                                title = "Order file summary",
                                                width = 12,
                                                icon = "fa fa-data",
                                                gradientColor = "maroon", 
                                                boxToolSize = "xs", 
                                                closable = T,
                                                collapsed=TRUE,
                                                column(
                                                  width = 12,
                                                  descriptionBlock(
                                                    number = h3(textOutput("val1")), 
                                                    number_color = "black", 
                                                    number_icon = "fa fa-caret-up",
                                                    header = "", 
                                                    text = h3("Total Missing Values"), 
                                                    right_border = TRUE,
                                                    margin_bottom = FALSE
                                                  )
                                                ),
                                                
                                                hr(),
                                                hr(),
                                                footer =
                                                  boxPlus(
                                                    title = "Data Summary", 
                                                    closable = TRUE, 
                                                    width = NULL,
                                                    enable_label = TRUE,
                                                    label_text = 1,
                                                    label_status = "danger",
                                                    status = "warning", 
                                                    solidHeader = FALSE, 
                                                    collapsible = TRUE,
                                                    collapsed=FALSE,
                                                    fluidRow(
                                                      column(
                                                        width=12,
                                                        column(width=12,
                                                               column(width=12,
                                                                      dataTableOutput("intable", width = "100%"))
                                                        )
                                                      )
                                                    ))
                                                
                                                )
                                              
                                              )
                                          )
                                   )
                                 )
                          )
                          
                          
                          
                                )
                      ),
                
                
##################################################################################################################                
                
                tabItem(tabName = "Optim2",
                        fluidPage(
                          column(width=12,
                                 fluidRow(
                                   column(width=12,""))
                          )
                        )
                )
                    )
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              )
              )