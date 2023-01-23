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
              dashboardSidebar(div(style="overflow-y: hidden"),
                               sidebarMenu(
                                 menuItem("Data Input", icon = icon("table"), tabName = "Optim1"),
                                 menuItem("Distance Matrix Calculation", icon = icon("route"), tabName = "Optim2"),
                                 menuItem("Route Optimization", icon = icon("map-marked-alt"), tabName = "Optim3"),
                                 menuItem("Route Details", icon = icon("map-marker-alt"), tabName = "Optim4")
                                
                               )),
              dashboardBody(tags$head(tags$style(
                HTML('.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}')
                # type="text/css", "label.control-label, .selectize-control.single{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;},"
                
              )),
              # useShinyjs(),
              useShinyalert(),
                tabItems(
                  tabItem(tabName = "Optim1",
                          fluidPage(
                            ##Var Input
                            # column(width=12,
                            #        fluidRow(actionButton("goButton2", "Map Your Column",icon("cogs"),
                            #        style="color: #fff; background-color: #FFDF00; border-color: #FFDF00")),
                            #        hr()
                                   # column(width=12, 
                                   #        box(
                                   #          title = "",
                                   #          width = NULL,
                                   #          accordion(
                                   #            accordionItem(
                                   #              id = 1,
                                   #              title = "",
                                   #              color = "danger",
                                   #              collapsed = F,
                                   #              selectInput("y_input1", label = ("Select Your Column"),"")
                                   #                                                      
                                   #            ),
                                   #            accordionItem(
                                   #              id = 2,
                                   #              title = "",
                                   #              color = "warning",
                                   #              collapsed = FALSE,
                                   #              selectInput("y_input2", label = ("Select Your Column"),"")
                                   #            ),
                                   #            accordionItem(
                                   #              id = 3,
                                   #              title = "",
                                   #              color = "info",
                                   #              collapsed = FALSE,
                                   #              selectInput("y_input3", label = ("Select Your Column"),"")
                                   #            ),
                                   #            accordionItem(
                                   #              id = 4,
                                   #              title = "",
                                   #              color = "danger",
                                   #              collapsed = FALSE,
                                   #              selectInput("y_input4", label = ("Select Your Column"),"")
                                   #            )
                                   #            
                                   #            )))
                                          
                            # ),
                            ###
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
                                              fileInput('file1', '', accept = c(".xlsx")),
                                              align = "center"),
                                            column(width=12,
                                                   column(width=6,
                                                          fluidRow(selectInput("y_input5", label = ("Select Your Customer Name Column"),"")),
                                                          fluidRow(selectInput("y_input6", label = ("Select Your Order Number Column"),"")),
                                                          fluidRow(selectInput("y_input1", label = ("Select Your Location Type Column"),""))
                                            # dataTableOutput("intable", width = "100%"))
                                            ),
                                            column(width=6,
                                                   fluidRow(selectInput("y_input2", label = ("Select Your Quantity Column"),"")),
                                                   fluidRow(selectInput("y_input3", label = ("Select Your Location Column"),"")),
                                                   fluidRow(selectInput("y_input4", label = ("Select Your Link Column"),""))
                                            )),
                                            
                                            column(width=6,
                                                   column(width=6,
                                                   fluidRow(actionButton("goButton2", "Map Your Column",icon("cogs"),
                                                                         style="color: #fff; background-color: #FFDF00; border-color: #FFDF00"),align = "left"))),
                                                   hr(),
                                            
                                            
                                            fluidRow(
                                              
                                              box(
                                                solidHeader = FALSE,
                                                title = "Please upload Distribution Center details in .xlsx format",
                                                background = NULL,
                                                width = 12,
                                                status = "danger",
                                                footer = fluidRow(
                                                  fluidRow(
                                                    fileInput('file2', '', accept = c(".xlsx")),
                                                    align = "center"),
                                                  column(width=12,
                                                         column(width=12,
                                                         column(width=6,
                                                                fluidRow(selectInput("y_input7", label = ("Select Your DC Number Column"),"")),
                                                                fluidRow(selectInput("y_input8", label = ("Select Your DC Adress Column"),""))
                                                              
                                                                # dataTableOutput("intable", width = "100%"))
                                                         ),
                                                         column(width=6,
                                                                fluidRow(selectInput("y_input9", label = ("Select Your DC Latitude Column"),"")),
                                                                fluidRow(selectInput("y_input10", label = ("Select Your DC Longitude Column"),""))
                                                         
                                                         )))
                                                  
                                                )
                                              )
                         
                                            ),
                                            
                                            hr(),
                                            fluidRow(
                                              
                                              box(
                                                solidHeader = FALSE,
                                                title = "Please upload Vehicle details in .xlsx format",
                                                background = NULL,
                                                width = 12,
                                                status = "danger",
                                                footer = fluidRow(
                                                  fluidRow(
                                                    fileInput('file3', '', accept = c(".xlsx")),
                                                    align = "center"),
                                                  column(width=12,
                                                         column(width=12,
                                                                column(width=6,
                                                                       fluidRow(selectInput("y_input11", label = ("Select Your Vehicle Number Column"),"")),
                                                                       fluidRow(selectInput("y_input12", label = ("Select Your Vehicle Capacity Column"),""))
                                                                       
                                                                       # dataTableOutput("intable", width = "100%"))
                                                                ),
                                                                column(width=6,
                                                                       fluidRow(selectInput("y_input13", label = ("Select Your Vehicle Type Column"),"")),
                                                                       fluidRow(selectInput("y_input14", label = ("Select Your Vehicle Contact Column"),""))
                                                                       
                                                                )))
                                                  
                                                )
                                              )
                                              
                                            ),
                                            
                                            
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
                                                width = 6,
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
                                              
                                              column(
                                                width = 6,
                                                descriptionBlock(
                                                  number = h3(textOutput("val2")), 
                                                  number_color = "black", 
                                                  number_icon = "fa fa-caret-up",
                                                  header = "", 
                                                  text = h3("TOTAL MISSING LOCATIONS"), 
                                                  right_border = FALSE,
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
                                                  label_text = 2,
                                                  label_status = "danger",
                                                  status = "warning", 
                                                  solidHeader = FALSE, 
                                                  collapsible = TRUE,
                                                  collapsed=TRUE,
                                                  fluidRow(
                                                    column(
                                                      width=12,
                                                      column(width=12,
                                                             column(width=12,
                                                                    dataTableOutput("intable", width = "100%"))
                                                      )
                                                    )
                                                  ),
                                                  hr(),
                                                  gradientBox(
                                                    title = "Missing Value Summary",
                                                    width = 12,
                                                    icon = "fa fa-matrix",
                                                    gradientColor = "maroon", 
                                                    boxToolSize = "xs", 
                                                    closable = F,
                                                    footer =
                                                  fluidRow(
                                                    column(
                                                      width =12,
                                                      column(width =12,
                                                             column(width =12,
                                                                    dataTableOutput("natab", width = "100%")))
                                                      
                                                    )
                                                  )
                                                )
                                                )
                                             ),
                                              
                                              gradientBox(
                                                title = "Order file summary",
                                                width = 12,
                                                icon = "fa fa-data",
                                                gradientColor = "white", 
                                                boxToolSize = "xs", 
                                                closable = F,
                                                
                                                fluidRow(
                                                  column(
                                                    width=12,
                                                    column(width=12,
                                                           column(width=12,
                                                                  dataTableOutput("", width = "100%"))
                                                    )
                                                  )
                                                ),
                                                hr(),
                                                column(
                                                  width = 6,
                                                  descriptionBlock(
                                                    number = h3(textOutput("")), 
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
                                                    number = h3(textOutput("")), 
                                                    number_color = "red", 
                                                    number_icon = "fa fa-caret-up",
                                                    header = "", 
                                                    text = h3("TOTAL MISSING LOCATIONS"), 
                                                    right_border = FALSE,
                                                    margin_bottom = FALSE
                                                  )
                                                ),
                                                hr(),
                                                hr(),
                                                footer = 
                                                  fluidRow(
                                                    column(
                                                      width =12,
                                                      column(width =12,
                                                             column(width =12,
                                                                    dataTableOutput("", width = "100%")))
                                                      
                                                    )
                                                  ))
                                              # box(
                                              #   solidHeader = FALSE,
                                              #   title = "",
                                              #   background = NULL,
                                              #   width = 12,
                                              #   status = "danger",
                                              #   footer = fluidRow(
                                              #     column(
                                              #       width = 6,
                                              #       descriptionBlock(
                                              #         number = h3(textOutput("val1")), 
                                              #         number_color = "red", 
                                              #         number_icon = "fa fa-caret-up",
                                              #         header = "", 
                                              #         text = h3("Total Missing Values"), 
                                              #         right_border = TRUE,
                                              #         margin_bottom = FALSE
                                              #       )
                                              #     ),
                                              #     column(
                                              #       width = 6,
                                              #       descriptionBlock(
                                              #         number = h3(textOutput("val2")), 
                                              #         number_color = "red", 
                                              #         number_icon = "fa fa-caret-up",
                                              #         header = "", 
                                              #         text = h3("TOTAL MISSING LOCATIONS"), 
                                              #         right_border = FALSE,
                                              #         margin_bottom = FALSE
                                              #       )
                                              #     )
                                              #   )
                                              # ),
                                              # hr(),
                                              # fluidRow(
                                              #   column(
                                              #     width=12,
                                              #     column(width=12,
                                              #            dataTableOutput("natab", width = "100%")
                                              #     )
                                              #   )
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
                                                       fluidRow(dataTableOutput("mat1"))
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
                                       # actionButton("goButton2", "Press here to calculate the optimum route",icon("cogs"), 
                                       #              style="color: #fff; background-color: #FFDF00; border-color: #FFDF00"),
                                       # hr(),
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