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
library(ABCanalysis)
# library(plotly)

dashboardPage(dashboardHeader(title="ABC Analysis", disable = F),
              dashboardSidebar(div(style="overflow-y: hidden"),
                               sidebarMenu(
                                 menuItem("Data Input", icon = icon("table"), tabName = "Optim1"),
                                 menuItem("Graph", icon = icon("chart-area"), tabName = "Optim2"),
                                 menuItem("Output File", icon = icon("file"), tabName = "Optim3")
                                 # menuItem("Route Details", icon = icon("map-marker-alt"), tabName = "Optim4")
                                
                               )),
              dashboardBody(tags$head(tags$style(
                HTML('.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}')
                # type="text/css", "label.control-label, .selectize-control.single{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;},"
                
              )),
              useShinyjs(),
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
                                 
                                 fluidRow(box(uiOutput("selectlevel"),width = 12)),
            
                                 fluidRow(
                                   column(width=12,
                                          flipBox(
                                            id = 1,
                                            main_img = "https://image.flaticon.com/icons/svg/2920/2920349.svg",
                                            header_img = "https://www.flaticon.com/svg/static/icons/svg/1178/1178027.svg",
                                            front_title = "Please upload file in .csv format",
                                            back_title = "Data",
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
                                                   fluidRow(selectInput("y_input3", label = ("Select Your Volume Column"),""))
                                                   # fluidRow(selectInput("y_input4", label = ("Select Your Link Column"),""))
                                            )),
                                            
                                            column(width=6,
                                                   column(width=6,
                                                   fluidRow(actionButton("goButton2", "Map Your Column",icon("cogs"),
                                                                         style="color: #fff; background-color: #1A3FA4; border-color: #1A3FA4"),align = "left"))),
                                                   hr(),
                                            
                                            fluidRow(
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
                                                      numberColor = "red", 
                                                      numberIcon = "fa fa-caret-up",
                                                      header = "", 
                                                      text = h3("Missing Values in Value"), 
                                                      rightBorder = TRUE,
                                                      marginBottom = FALSE
                                                    )
                                                  ),
                                                  column(
                                                    width = 6,
                                                    descriptionBlock(
                                                      number = h3(textOutput("val2")), 
                                                      numberColor = "red", 
                                                      numberIcon = "fa fa-caret-up",
                                                      header = "", 
                                                      text = h3("Missing Values in Volume"), 
                                                      rightBorder = FALSE,
                                                      marginBottom = FALSE
                                                    )
                                                  )
                                                )
                                              ),
                                              hr(),
                                              fluidRow(
                                                column(
                                                  width=12,
                                                  column(width=12,
                                                         column(width=12,
                                                         dataTableOutput("natab", width = "100%"))
                                                  )
                                                )
                                              )
                                              
                                            ),
                                            
                                            back_content = tagList(
                                              column(
                                                width =12,
                                                column(width =12,
                                                       column(width =12,
                                                dataTableOutput("intable", width = "100%")))
                                                
                                              )
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
                                       title = "ABC Graph",
                                       width = 12,
                                       icon = "fa fa-th",
                                       gradientColor = "purple", 
                                       boxToolSize = "sm", 
                                       closable = F,
                                       footer = column(width=12,align="center",
                                                       fluidRow(actionButton("goButton1", "Press here to analyze",icon("cogs"), 
                                                                             style="color: #fff; background-color: #1A3FA4; border-color: #1A3FA4")),
                                                       # fluidRow(dataTableOutput("Ftable"))
                                                       fluidRow(plotOutput("ABCpl")),
                                                       hr(),
                                                       downloadButton('down',"Download the graph")
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
                                    
                                     gradientBox(
                                       title = "Output Table",
                                       width = 12,
                                       icon = "fa fa-th",
                                       gradientColor = "purple", 
                                       boxToolSize = "sm", 
                                       closable = F,
                                       footer = dataTableOutput("Ftable", width = "100%")
                                     ),
                                     hr(),
                                     downloadButton('download',"Download the file")
                                     
                                   )
                            )
                          ))
                  #TAB4########################################################
                  # tabItem(tabName = "Optim4",
                  #         fluidPage(
                  #           column(width=12,
                  #                  fluidRow(
                  #                    box(
                  #                      title = "Map", background = "purple", solidHeader = TRUE, width = 12,
                  #                      leafletOutput("map2")
                  #                    )
                  #                    
                  #                    
                  #                  ),
                  #                  
                  #                  hr(),
                  #                  fluidRow(
                  #                    column(width=3,
                  #                           gradientBox(
                  #                             title = "Total Distance",
                  #                             width = 12,
                  #                             icon = "fa fa-road",
                  #                             gradientColor = "teal", 
                  #                             boxToolSize = "xs", 
                  #                             closable = FALSE,
                  #                             h2(textOutput("val11"))
                  #                           )),
                  #                    column(width=3,
                  #                           gradientBox(
                  #                             title = "Total Time Spend",
                  #                             width = 12,
                  #                             icon = "fa fa-clock",
                  #                             gradientColor = "teal", 
                  #                             boxToolSize = "xs", 
                  #                             closable = FALSE,
                  #                             h2(textOutput("val12"))
                  #                           )),
                  #                    column(width=3,
                  #                           gradientBox(
                  #                             title = "Number of locations",
                  #                             width = 12,
                  #                             icon = "fa fa-city",
                  #                             gradientColor = "teal", 
                  #                             boxToolSize = "xs", 
                  #                             closable = FALSE,
                  #                             h2(textOutput("val13"))
                  #                           )),
                  #                    column(width=3,
                  #                           gradientBox(
                  #                             title = "Quantity in CBM",
                  #                             width = 12,
                  #                             icon = "fa fa-box",
                  #                             gradientColor = "teal", 
                  #                             boxToolSize = "xs", 
                  #                             closable = FALSE,
                  #                             h2(textOutput("val14"))
                  #                           ))
                  #                    
                  #                    
                  #                  )
                  # 
                  #                  
                  #           )
                  #         )
                  # )
                  
                )
              )
)