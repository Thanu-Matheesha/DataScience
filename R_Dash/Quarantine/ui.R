library(shiny)
library(shinydashboard)
# library(dygraphs)
library(DT)
library(shinydashboardPlus)
library(shinyalert)
library(shinycssloaders)
# library(plotly)

dashboardPage(dashboardHeader(title="HR Allocation Tool"),
              dashboardSidebar(div(style="overflow-y: scroll"),
                               sidebarMenu(
                                 menuItem("Data Input", icon = icon("table"), tabName = "Tab1" ),
                                 menuItem("Optimization", icon = icon("filter"), tabName = "Tab2",
                                          numericInput("lab", "Number of Maximum Employees required for each shift", 1),
                                          numericInput("shi", "Number of Maximum shifts for each Employee", 1),
                                          actionButton("goButton", "Optimize!"),
                                          h3(""),
                                          h4(textOutput("")),
                                          h3(""),
                                          h4(textOutput(""))),
                                 menuItem("Results", icon = icon("file"), tabName = "Tab3" )
                                
                               )),
              dashboardBody(
                tabItems(
                  tabItem(tabName = "Tab1",
                          fluidPage(
                        
                          # column(width=6,
                          #        fluidRow(
                          #          fileInput('file1', 'Insert File', accept = c(".xlsx")),
                          #          # textInput('file1sheet','Name of Sheet (Case-Sensitive)'),
                          #          h3(textOutput("text1")),
                          #          dataTableOutput("value"))
                          #        ),
                          
                          column(width=12,
                                 
                                 fluidRow(
                                   column(width=12,
                                          flipBox(
                                            id = 1,
                                            main_img = "https://image.flaticon.com/icons/svg/2705/2705152.svg",
                                            header_img = "https://image.flaticon.com/icons/svg/3232/3232966.svg",
                                            front_title = "Please upload Human Resource details in .xlsx format",
                                            back_title = "Data",
                                            column(
                                              width=12,
                                              column(width=12,
                                                     column(width=12,
                                            boxPlus(
                                              title = "Upload your human resource allocation plan here", 
                                              closable = TRUE, 
                                              width = NULL,
                                              enable_label = TRUE,
                                              label_text = 1,
                                              label_status = "danger",
                                              status = "warning", 
                                              solidHeader = FALSE, 
                                              collapsible = TRUE,
                                              
                                              p(
                                            fluidRow(
                                              column(width=6,
                                              # column(width=6,
                                              fileInput('file1', '', accept = c(".xlsx")),
                                              align = "left")),
                                            column(
                                              width=12,
                                              column(width=12,
                                                     column(width=12,
                                                            dataTableOutput("value", width = "100%"))
                                              )
                                            )))))),
                                            
                                            fluidRow(
                                              column(width=12,
                                              column(width=12,
                                              column(width=9,
                                                     
                                                     fluidRow(uiOutput("selectname"))

                                              )))),
                                            
                                           
                                            
                                            column(
                                              width=12,
                                              column(width=12,
                                                     column(width=12,
                                                            boxPlus(
                                                              title = "Upload your human resource contact information here", 
                                                              closable = TRUE, 
                                                              width = NULL,
                                                              enable_label = TRUE,
                                                              label_text = 1,
                                                              label_status = "danger",
                                                              status = "warning", 
                                                              solidHeader = FALSE, 
                                                              collapsible = TRUE,
                                                              
                                                            p( fluidRow(
                                                              column(width=6,
                                                              fileInput('file3', '', accept = c(".xlsx")),
                                                              align = "center")),  
                                                              dataTableOutput("intable", width = "100%")))
                                                     )
                                              )
                                            ),
                                            
                                            
                                            
                                            back_content = tagList(
                                              column(
                                                width =12,
                                                column(width =12,
                                                       column(width =12,
                                                              dataTableOutput("", width = "100%")))
                                                
                                              )
                                            )
                                          )
                                   )
                                 )
                          ))),
                  #TAB2###########################################################################
                  tabItem(tabName = "Tab3",
                  
                  
                  
                  
                  
                                 fluidRow(
                                   column(width=9,
                                          
                                          fluidRow(uiOutput(""))
                                          
                                          
                                   ),
                                   fluidRow(width=9,
                                          
                                          column(width=12,
                                                 column(width=12,
                                                        boxPlus(
                                                          title = "Download your optimized human resource allocation plan from here", 
                                                          closable = TRUE, 
                                                          width = NULL,
                                                          enable_label = TRUE,
                                                          label_text = 1,
                                                          label_status = "danger",
                                                          status = "warning", 
                                                          solidHeader = FALSE, 
                                                          collapsible = TRUE,
                                                          
                                                          p(
                                                 
                                                 dataTableOutput("dd"),
                                                 hr(),
                                                 downloadButton('download',"Download the file")
                                                 # style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                                                 ))))
                                          
                                   )
                                 ),
                          hr(),
                          fluidRow(
                            column(width=6,
                                   gradientBox(
                                     title = "Objective Value",
                                     width = 12,
                                     icon = "fa fa-bullseye",
                                     gradientColor = "teal", 
                                     boxToolSize = "xs", 
                                     closable = FALSE,
                                     h2(textOutput("val1"))
                                   )),
                            column(width=6,
                                   gradientBox(
                                     title = "Solver Status",
                                     width = 12,
                                     icon = "fa fa-thermometer-three-quarters",
                                     gradientColor = "teal", 
                                     boxToolSize = "xs", 
                                     closable = FALSE,
                                     h2(textOutput("val2"))
                                   )))
                          
                  )
                )
              )
              
              
              
)