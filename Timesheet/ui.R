library(shiny)
library(shinydashboard)
library(shinyTime)
library(DT)
library(shinydashboardPlus)
library(shinyalert)
library(shinycssloaders)
library(flexdashboard)

dashboardPage(dashboardHeader(title="Time Sheet Tool"),
              dashboardSidebar(div(style="overflow-y: scroll"),
                               sidebarMenu(
                                 menuItem("Data Input", icon = icon("table"), tabName = "Tab1" ),
                                 menuItem("Analytics", icon = icon("filter"), tabName = "Tab2")
                                 
                               )),
              dashboardBody(
                tabItems(
                  tabItem(tabName = "Tab1",
                          fluidPage(
                            
                            fluidRow(
                              column(width=6,
                              column(width=12,
                                                   
                                  fluidRow(
                                    dateInput("InDate","Start Date")),
                                  
                                  fluidRow(
                                    timeInput("time1", "Start Time:", value = Sys.time(), seconds = F))       
                                                   
                                            )),
                              
                              column(width=6,
                                     column(width=12,
                                            
                                            fluidRow(
                                              dateInput("OutDate","End Date")),
                                            
                                            fluidRow(
                                              timeInput("time2", "End Time:", value = Sys.time(), seconds = F)) 
                              
                              ))),
                            
                            fluidRow(
                              column(width=6,
                                     column(width = 12,
                                     fluidRow(uiOutput("selectser")),
                                     fluidRow(textInput("Ser1", "If you can't find the service type in dropdown Enter Service Type", "")),
                                     fluidRow(actionButton("test", "Save")),
                                    
                                     hr(),
                                     
                                     fluidRow(
                                       column(width = 12,
                                            # boxPlus(
                                            #   title = "Upload your human resource contact information here", 
                                            #   closable = TRUE, 
                                            #   width = NULL,
                                            #   enable_label = TRUE,
                                            #   label_text = 1,
                                            #   label_status = "danger",
                                            #   status = "warning", 
                                            #   solidHeader = FALSE, 
                                            #   collapsible = TRUE,
                                            #   
                                            #   p( fluidRow(
                                            #     column(width=6,
                                            #            # fileInput('file3', '', accept = c(".xlsx")),
                                            #            align = "center")),  
                                            #     dataTableOutput("", width = "100%")))
                                                
                                     )
                                     
                                     #fluidRow(dataTableOutput("dfff"))
                            ))),
                            
                            column(width=6,
                                   column(width=12,
                                   fluidRow(uiOutput("selectComp")),
                                   fluidRow(textInput("caption", "If you can't find the company name in dropdown Enter Company Name", ""))
                            ))
                            
                          )
                        
                  )
                ),
                
                ######################tab 2
                
                tabItem(tabName = "Tab2",
                        fluidPage(
                          
                          fluidRow(
                            column(width=4,
                                   column(width=12,
                                          fluidRow(
                                            gradientBox(
                                              title = "Total Hours:",
                                              width = 12,
                                              icon = "fa fa-bullseye",
                                              gradientColor = "teal", 
                                              boxToolSize = "xs", 
                                              closable = FALSE,
                                              h3(numericInput("obs", "", 200, min = 10, max = 1000, step = 10))
                                            )))),
                            
                            column(width=4,
                                   column(width=12,
                                          
                                          gradientBox(
                                            title = "Total Hours Spend",
                                            width = 12,
                                            icon = "fa fa-bullseye",
                                            gradientColor = "teal", 
                                            boxToolSize = "xs", 
                                            closable = FALSE,
                                            h2(textOutput("Tot"))
                                          ))),
                            
                            column(width=4,
                                   column(width=12,
                                          
                                          gradientBox(
                                            title = "Remaining Hours:",
                                            width = 12,
                                            icon = "fa fa-bullseye",
                                            gradientColor = "teal", 
                                            boxToolSize = "xs", 
                                            closable = FALSE,
                                            gaugeOutput("gauge")
                                          )))
                            
                          ),
                          
                          fluidRow(
                            column(width = 9,
                                   
                                   boxPlus(
                                     title = "History of Consulting Hours", 
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
                                              align = "center")),  
                                       dataTableOutput("dfff", width = "100%")))
                              
                              
                            )
                          )
                        
                          
                        )
                        
                )
                
                
              )
              
              
              
              
              
              )
              )