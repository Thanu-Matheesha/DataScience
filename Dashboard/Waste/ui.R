library(shiny)
library(shinydashboard)
library(dygraphs)
library(plotly)

dashboardPage(dashboardHeader(title="WMS"),
              dashboardSidebar(div(style="overflow-y: scroll"),
                               sidebarMenu(
                                 menuItem("Waste Reduction", icon = icon("bar-chart-o"), tabName = "forecast" )
                               )),
              dashboardBody(
                tabItems(
                  tabItem(tabName = "forecast",
                         column(width=6,
                        fluidRow(box(uiOutput("selectlevel"),width = 12)),
                        fluidRow(
                          column(width=9,
                                 
                                 fluidRow(uiOutput("selectcat_1")),
                                 fluidRow(uiOutput("selectcat_2")),
                                 fluidRow(uiOutput("selectitem"))
                                )
                              )
                         
                         
                              )
                        )
                        )
                          )
                
                              
)