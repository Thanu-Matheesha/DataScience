library(shiny)
library(shinydashboard)
library(dygraphs)
# library(plotly)

dashboardPage(dashboardHeader(title="WMS"),
              dashboardSidebar(div(style="overflow-y: scroll"),
                               sidebarMenu(
                                 menuItem("Waste Reduction", icon = icon("bar-chart-o"), tabName = "forecast" )
                               )),
              dashboardBody(
                tabItems(
                  tabItem(tabName = "forecast",
                          column(width=6,
                                 fluidRow(
                                   fileInput('file1', 'Insert File', accept = c(".xlsx")),
                                   # textInput('file1sheet','Name of Sheet (Case-Sensitive)'),
                                   tableOutput("value")
                                 ),
                                 fluidRow(
                                   column(width=9,
                                          
                                          fluidRow(uiOutput("selectname"))

                                         ),
                                   column(width=9,
                                          
                                          fluidRow(uiOutput("dd"))
                                          
                                   )
                                         )
                                 )
                        )
                       )
                         )
              
              
)