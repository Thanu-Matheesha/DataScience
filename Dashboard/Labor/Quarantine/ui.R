library(shiny)
library(shinydashboard)
library(dygraphs)
library(DT)
# library(plotly)

dashboardPage(dashboardHeader(title="Optimization"),
              dashboardSidebar(div(style="overflow-y: scroll"),
                               sidebarMenu(
                                 menuItem("Labor Optimization", icon = icon("bar-chart-o"), tabName = "Optimization" ),
                                 numericInput("lab", "Number of Maximum Labors required for each shift", 1),
                                 numericInput("shi", "Number of Maximum shifts for each Labor", 1),
                                 actionButton("goButton", "Optimize!"),
                                 h3("Objective Value"),
                                 h4(textOutput("val1")),
                                 h3("Solver Status"),
                                 h4(textOutput("val2"))
                               )),
              dashboardBody(
                tabItems(
                  tabItem(tabName = "Optimization",
                          column(width=6,
                                 fluidRow(
                                   fileInput('file1', 'Insert File', accept = c(".xlsx")),
                                   # textInput('file1sheet','Name of Sheet (Case-Sensitive)'),
                                   h3(textOutput("text1")),
                                   dataTableOutput("value"))
                                 ),
                                 fluidRow(
                                   column(width=9,
                                          
                                          fluidRow(uiOutput("selectname"))
                                          
                                          
                                   ),
                                   fluidRow(width=9,
                                          
                                          column(width=9,
                                                 dataTableOutput("dd"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;")
                                          
                                   )
                                 )
                          
                  )
                )
              )
              
              
)