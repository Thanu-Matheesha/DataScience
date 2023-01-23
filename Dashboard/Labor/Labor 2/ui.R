library(shiny)
library(shinydashboard)
library(dygraphs)
# library(plotly)

dashboardPage(dashboardHeader(title="Labor Optimization"),
              dashboardSidebar(div(style="overflow-y: scroll"),
                               sidebarMenu(
                                 menuItem("Labor Optimization", icon = icon("bar-chart-o"), tabName = "Optim1"),
                                          numericInput("lab", "Number of Maximum Labors required for each shift", 1),
                                          numericInput("shi", "Number of Maximum shifts for each Labor", 1)
                                   
                                              
                                 
                               )),
              dashboardBody(
                tabItems(
                  tabItem(tabName = "Optim1",
                          column(width=6,
                                 fluidRow(
                                   fileInput('file1', 'Insert File', accept = c(".xlsx")),
                                   # textInput('file1sheet','Name of Sheet (Case-Sensitive)'),
                                   tableOutput("contents")
                                 ),
                                 fluidRow(actionButton("goButton", "Optimize!")),
                                 fluidRow(actionButton("go", "Reset!"))
                                 # fluidRow(actionButton("reset", "Reset"))
                                 ),
                          column(width=6,
                                 fluidRow(tableOutput("model"), width=12),
                                 column(width=3,
                                 fluidRow(h4("Objective Value")),
                                 fluidRow(h5(textOutput("val1"))),
                                 fluidRow(h4("Solver Status")),
                                 fluidRow(h5(textOutput("val2")))))
                          )
                  )
                )
              )
              
              
