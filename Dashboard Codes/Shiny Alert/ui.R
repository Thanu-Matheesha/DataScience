library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyalert)


dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    useShinyalert(),
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                column(3,
                       
                       fileInput("file1", "Choose CSV File",
                                 multiple = FALSE,
                                 accept = c("text/csv",
                                            "text/comma-separated-values,
                                            text/plain",
                                            ".csv"))
                )
                
                       ),
              fluidRow(
                tableOutput("contents")
              )
              )
    )
    )
  )