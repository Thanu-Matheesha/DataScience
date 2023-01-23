library(shiny)
library(shinydashboard)
library(dygraphs)
library(plotly)

dashboardPage(
  dashboardHeader(title="IDeP - DBL"),
  dashboardSidebar(div(style="overflow-y: scroll"),
                   sidebarMenu(
                     menuItem("Planning", icon = icon("bar-chart-o"), tabName = "consumption" ),
                     menuItem("Inventory", icon = icon("cubes"), tabName = "inventory" )
                   )),
  dashboardBody(tags$head(
    includeCSS("new.css"),
    includeCSS("styles.css")
  ),
    
  tabItems(
    tabItem(tabName = "consumption",
            fluidPage(
              
              fluidRow(
                column(width=9,
                       fluidRow(box(uiOutput("selectlevel"),width = 12)),
                       fluidRow(
                         box(title = "Demand Forecast",width = 12,solidHeader = T, dygraphOutput('dygraph'))), #dygraphOutput('dygraph')
                       br(),
                       fluidRow(
                         column(width=4,infoBoxOutput("mae",width = "100%")),
                         column(width=4,infoBoxOutput("mape",width = "100%")),
                         column(width=4,infoBoxOutput("mase",width = "100%"))
                       )
                ),
                column(width =3,
                       
                       box(width = 12,conditionalPanel(condition="input.selectlevel=='dealer'",
                                      uiOutput("selectcat_1"),
                                      uiOutput("selectcat_2"),
                                      uiOutput("selectitem"),
                                      uiOutput("selectarea"),
                                      uiOutput("selectdealer")),
                          conditionalPanel(condition="input.selectlevel=='area'",
                                      uiOutput("selectcat_1_area"),
                                      uiOutput("selectcat_2_area"),
                                      uiOutput("selectitem_area"),
                                      uiOutput("selectarea_area")),
                          conditionalPanel(condition="input.selectlevel=='item'",
                                      uiOutput("selectcat_1_item"),
                                      uiOutput("selectcat_2_item"),
                                      uiOutput("selectitem_item")),
                          conditionalPanel(condition="input.selectlevel=='cat2'",
                                     uiOutput("selectcat_1_cat2"),
                                     uiOutput("selectcat_2_cat2")),
                          conditionalPanel(condition="input.selectlevel=='cat1'",
                                     uiOutput("selectcat_1_cat1")),
                                     uiOutput("addoverride")),
                       
                       box(width = 12,conditionalPanel(condition="input.addoverride=='yes'",
                                                       uiOutput("override1"),
                                                       uiOutput("override2"),
                                                       uiOutput("override3"),
                                                       uiOutput("override"),br()),
                      uiOutput("replenishment"))
                )
              )
              
            )),
    tabItem(tabName = "inventory",
      fluidRow(
        box(width = 12,
            column(width=6,
            uiOutput("selectcat_inventory_1")),
            column(width=6,
            uiOutput("selectcat_inventory_2"))
        )),
      fluidRow(
        box(width = 12,
             plotlyOutput("inventory")    
            
          )
      ),
      fluidRow(
        box(width = 12,
          DT::dataTableOutput('inventory_table')
        )
      )
    )
            
  )               
))