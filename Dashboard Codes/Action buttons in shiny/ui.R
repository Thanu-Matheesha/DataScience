library(shinycssloaders)

fluidPage(
  DT::dataTableOutput("data"),
  textOutput('myText')
)