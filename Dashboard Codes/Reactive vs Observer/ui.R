fluidPage(
  fluidRow(
    column(4,
           h2("Reactive Test"),
           textInput("Test_R","Test_R"),
           textInput("Test_R2","Test_R2"),
           textInput("Test_R3","Test_R3"),
           tableOutput("React_Out")
    ),
    column(4,
           h2("Observe Test"),
           textInput("Test","Test"),
           textInput("Test2","Test2"),
           textInput("Test3","Test3"),
           tableOutput("Observe_Out")
    ),
    column(4,
           h2("Observe Event Test"),
           textInput("Test_OE","Test_OE"),
           textInput("Test_OE2","Test_OE2"),
           textInput("Test_OE3","Test_OE3"),
           tableOutput("Observe_Out_E"),
           actionButton("Go","Test")
    )
    
  ),
  fluidRow(
    column(8,
           h4("Note that observe and reactive work very much the same on the surface,
              it is when we get into the server where we see the differences, and how those
              can be exploited for diffrent uses.")
           ))
  
           )