function(input,output,session){
  
  # Create a reactive Evironment. Note that we can call the varaible outside same place
  # where it was created by calling Reactive_Var(). When the varaible is called by
  # renderTable is when it is evaluated. No real diffrence on the surface, all in the server.
  
  Reactive_Var<-reactive({c(input$Test_R, input$Test_R2, input$Test_R3)})
  
  output$React_Out<-renderTable({
    Reactive_Var()
  })
  
  # Create an observe Evironment. Note that we cannot access the created "df" outside 
  # of the env. A, B,and C will update with any input into any of the three Text Feilds.
  observe({
    A<-input$Test
    B<-input$Test2
    C<-input$Test3
    df<-c(A,B,C)
    output$Observe_Out<-renderTable({df})
  })
  
  #We can change any input as much as we want, but the code wont run until the trigger
  # input$Go is pressed.
  observeEvent(input$Go, {
    A<-input$Test_OE
    B<-input$Test_OE2
    C<-input$Test_OE3
    df<-c(A,B,C)
    output$Observe_Out_E<-renderTable({df})
  })
  
}