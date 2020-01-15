shinyServer <- function(input,output,session){
  filtered <- reactive({
    health_data %>%
      filter(Year >= input$yearInput[1],
             Year <= input$yearInput[2],
             Age == input$ageInput,
             Sex == input$sexInput,
             Source %in% input$sourceInput,
             Region == input$regionInput
      )
  })
  
  output$plotNumber <- renderPlot({
    ggplot(filtered(),aes(y=Number,x=Year,group=Source))+
      geom_line(aes(color=Source))+
      theme_minimal()+
      xlab("Year") +ylab("Number")
    
  })
  output$plotRate <- renderPlot({
    ggplot(filtered(),aes(y=Rate,x=Year,group=Source))+
      geom_line(aes(color=Source))+
      theme_minimal()+
      xlab("Year") +ylab("Rate (%)")
  })
  
  output$Health_model <- renderDataTable({
    filtered()
  }, options=list(aLengthMenu = c(12,20,40),iDisplayLength=12))
}
