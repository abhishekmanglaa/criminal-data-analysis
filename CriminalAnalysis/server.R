
library(shiny)


shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
   
    
    commAreasubset <- subset(commArea , X2 = input$community)
    commNo <- commAreasubset$X1
    subCrimeData <- subset(crime.data, date > input$startdate & date < input$enddate & Primary.Type = crimetype & Community.Area = commNo,
                           select=ID:crime)
    subCrimeData
    
    
  })
  
})
