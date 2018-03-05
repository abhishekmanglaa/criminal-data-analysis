
library(shiny)


shinyServer
(
  function(input, output) 
  {
    output$try <- renderTable
    (
      {
        commAreaSubset=subset(commArea,input$community)
        commNo=commAreaSubset$X1
        subCrimeData=subset(crime.data,crime.data$`Primary Type`==input$crimetype&&crime.data$`Community Area`==commNo)
        summary(subCrimeData)
      }
    )
  }
)
