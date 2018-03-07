library(shiny)




shinyServer
(
  function(input, output) 
  {
<<<<<<< HEAD
    #output$try <- renderTable
    #(
     # {
      #  commAreaSubset=subset(commArea,input$community)
       # commNo=commAreaSubset$X1
        #subCrimeData=subset(crime.data,crime.data$`Primary Type`==input$crimetype&&crime.data$`Community Area`==commNo)
        #summary(subCrimeData)
      #}
    #)
    output$table=renderTable({
      head(commNames)
    })
    output$summr=renderPrint({
      summary(commNames)
    })
=======
    output$table <- renderTable(commArea)
    output$try <- renderTable(iris)
>>>>>>> b9d3194aa1334399c4a58527b8693a2568cd9d41
  }
)
