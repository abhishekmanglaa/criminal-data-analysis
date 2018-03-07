library(shiny)




shinyServer
(
  function(input, output) 
  {
    output$table <- renderTable(commArea)
    output$try <- renderTable(iris)
  }
)
