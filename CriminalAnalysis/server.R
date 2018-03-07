library(shiny)




shinyServer
(
  function(input, output) 
  {
    output$table = renderTable(iris)
  }
)
