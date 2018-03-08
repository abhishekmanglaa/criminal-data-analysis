library(shiny)

shinyServer
(
  function(input, output) 
  {
    output$data = DT::renderDataTable(
      crime.simple.data
    )
  }
)
