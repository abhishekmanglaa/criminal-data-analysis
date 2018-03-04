#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    commAreasubset <- subset(commArea , X2 = input$community)
    commNo <- commAreasubset$X1
    subCrimeData <- subset(crime.data, date > input$startdate & date < input$enddate & Primary.Type = crimetype & Community.Area = commNo,
                           select=ID:crime)
    subCrimeData
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
})
