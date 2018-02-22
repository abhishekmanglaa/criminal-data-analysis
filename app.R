library(shiny)
h1("My Title")

ui <- fluidPage(
  
  titlePanel("title panel",
             h1("Mytitle")
  ),
  
  sidebarLayout(
    position = "left",
    sidebarPanel("sidebar panel"),
    mainPanel(
      h2("Header 2")
      ),
    
  ),
  
  navbarPage("Navbar page",
             position = "fixed-bottom"
  )
  
)

server <- function(input, output) {
  
  
}

shinyApp(ui = ui, server = server)

