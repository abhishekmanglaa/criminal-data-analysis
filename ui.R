library(shiny)

shinyUI
(
  fluidPage
  ( theme = shinytheme("journal"),
    headerPanel("Chicago crime data visualisation"),
    sidebarLayout
    (
      sidebarPanel
      (
        wellPanel
        (
          helpText(HTML("<b>Ready?</b>")),
          HTML("Scroll down to modify the settings. Click this when you are ready to render new plots."),
          submitButton("Update graphs and tables")
        ),
        
        wellPanel
        (
          helpText(HTML("<b>BASIC SETTING</b>")),
          selectInput("crimetype","Select crime type.",choices=c(unique(crimeData$crime))),
          helpText("Examples: BATTERY, THEFT etc."),
          dateInput("startdate", "Start Date of Data Collection:", value = "2000-01-01", format = "yyyy-dd-mm",
                    min = "2000-01-01", max = "2014-09-29"),
          dateInput("enddate", "End Date of Data Collection:", value = "2018-01-01", format = "yyyy-dd-mm",
                    min = "2000-01-01", max = "2018-01-02"),
          
          helpText("MM-DD-YYYY as Date format")
        ),
        
        wellPanel
        (
          selectInput('community','Community Area', choices = commNames, selected = "Chicago-All",selectize=TRUE),
          helpText("Applies to Crime Map, Analysis, and Weather sections")
        ),
        
        wellPanel
        (
          selectInput("period", "Choose period for analysis:", choice = c("Monthly","Weekly","Daily","Yearly")),
          helpText("Applies to Analysis and Weather sections")
        ),
        
        radioButtons("color","Select the color", choices = c("Blue","Orange","Green","Red","Purple")),
        
        wellPanel
        (
          selectInput("typeofplot", "Choose type of plot", choice = c("Number of crimes vs CrimeType","Crime by time of Day","Crime By month","Crime by day")),
          helpText("Applies to the Plots Tab")
        )
      ),
      
      mainPanel
      (
        tabsetPanel(
          type = "tab",
          tabPanel("Introduction",includeMarkdown("docs/introduction.md")),
          tabPanel("Data",dataTableOutput("datatable")),
          tabPanel("Map",plotOutput("map",height = 600,width = 600)),
          #tabPanel("Temp",tableOutput('temp')),
          tabPanel("Basic Stats",showOutput("analysis","highcharts")),
          tabPanel("Plots", plotOutput("plots")),
          tabPanel("HeatMaps", plotOutput("heatMaps"),radioButtons("heatplotselect",
                                                                   label="Select the heat plot",
                                                                   choices=list("By time","By Day of Week","By Month"),
                                                                   selected="By time"))
          
    
        )
      )
    ) 
    

  )
)

