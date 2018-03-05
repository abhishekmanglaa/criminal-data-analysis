library(shiny)

<<<<<<< HEAD
shinyUI
(
  pageWithSidebar
  (
    headerPanel("Chicago crime data visualisation"),
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
=======
shinyUI(
  
  pageWithSidebar(
    
    headerPanel("Chicago crime data visualisation"),
    
    sidebarPanel(
      
      wellPanel(
        helpText(HTML("<b>Ready?</b>")),
        HTML("Scroll down to modify the settings. Click this when you are ready to render new plots."),
        submitButton("Update graphs and tables")
        ),
      
      wellPanel(
>>>>>>> 32b29880c52eaa4757fe1eee21ff2e8690fe3566
        helpText(HTML("<b>BASIC SETTING</b>")),
        selectInput("crimetype","Select crime type.",
                    choices=c(unique(crime.data$crime))),
        helpText("Examples: BATTERY, THEFT etc."),
        dateInput("startdate",
                  "Start Date of Data Collection:",
                  value = "2000-01-01", 
                  format = "mm-dd-yyyy",
                  min = "2000-01-01", 
                  max = "2014-09-29"),
        dateInput("enddate", 
                  "End Date of Data Collection:", 
                  value = "2015-01-02", 
                  format = "mm-dd-yyyy",
                  min = "startdate", max = "2014-09-30"),
        
        helpText("MM-DD-YYYY as Date format")
      ),
      
<<<<<<< HEAD
      wellPanel
      (
        selectInput('community','Community Area', choices = commNames, selected = "Chicago-All",selectize=TRUE),
=======
      wellPanel(
        selectInput('community',
                    'Community Area',
                    choices = commNames, 
                    selected = "Chicago-All",
                    selectize=TRUE),
        
>>>>>>> 32b29880c52eaa4757fe1eee21ff2e8690fe3566
        helpText("Applies to Crime Map, Analysis, and Weather sections")
      ),
      
      wellPanel
      (
        selectInput("period", "Choose period for analysis:", choice = c("Monthly","Weekly","Daily","Yearly")),
        helpText("Applies to Analysis and Weather sections")
      )
    ), 
    
<<<<<<< HEAD
    mainPanel
    (
      tableOutput("try")
    )
    
=======
    
    
    mainPanel(
      
    )
>>>>>>> 32b29880c52eaa4757fe1eee21ff2e8690fe3566
  )
)

