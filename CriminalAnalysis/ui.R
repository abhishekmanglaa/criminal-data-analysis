library(shiny)

shinyUI
(
  fluidPage
  (
    headerPanel("Chicago crime data visualisation"),
    sidebarLayout
    (
      sidebarPanel
      (
        wellPanel
        (
          helpText(HTML("<b>BASIC SETTING</b>")),
          selectInput("crimetype","Select crime type.",choices=c(unique(crime.data$crime))),
          helpText("Examples: BATTERY, THEFT etc."),
          dateInput("startdate", "Start Date of Data Collection:", value = "2000-01-01", format = "mm-dd-yyyy",min = "2000-01-01", max = "2014-09-29"),
          #dateInput("enddate", "End Date of Data Collection:", value = "2015-01-02", format = "mm-dd-yyyy",min = "startdate", max = "2014-09-30"),
          helpText("MM-DD-YYYY as Date format")
        )
      ),
      mainPanel
      (
        tabsetPanel
        (
          type="tab",
          tabPanel("Print",tableOutput("table")),
          tabPanel("Summary",verbatimTextOutput("summr"))
        )
      )
    )
  )
)

