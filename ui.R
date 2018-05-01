dashboardPage(
  skin = "red",
  
  dashboardHeader(
    title = "Chicago Crime Analysis & Visualization",
    titleWidth = 375,
    disable = FALSE
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction",tabName = "intro",icon = icon("info")),
      menuItem("Map",tabName = "maps",icon = icon("map-marker")),
      menuItem("Basic Stats", tabName = "analysis", icon = icon("line-chart")),
      menuItem("Public Facilities Plotting", tabName = "publicf", icon = icon("list-alt")),
      menuItem("Safe Route", tabName = "saferoute", icon = icon("road")),
      menuItem("Heat Map/ Correlation Plot", tabName = "heatmap",icon = icon("bars")),
      menuItem("Some Basic Plotting", tabName = "plots", icon = icon("area-chart")),
      menuItem("Data", tabName = "data",icon = icon("table"))
      
    )
    #,div(includeMarkdown("docs/introduction.md"), style = "padding:10px")
    
  ),
  
  dashboardBody(
    tags$head(tags$script(src = "js/ga.js")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/custom_fixs.css")),
    tabItems(
      
      tabItem(
        tabName = "intro",
        mainPanel(
          includeMarkdown("docs/introduction.md")
        )
      ),
      
      tabItem(
        tabName = "maps",
        sidebarLayout(position = "right",
                      sidebarPanel(
                        h4("Filter"),
                        selectInput("crimetype","Select crime type.",choices=c(unique(crimeData$crime))),
                        dateInput("startdate", "Start Date of Data Collection:", value = "2000-01-01", format = "yyyy-dd-mm",
                                  min = "2000-01-01", max = "2014-09-29"),
                        dateInput("enddate", "End Date of Data Collection:", value = "2018-01-01", format = "yyyy-dd-mm",
                                  min = "2000-01-01", max = "2018-01-02"),
                        
                        helpText("MM-DD-YYYY as Date format"),
                        selectInput('community','Community Area', choices = commNames, selected = "Chicago-All",selectize=TRUE),
                        helpText("Applies to Crime Map, Analysis, and Weather sections")
                      ),
                      mainPanel(
                        leafletOutput("map")
                      )
        )
      ),
      
      tabItem(
        tabName = "analysis",
        sidebarLayout(position = "right",
                      sidebarPanel(
                        h4("Filter"),
                        selectInput("crimetype1","Select crime type.",choices=c(unique(crimeData$crime))),
                        dateInput("startdate1", "Start Date of Data Collection:", value = "2000-01-01", format = "yyyy-dd-mm",
                                  min = "2000-01-01", max = "2018-01-01"),
                        dateInput("enddate1", "End Date of Data Collection:", value = "2018-01-01", format = "yyyy-dd-mm",
                                  min = "2000-01-01", max = "2018-01-01"),
                        
                        helpText("MM-DD-YYYY as Date format"),
                        selectInput('community1','Community Area', choices = commNames, selected = "Chicago-All",selectize=TRUE),
                        helpText("Applies to Crime Map, Analysis, and Weather sections"),
                        selectInput("period", "Choose period for analysis:", choice = c("Monthly","Weekly","Daily","Yearly")),
                        helpText("Applies to Analysis Sections")
                      ),
                      mainPanel(
                        showOutput("analysis","highcharts")
                      )
        )
      ),
      
      tabItem(
        tabName = "publicf",
        sidebarLayout(position = "right",
                      sidebarPanel(
                        helpText("Scatter Plot")
                        # selectInput("crimetype2","Select crime type.",choices=c(unique(crimeData$crime)),selected = "BATTERY"),
                        #selectInput("facility","Select Facility",choices=c("RESIDENTIAL","FOOD","BAR"),selected = "BAR")
                        
                        #submitButton("Update Plot")
                        
                        
                      ),
                      mainPanel(
                        highchartOutput("publicf",height = "700px")
                      )
        )
      ),
      
      tabItem(
        tabName = "saferoute",
        sidebarLayout(position = "right",
                      sidebarPanel(
                        h4("Filter"),
                        selectInput('source','Enter Source', choices = commNames, selected = "Uptown",selectize=TRUE),
                        selectInput('destination','Enter Destination', choices = commNames, selected = "Rogers Park",selectize=TRUE)
                        
                      ),
                      mainPanel(
                        leafletOutput("saferoute")
                      )
        )
      ),
      
      tabItem(
        tabName = "heatmap",
        sidebarLayout(position = "right",
                      sidebarPanel(
                        h4("Filter"),
                        radioButtons("heatplotselect",
                                     label="Select the heat plot",
                                     choices=list("By time","By Day of Week","By Month"),
                                     selected="By time"),
                        helpText("Applies to the heat Tab")
                        
                      ),
                      mainPanel(
                        plotOutput("heatmaps")
                      )
        )
      ),
      tabItem(
        tabName = "plots",
        sidebarLayout(position = "right",
                      sidebarPanel(
                        h4("Filter"),
                        selectInput("typeofplot", "Choose type of plot", choice = c("Number of crimes vs CrimeType","Crime by time of Day","Crime By month","Crime by day")),
                        helpText("Applies to the Plots Tab")
                        
                      ),
                      mainPanel(
                        plotOutput("plots")
                      )
        )
      ),
      tabItem(
        tabName = "data",
        sidebarLayout(position = "right",
                      sidebarPanel(helpText("Data Reference")),
                      mainPanel(
                        dataTableOutput("datatable")
                      )
        )
      )
      
      
      
      
    )
  )
  
  
  
)