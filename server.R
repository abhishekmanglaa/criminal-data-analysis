library(shiny)



shinyServer
(
  
  function(input, output) 
  {
    output$data = DT::renderDataTable(crime.simple.data)
    
    
    
    
    datetypesubset <- reactive({
      
      tempData <- subset(crime.data, 
                         crime.data$Date > as.POSIXct(strptime(input$startdate, format="%Y-%m-%d")) & crime.data$Date < as.POSIXct(strptime(input$enddate, format="%Y-%m-%d")) & crime.data$crime == input$crimetype)
      
      commSubset <- subset(commArea,commArea$X2 == input$community)
      tempData <- subset(tempData, tempData$Community.Area == commSubset$X1,
                         select = c(Date,Primary.Type,Latitude,Longitude))
      return(tempData)
    })
    
    
    
    crimebytimeXTS <- reactive({
      dfin <- datetypesubset()
      df.xts <- xts(x = dfin[, c("Primary.Type","Date")], order.by = dfin[, "Date"])
      
      crimebytime <- apply.yearly(df.xts, function(d) {sum(str_count(d, input$crimetype ))})
      df.xts <- NULL
      return(crimebytime)
    })
    
    
  
    
    output$map <- renderPlot({
      crimebydatetype <- datetypesubset()
      
      
      map.center = head(crimebydatetype,n=2)
      map.center = map.center[c("Longitude","Latitude")]
      names(map.center[1])= 'lon'
      names(map.center[2])= 'lat'
      map.center = map.center[-1,]
      
     
      map.base = get_googlemap(
        as.matrix(map.center),
        maptype = "terrain",
        zoom = 15,
        messaging = FALSE
      )
      
      map.base <- ggmap(map.base, extend = "panel", messaging = FALSE) + coord_cartesian() + coord_fixed(ratio = 1.5)
      
      p <- map.base + geom_point(aes(x=Longitude, y=Latitude), colour="red", size = 4, na.rm=TRUE, data=crimebydatetype)
      
      plot(p)
      
      
      
    })
  }
)
