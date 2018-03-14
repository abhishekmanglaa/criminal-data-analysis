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
                         select = c(Latitude,Longitude))
      return(tempData)
    })
    
    output$map <- renderPlot({
      crimebydatetype <- datetypesubset()
      
      
     # map.center = head(crime.data,n=2)
     # map.center = map.center[c("Longitude","Latitude")]
     # names(map.center[1])= 'lon'
     # names(map.center[2])= 'lat'
     # map.center = map.center[-1,]
      
      map.center = geocode("Chicago")
      map.base = get_googlemap(
        as.matrix(map.center),
        maptype = "terrain",
        zoom = 10,
        messaging = FALSE
      )
      
      map.base <- ggmap(map.base, extend = "panel", messaging = FALSE) + coord_cartesian() + coord_fixed(ratio = 1.5)
      
      p <- map.base + geom_point(aes(x=Longitude, y=Latitude), colour="red", size = 4, na.rm=TRUE, data=crimebydatetype)
      
      plot(p)
      
      
      
    })
   
    
    
  # output$temp <- DT::renderDataTable({crimebydatetype()})
    
    
    
  }
)
