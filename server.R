library(shiny)

xtsMelt <- function(data) {
  require(reshape2)
  #translate xts to time series to json with date and data
  #for this behavior will be more generic than the original
  #data will not be transformed, so template.rmd will be changed to reflect
  #convert to data frame
  data.df <- data.frame(cbind(format(index(data),"%Y-%m-%d"),coredata(data)))
  colnames(data.df)[1] = "date"
  data.melt <- melt(data.df,id.vars=1,stringsAsFactors=FALSE)
  colnames(data.melt) <- c("date","indexname","value")
  #remove periods from indexnames to prevent javascript confusion
  #these . usually come from spaces in the colnames when melted
  data.melt[,"indexname"] <- apply(matrix(data.melt[,"indexname"]),2,gsub,pattern="[.]",replacement="")
  return(data.melt)
  #return(df2json(na.omit(data.melt)))
}



shinyServer
(
  
  function(input, output) 
  {
    datetypesubsetforsimpledata <- reactive({
      
      tempData <- subset(crime.data, 
                         crime.data$Date > as.POSIXct(strptime(input$startdate, format="%Y-%m-%d")) & crime.data$Date < as.POSIXct(strptime(input$enddate, format="%Y-%m-%d")) & crime.data$crime == input$crimetype)
      
      commSubset <- subset(commArea,commArea$X2 == input$community)
      tempData <- subset(tempData, tempData$Community.Area == commSubset$X1)
      tempData <- tempData[c(4,6:8,19,24,26,27)]
      return(tempData)
    })
    
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
      
      if (input$period == "Daily") {crimebytime <- apply.daily(df.xts, function(d) {sum(str_count(d, input$crimetype ))})}
      if (input$period == "Weekly") {crimebytime <- apply.weekly(df.xts, function(d) {sum(str_count(d, input$crimetype ))})}
      if (input$period == "Monthly") {crimebytime <- apply.monthly(df.xts, function(d) {sum(str_count(d, input$crimetype ))})}
      if (input$period == "Yearly") {crimebytime <- apply.yearly(df.xts, function(d) {sum(str_count(d, input$crimetype ))})}
      
      df.xts <- NULL
      return(crimebytime)
    })
    
    
    
    output$data = DT::renderDataTable({datetypesubsetforsimpledata()})
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
    output$plots <- renderPlot({
      p <- qplot(crime.data$crime,xlab = "Crime",main = "Crimes in Chicago")+scale_y_continuous("Number of Crimes")
      p <- qplot(crime.data$crime,xlab = "Crime",main = "Crimes in Chicago")+scale_y_continuous("Number of Crimes")
      q <- qplot(crime.data$time.tag, xlab="Time of day", main="Crimes by time of day") + scale_y_continuous("Number of crimes")
      r <- qplot(crime.data$day,xlab = "Day of Week",main="Crimes by day of Week")+scale_y_continuous("Number of crimes")
      s <- qplot(crime.data$month, xlab= "Month", main="Crimes by month")+ scale_y_continuous("Number of crimes")
      type <- input$typeofplot
      if(type == "Number of crimes vs CrimeType") { print(p)}
      if(type == "Crime by time of Day") { print(q)}
      if(type == "Crime By month") { print(s)}
      if(type == "Crime by day") { print(r)}
      
    })
    
    
    
    
    
    
    
    output$analysis <- renderChart2({
      
      crimebytime <-crimebytimeXTS()
      
      #Convert data using xtsMelt for highcharts plot
      ust.melt <- na.omit(xtsMelt(crimebytime))
      ust.melt$date2 <- as.Date(ust.melt$date, format = "%Y-%m-%d")
      ust.melt$Crime <- as.numeric(as.character(ust.melt$value))
      ust.melt$date4  <- as.numeric(as.POSIXct(ust.melt$date2, origin="1970-01-01")) * 1000
      
      #Highchart plot
      h1 <- hPlot(
        Crime ~ date4,  #or x="date", y="value"
        data = ust.melt, 
        color = '#4572A7',
        type = 'spline',
        title = paste("Crimes for ",input$crimetype)
      ) 
      h1$xAxis(type = "datetime")
      
      h1
    })
    
  }
)
