library(shiny)


map <- function() {
  m <- leaflet() %>%
    addTiles(group = "OSM") %>%
    addProviderTiles("Stamen.TonerLite") %>%
    addLayersControl(
      baseGroups = c("OSM", "Stamen.TonerLite")
    )
  return(m)
}







viaroute <- function(lat1, lng1, lat2, lng2) {
  R.utils::evalWithTimeout({
    repeat {
      res <- try(
        route <- rjson::fromJSON(
          file = paste("http://router.project-osrm.org/route/v1/driving/",
                       lng1, ",", lat1, ";", lng2, ",", lat2,
                       "?overview=full", sep = "", NULL)))
      if (class(res) != "try-error") {
        if (!is.null(res)) {
          break
        }
      }
    }
  }, timeout = 1, onTimeout = "warning")
  return(res)
}



decode_geom <- function(encoded) {
  scale <- 1e-5
  len = str_length(encoded)
  encoded <- strsplit(encoded, NULL)[[1]]
  index = 1
  N <- 100000
  df.index <- 1
  array = matrix(nrow = N, ncol = 2)
  lat <- dlat <- lng <- dlnt <- b <- shift <- result <- 0
  
  while (index <= len) {
    # if (index == 80) browser()
    shift <- result <- 0
    repeat {
      b = as.integer(charToRaw(encoded[index])) - 63
      index <- index + 1
      result = bitOr(result, bitShiftL(bitAnd(b, 0x1f), shift))
      shift = shift + 5
      if (b < 0x20) break
    }
    dlat = ifelse(bitAnd(result, 1),
                  -(result - (bitShiftR(result, 1))),
                  bitShiftR(result, 1))
    lat = lat + dlat;
    
    shift <- result <- b <- 0
    repeat {
      b = as.integer(charToRaw(encoded[index])) - 63
      index <- index + 1
      result = bitOr(result, bitShiftL(bitAnd(b, 0x1f), shift))
      shift = shift + 5
      if (b < 0x20) break
    }
    dlng = ifelse(bitAnd(result, 1),
                  -(result - (bitShiftR(result, 1))),
                  bitShiftR(result, 1))
    lng = lng + dlng
    
    array[df.index,] <- c(lat = lat * scale, lng = lng * scale)
    df.index <- df.index + 1
  }
  
  geometry <- data.frame(array[1:df.index - 1,])
  names(geometry) <- c("lat", "lng")
  return(geometry)
}


map_route <- function(df, my_list) {
  m <- map()
  m <- addCircleMarkers(map = m,
                        lat = df$lat,
                        lng = df$lng,
                        color = "blue",
                        stroke = FALSE,
                        radius = 6,
                        fillOpacity = 0.8) %>%
    addLayersControl(baseGroups = c("OSM", "Stamen.TonerLite")) %>%
    {
      for (i in 1:length(my_list)) {
        . <- addPolylines(., lat = my_list[[i]]$lat, lng = my_list[[i]]$lng, color = "red", weight = 4)
      }
      return(.)
    }
  return(m)
}






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
      
      tempData <- subset(crimeData, 
                         crimeData$PosixctDate > as.POSIXct(strptime(input$startdate, format="%Y-%m-%d")) & crimeData$PosixctDate < as.POSIXct(strptime(input$enddate, format="%Y-%m-%d")) & crimeData$crime == input$crimetype)
      
      commSubset <- subset(commArea,commArea$X2 == input$community)
      tempData <- subset(tempData, tempData$`Community Area` == commSubset$X1)
      tempData <- tempData[c("Date","crime","Description","Arrest")]
      return(tempData)
    })
    
    datetypesubset <- reactive({
      
      tempData <- subset(crimeData, 
                         crimeData$PosixctDate > as.POSIXct(strptime(input$startdate, format="%Y-%m-%d")) & crimeData$PosixctDate < as.POSIXct(strptime(input$enddate, format="%Y-%m-%d")) & crimeData$crime == input$crimetype)
      
      commSubset <- subset(commArea,commArea$X2 == input$community)
      tempData <- subset(tempData, tempData$`Community Area` == commSubset$X1,
                         select = c(PosixctDate,crime,Latitude,Longitude,abc))
      return(tempData)
    })
    
    
    
    datetypesubsetformaps <- reactive({
      
      
      commSubset <- subset(commArea,commArea$X2 == input$source)
      tempData <- subset(crimeData, crimeData$`Community Area` == commSubset$X1,
                         select = c(Latitude,Longitude))
      source <- tempData[1,]
      commSubset1 <- subset(commArea,commArea$X2 == input$destination)
      tempData1 <- subset(crimeData, crimeData$`Community Area` == commSubset1$X1,
                         select = c(Latitude,Longitude))
      destination <- tempData1[1,]
      df <- data.frame(c(source$Latitude,destination$Latitude),c(source$Longitude,destination$Longitude))
      
      return(df)
    })
    
    
    
    
    crimebytimeXTS <- reactive({
      dfin <- datetypesubset()
      df.xts <- xts(x = dfin[, c("crime","PosixctDate")], order.by =dfin$PosixctDate)
      
      if (input$period == "Daily") {crimebytime <- apply.daily(df.xts, function(d) {sum(str_count(d, input$crimetype ))})}
      if (input$period == "Weekly") {crimebytime <- apply.weekly(df.xts, function(d) {sum(str_count(d, input$crimetype ))})}
      if (input$period == "Monthly") {crimebytime <- apply.monthly(df.xts, function(d) {sum(str_count(d, input$crimetype ))})}
      if (input$period == "Yearly") {crimebytime <- apply.yearly(df.xts, function(d) {sum(str_count(d, input$crimetype ))})}
      
      df.xts <- NULL
      return(crimebytime)
    })
    
    
    
    output$datatable <- renderDataTable({
      
      datetypesubsetforsimpledata()
      
    }, options = list(aLengthMenu = c(10, 25, 50, 100, 1000), iDisplayLength = 10))
    
    
    output$map <- renderLeaflet({
      crimebydatetype <- datetypesubset()
      
      
      
      m<-map()
      m <- m %>% addCircleMarkers(lat = crimebydatetype$Latitude,
                                  lng = crimebydatetype$Longitude,
                                  color = "blue",
                                  stroke = FALSE,
                                  radius = 5,
                                  fillOpacity = 1)
      
      
     print(m)
      
      
      
      
    })
    
    
    output$plots <- renderPlot({
      p <- qplot(crimeData$crime,xlab = "Number of Crimes",fill = I("pink"),col = I("red"), main = "Crimes in Chicago")+scale_y_continuous("Crime")
      p <- qplot(crimeData$crime,xlab = "Crime",main = "Crimes in Chicago")+scale_y_continuous("Number of Crimes")
      q <- qplot(crimeData$timeTag, xlab="Time of day", main="Crimes by time of day") + scale_y_continuous("Number of crimes")
      r <- qplot(crimeData$day,xlab = "Day of Week",main="Crimes by day of Week")+scale_y_continuous("Number of crimes")
      s <- qplot(crimeData$month, xlab= "Month", main="Crimes by month")+ scale_y_continuous("Number of crimes")
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
    
    
    output$heatMaps <- renderPlot({
      tempp <- aggregate(crimeData$crime, by=list(crimeData$crime,
                                                   crimeData$timeTag), FUN= length)
      tempq <- aggregate(crimeData$crime, by=list(crimeData$crime,
                                                  crimeData$day), FUN= length)
      tempr <- aggregate(crimeData$crime, by=list(crimeData$crime,
                                                  crimeData$month), FUN= length)
      names(tempp) <- c("crime", "timeTag", "count")
      names(tempq) <- c("crime", "day", "count")
      names(tempr) <- c("crime", "month", "count")
      
      p <- ggplot(tempp,aes(x=factor(timeTag),y=crime))+geom_tile(aes(fill=count))+scale_x_discrete("Crime",expand=c(0,0))+scale_y_discrete("Time of the day",expand = c(0,-2))+scale_fill_gradient("Number of crimes",low="white",high="red")+theme_bw()+ggtitle("Crimes by time of day")+theme(panel.grid.major = element_line(color=NA),panel.grid.minor = element_line(color=NA))
      
      q <- ggplot(tempq,aes(x=factor(day),y=crime))+geom_tile(aes(fill=count))+scale_x_discrete("Crime",expand=c(0,0))+scale_y_discrete("Day of Week",expand = c(0,-2))+scale_fill_gradient("Number of crimes",low="white",high="red")+theme_bw()+ggtitle("Crimes by day of week")+theme(panel.grid.major = element_line(color=NA),panel.grid.minor = element_line(color=NA))
      
      r <- ggplot(tempr,aes(x=factor(month),y=crime))+geom_tile(aes(fill=count))+scale_x_discrete("Crime",expand=c(0,0))+scale_y_discrete("Month",expand = c(0,-2))+scale_fill_gradient("Number of crimes",low="white",high="red")+theme_bw()+ggtitle("Crimes by month")+theme(panel.grid.major = element_line(color=NA),panel.grid.minor = element_line(color=NA))
      
      
      if(input$heatplotselect=="By time"){ print(p)}
      if(input$heatplotselect=="By Day of Week"){ print(q)}
      if(input$heatplotselect=="By Month"){ print(r)}
      
     
    })
    
    output$shortroute <- renderLeaflet({
      
        df <- datetypesubsetformaps()
        
        
        colnames(df) = c("lat","lon") 
        
        df <- structure(list(
          lat = df$lat, 
          lng = df$lon),
          .Names = c("lat", "lng"), 
          row.names = c(NA, 2L), class = "data.frame")
        nn <- nrow(df)
      
        
        m<-map()
        m <- m %>% addCircleMarkers(lat = df$lat,
                                    lng = df$lng,
                                    color = "red",
                                    stroke = FALSE,
                                    radius = 10,
                                    fillOpacity = 0.8)
        
        
        my_list <- list()
        r <- 1
        for (i in 1:(nn-1)) {
          for (j in ((i+1):nn)) {
            my_route <- viaroute(df$lat[i], df$lng[i],df$lat[j], df$lng[j])
            geom <- decode_geom(my_route$routes[[1]]$geometry)
            my_list[[r]] <- geom
            r <- r + 1
          }
        }
        
        print(m)
        
        print(map_route(df, my_list))
      
    })
    
    
    
  }
)
