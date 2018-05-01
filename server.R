
registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}
############################################################################################

map <- function() {
  m <- leaflet() %>%
    addTiles(group = "OSM") %>%
    addProviderTiles("Stamen.TonerLite") %>%
    addLayersControl(
      baseGroups = c("OSM", "Stamen.TonerLite")
    )
  return(m)
}

############################################################################################

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

############################################################################################

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

############################################################################################


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
  
  m <- m %>% addTiles() %>%
    fitBounds(min(chota$Longitude), min(chota$Latitude), max(chota$Longitude),     max(chota$Latitude)) %>%
    registerPlugin(heatPlugin) %>%
    onRender("function(el, x, data) {
             data = HTMLWidgets.dataframeToD3(data);
             data = data.map(function(val) { return [val.Latitude, val.Longitude, .05]; });
             L.heatLayer(data, {radius: 25}).addTo(this);
}", data = chota %>% select(Latitude, Longitude))
  
  
  
  m <- m%>% setView(-87.78206,41.93177,zoom =10)
  
  return(m)
  }


############################################################################################




xtsMelt <- function(data) {
  require(reshape2)
  
  data.df <- data.frame(cbind(format(index(data),"%Y-%m-%d"),coredata(data)))
  colnames(data.df)[1] = "date"
  data.melt <- melt(data.df,id.vars=1,stringsAsFactors=FALSE)
  colnames(data.melt) <- c("date","indexname","value")
  
  data.melt[,"indexname"] <- apply(matrix(data.melt[,"indexname"]),2,gsub,pattern="[.]",replacement="")
  return(data.melt)
  #return(df2json(na.omit(data.melt)))
}

############################################################################################

shinyServer
(
  
  function(input, output) 
  {
    
    ############################################################################################   
    
    datetypesubsetforsimpledata <- reactive({
      
      tempData <- subset(crimeData, 
                         crimeData$PosixctDate > as.POSIXct(strptime(input$startdate, format="%Y-%m-%d")) & crimeData$PosixctDate < as.POSIXct(strptime(input$enddate, format="%Y-%m-%d")) & crimeData$crime == input$crimetype)
      
      commSubset <- subset(commArea,commArea$X2 == input$community)
      tempData <- subset(tempData, tempData$`Community Area` == commSubset$X1)
      tempData <- tempData[c("Date","crime","Description","Arrest")]
      return(tempData)
    })
    
    ############################################################################################    
    
    datetypesubset <- reactive({
      
      tempData <- subset(crimeData, 
                         crimeData$PosixctDate > as.POSIXct(strptime(input$startdate, format="%Y-%m-%d")) & crimeData$PosixctDate < as.POSIXct(strptime(input$enddate, format="%Y-%m-%d")) & crimeData$crime == input$crimetype)
      
      commSubset <- subset(commArea,commArea$X2 == input$community)
      tempData <- subset(tempData, tempData$`Community Area` == commSubset$X1,
                         select = c(PosixctDate,crime,Latitude,Longitude,abc))
      return(tempData)
    })
    
    
    
    ############################################################################################    
    
    datetypesubset2 <- reactive({
      
      tempData <- subset(chota, chota$crime == input$crimetype2,select = c(ZIP))
      tempData <- as.data.frame(table(tempData$ZIP))
      return(tempData)
    })
    
    ############################################################################################    
    
    datetypesubset1 <- reactive({
      
      tempData <- subset(crimeData, 
                         crimeData$PosixctDate > as.POSIXct(strptime(input$startdate1, format="%Y-%m-%d")) & crimeData$PosixctDate < as.POSIXct(strptime(input$enddate1, format="%Y-%m-%d")) & crimeData$crime == input$crimetype1)
      
      commSubset <- subset(commArea,commArea$X2 == input$community1)
      tempData <- subset(tempData, tempData$`Community Area` == commSubset$X1,
                         select = c(PosixctDate,crime,Latitude,Longitude,abc))
      return(tempData)
    })
    
    ############################################################################################    
    
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
    
    ############################################################################################   
    
    
    crimebytimeXTS <- reactive({
      dfin <- datetypesubset1()
      df.xts <- xts(x = dfin[, c("crime","PosixctDate")], order.by =dfin$PosixctDate)
      
      if (input$period == "Daily") {crimebytime <- apply.daily(df.xts, function(d) {sum(str_count(d, input$crimetype1 ))})}
      if (input$period == "Weekly") {crimebytime <- apply.weekly(df.xts, function(d) {sum(str_count(d, input$crimetype1 ))})}
      if (input$period == "Monthly") {crimebytime <- apply.monthly(df.xts, function(d) {sum(str_count(d, input$crimetype1 ))})}
      if (input$period == "Yearly") {crimebytime <- apply.yearly(df.xts, function(d) {sum(str_count(d, input$crimetype1 ))})}
      
      df.xts <- NULL
      return(crimebytime)
    })
    
    ############################################################################################ 
    mergeFinal <- reactive({
      
      
      #if(input$facility=="PUBLIC FACILITIES"){return(merge(mainDatazips,,by ="Var1"))}
      if(input$facility =="HOUSE"){mergeFinal<-mergeR}
      if(input$facility  =="FOOD"){mergeFinal<- mergeF}
      if(input$facility  =="BAR"){mergeFinal<-mergeB}
      
      mergeFinal <- as.data.frame(mergeFinal)
      return(mergeFinal)
      
    })
    
    
    ############################################################################################ 
    
    output$datatable <- renderDataTable({
      
      datetypesubsetforsimpledata()
      
    }, options = list(aLengthMenu = c(10, 25, 50, 100, 1000), iDisplayLength = 10))
    
    
    ############################################################################################
    
    output$map <- renderLeaflet({
      crimebydatetype <- datetypesubset()
      
      m<-map()
      m <- m %>% addCircleMarkers(lat = crimebydatetype$Latitude,
                                  lng = crimebydatetype$Longitude,
                                  color = "red",
                                  stroke = FALSE,
                                  radius = 4,
                                  fillOpacity = .9)
      
      
      
      
      
      
      
    })
    
    ############################################################################################
    
    
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
    
    
    
    ############################################################################################
    
    
    output$analysis <- renderChart2({
      
      crimebytime <-crimebytimeXTS()
      
      #Convert data using xtsMelt for highcharts plot
      ust.melt <- na.omit(xtsMelt(crimebytime))
      ust.melt$date2 <- as.Date(ust.melt$date, format = "%Y-%m-%d")
      ust.melt$Crime <- as.numeric(as.character(ust.melt$value))
      ust.melt$date4  <- as.numeric(as.POSIXct(ust.melt$date2, origin="1970-01-01")) * 1000
      
      #Highchart plot
      h1 <- hPlot(
        Crime ~ date4,  
        data = ust.melt, 
        color = '#4572A7',
        type = 'spline',
        title = paste("Crimes for ",input$crimetype1)
      ) 
      h1$xAxis(type = "datetime")
      h1$params$width <- 700
      
      h1 
    })
    
    
    ############################################################################################
    
    
    
    output$heatmaps <- renderPlot({
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
      
      
      if(input$heatplotselect=="By time"){print(p) }
      if(input$heatplotselect=="By Day of Week"){print(q) }
      if(input$heatplotselect=="By Month"){ print(r)}
      
      
    })
    
    
    ############################################################################################
    
    
    
    output$saferoute <- renderLeaflet({
      
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
      
      
      m <- m %>% addTiles() %>%
        fitBounds(min(chota$Longitude), min(chota$Latitude), max(chota$Longitude),     max(chota$Latitude)) %>%
        registerPlugin(heatPlugin) %>%
        onRender("function(el, x, data) {
                 data = HTMLWidgets.dataframeToD3(data);
                 data = data.map(function(val) { return [val.Latitude, val.Longitude, .05]; });
                 L.heatLayer(data, {radius: 25}).addTo(this);
    }", data = chota %>% select(Latitude, Longitude))
        
      
      
      
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
      
      
      
      
      
      
      print(map_route(df, my_list))
      
  })
    
    
    ############################################################################################
    
    output$publicf<-renderHighchart({
      
      
      
      hc<-hchart(mergeMore,"scatter",hcaes(x = mergeMore$CRIME, y =mergeMore$BAR,label=mergeMore$ZIP,group = ZIP))%>% 
        hc_xAxis(title=list(text = 'Number of Public Facilities')) %>% 
        hc_yAxis(title=list(text='Number of Crimes')) %>% 
        hc_title(text = "Crime Against Public Facility distribution by Zipcode")%>% hc_add_theme(hc_theme_flat())
      
      
      
    })
    
    
    
    
    }
    )
