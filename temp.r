
df <- structure(list(
  lat = c(-33.9409444, -33.9335713, -33.9333906, -33.9297826), 
  lng = c(18.5001774, 18.5033218, 18.518719, 18.5209372)),
  .Names = c("lat", "lng"), 
  row.names = c(NA, 4L), class = "data.frame")
nn <- nrow(df)

# Functions
# =========
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

map <- function() {
  m <- leaflet() %>%
    addTiles(group = "OSM") %>%
    addProviderTiles("Stamen.TonerLite") %>%
    addLayersControl(
      baseGroups = c("OSM", "Stamen.TonerLite")
    )
  return(m)
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

# Main
# ======
m <- map()
m <- m %>% addCircleMarkers(lat = df$lat,
                            lng = df$lng,
                            color = "red",
                            stroke = FALSE,
                            radius = 10,
                            fillOpacity = 0.8)
print(m)

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
print()







print(geocode("Northwestern, Chicago"))





