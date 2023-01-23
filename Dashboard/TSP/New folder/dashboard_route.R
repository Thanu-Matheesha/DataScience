setwd("~/KPMG/Dashboard/TSP/New folder")
if(interactive()) {
  
  
  library(leaflet)
  library(leaftime)
  library(htmltools)
  library(chron)
  load("dynamic_route.RData")
  
  power <- temp
  colnames(power)[2] <- c("Latitude")
  colnames(power)[3] <- c("Longitude")
  power$start <- seq(Sys.Date(), by = "day", length.out = nrow(power))
  date <- max(power$start)
  power$end <- seq.Date(as.Date(date), by = "day", along.with = nrow(power))
  
  power2 <- temp
  colnames(power2)[2] <- c("Latitude")
  colnames(power2)[3] <- c("Longitude")
  power2$start <- seq.Date(as.Date("2015-01-01"), by = "day", length.out = nrow(power2))
  power2$end <- seq.Date(as.Date("2015-01-01"), by = "day", length.out = nrow(power2)) + 1
  #Build data.frame with 10 obs + 3 cols
  # power <- data.frame(
  #   "Latitude" = c(
  #     33.515556, 38.060556, 47.903056, 49.71, 49.041667, 31.934167,
  #     54.140586, 54.140586, 48.494444, 48.494444
  #   ),
  #   "Longitude" = c(
  #     129.837222, -77.789444, 7.563056, 8.415278, 9.175, -82.343889,
  #     13.664422, 13.664422, 17.681944, 17.681944
  #   ),
  #   "start" = seq.Date(as.Date("2015-01-01"), by = "day", length.out = 10),
  #   "end" = seq.Date(as.Date("2015-01-01"), by = "day", length.out = 10) + 1
  # )
  
  # use geojsonio to convert our data.frame
  #  to GeoJSON which timeline expects
  power_geo <- geojsonio::geojson_json(power3,lat="Latitude",lon="Longitude")
  
  # # we can add data in addTimeline
  # leaflet() %>%
  #   addTiles() %>%
  #   setView(44.0665,23.74667,2) %>%
  #   addTimeline(data = power_geo)
  # 
  # # or we can add data in leaflet()
  # leaflet(power_geo) %>%
  #   addTiles() %>%
  #   setView(44.0665,23.74667,2) %>%
  #   addTimeline()
  
  # we can control the slider controls through sliderOptions
  leaflet(power_geo) %>%
    addProviderTiles("OpenStreetMap.Mapnik")%>%
    # addPolylines(data=power2, lng = ~Longitude, lat = ~Latitude, group = ~Point) %>%
    # setView(44.0665,23.74667,2) %>%
    addCircleMarkers(data = kml.coordinates[kml.coordinates$Location.Type != "DC", ],
                     lat = ~lat,
                     lng = ~lon,
                     popup = paste("Address: ", kml.coordinates$Location,"<br/>",
                                   "Order No: ", kml.coordinates$Order.No., "<br/>",
                                   "Quantity: ", kml.coordinates$`Quantity(CBM)`, "<br/>"),
                     color = "red",
                     stroke = FALSE,
                     radius = 7,
                     fillOpacity = 0.8) %>%
    addCircleMarkers(data = kml.coordinates[kml.coordinates$Location.Type == "DC", ],
                     lng = ~lon,
                     lat = ~lat,
                     # icon = makeIcon("DC.png",20,20),
                     color = "green",
                     popup = paste("DC"))%>%
    addTimeline(
      timelineOpts = timelineOptions(
        styleOptions = styleOptions(
          radius = 4,
          color = "yellow",
          fillColor = "yellow",
          fillOpacity = 2
        )
      ),
      sliderOpts = sliderOptions(
        formatOutput = htmlwidgets::JS(
          
          "function(date) {return new Date(date).toDateString()}"
          ),
        position = "bottomright",
        step = 500,
        duration = 200,
        showTicks = F
        )
    )
  
  # we can control the timeline through timelineOptions
  #  wondering what should be the default
  #  currently timeline uses marker
  leaflet(power_geo) %>%
    addTiles() %>%
    setView(44.0665,23.74667,2) %>%
    addTimeline(
      timelineOpts = timelineOptions(
        pointToLayer = htmlwidgets::JS(
          "
          function(data, latlng) {
          return L.circleMarker(latlng, {
          radius: 3
          })
          }
          "
        ),
        style = NULL
        )
    )
  
  
  
  }
leaflet()%>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addCircleMarkers(lat=6.87204,
                   lng=79.86845)
  