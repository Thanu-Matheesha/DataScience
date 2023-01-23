setwd("~/KPMG/Dashboard/TSP")
library(sp)
library(maptools)
library(dplyr)
library(ggplot2)
library(ggmap)
library(gmapsdistance)
library(TSP)
library(purrr)
library(shiny)
library(leaflet)
library(osrm)
library(tidyverse)
library(DT)
library(googleway)
library(dplyr)

function(input, output, session){
  
  getData <- reactive({
    
    inFile <- input$file1
    
    if (is.null(input$file1))
      return(NULL)
    
    read.csv(inFile$datapath, stringsAsFactors = F)
    
  })
  
  observe({
    if (!is.null(input$file1)) {
    kml.coordinates <- getData()
    output$map1 <- renderLeaflet({leaflet(kml.coordinates) %>%
      addProviderTiles("OpenStreetMap", group = "OSM") %>%
      addCircleMarkers(lat = kml.coordinates$lat,
                       lng = kml.coordinates$lon,
                       popup = kml.coordinates$Location,
                       color = "red",
                       stroke = FALSE,
                       radius = 8,
                       fillOpacity = 0.8) %>%
        addCircleMarkers(data = kml.coordinates[kml.coordinates$Location.Type == "DC", ],
                         lng = ~lon,
                         lat = ~lat,
                         # icon = makeIcon("DC.png",20,20),
                         color = "green",
                         popup = paste("DC"))
                                  })
    
    
    }
    else {
      return(NULL)
    }
    
    observeEvent(input$goButton, {
      if (is.null(input$file1)) return()
      showModal(modalDialog("Optimizing!", footer=NULL))
      register_google(key = "AIzaSyC5LYaLNN7w5gQh3S6Sot6jgAYtV0a8raM")
      set.api.key("AIzaSyC5LYaLNN7w5gQh3S6Sot6jgAYtV0a8raM")
      distances <- gmapsdistance(origin = kml.coordinates$latlon,
                                 destination = kml.coordinates$latlon,
                                 combinations = "all",
                                 mode = "driving")$Distance[, -1]
      distances <- as.matrix(distances) / 1000
      colnames(distances) <- kml.coordinates$Location
      rownames(distances) <- kml.coordinates$Location
      distances <- as.dist(distances)
      tsp <- TSP(distances)
      methods <- c(
        "nearest_insertion",
        "farthest_insertion",
        "cheapest_insertion",
        "arbitrary_insertion",
        "nn",
        "repetitive_nn",
        "two_opt"
      )
      tours <- methods %>% map(function(method) {
        solve_TSP(tsp, method)
      })
      tour <- solve_TSP(tsp)
      
      test <- min(sapply(tours, function(x) tour_length(x)))
      test2 <- Filter(function(x) tour_length(x) == test, tours)
      test3 <- list(tour, test2[[1]])
      test4 <- min(sapply(test3, function(x) tour_length(x)))
      test5 <- Filter(function(x) tour_length(x) == test4, test3)
      
      path <- cut_tour(test5[[1]], cut = "DC", exclude_cut = FALSE)
      
      tour_order <- as.integer(path)
      kml.coordinates <- kml.coordinates[tour_order,]
      kml.coordinates$Tour_Number <- 1:nrow(kml.coordinates) 
      
      origin1 <- kml.coordinates[,c(1:3)]
      destination <- rbind(kml.coordinates[2:nrow(kml.coordinates),], kml.coordinates[1, ])[,c(1:3)]
      new_tour <- cbind(origin1, destination)
      colnames(new_tour)[4] <- "des"
      dis_time_tour <- new_tour[,c("Location", "des")]
      dis_time_tour <- base::merge(dis_time_tour, kml.coordinates[, c(1,4)], by = "Location", all.x = T, sort = F)
      dis_time_tour <- base::merge(dis_time_tour, kml.coordinates[, c(1,4)], by.x = "des", by.y = "Location", all.x = T, sort=F)
      dis_time_tour <- dis_time_tour[,c("Location", "des", "latlon.x", "latlon.y")]
      
      dis_time_tour <- dis_time_tour %>%
        nest(c(latlon.x,latlon.y)) %>%
        mutate(models = lapply(data, 
                               function(df){gmapsdistance(origin = df$latlon.x,
                                                          destination = df$latlon.y,
                                                          mode = "driving",
                                                          combinations = "pairwise")})) %>%
        mutate( dfs = lapply(models, function(x){  bind_cols(x) })) %>%
        unnest(dfs)
      dis_time_tour$Time <- dis_time_tour$Time/3600
      dis_time_tour$Distance <- dis_time_tour$Distance/1000
      
      
      new_tour <- new_tour[,-4]
      new_tour <- new_tour[,c("Location","lat","lon","lat.1","lon.1")]
      mykey <- "AIzaSyC5LYaLNN7w5gQh3S6Sot6jgAYtV0a8raM"
      lapply(1:nrow(new_tour), function(x){
        
        foo <- google_directions(origin = unlist(new_tour[x, 2:3]),
                                 destination = unlist(new_tour[x, 4:5]),
                                 key = mykey,
                                 mode = "driving",
                                 simplify = TRUE)
        
        pl <- decode_pl(foo$routes$overview_polyline$points)
        
        return(pl)
        
      }
      ) %>%
        bind_rows(.id = "Location") -> temp
      
      output$map1 <- renderLeaflet({leaflet() %>%
          addProviderTiles("OpenStreetMap.Mapnik") %>%
          addPolylines(data = temp, lng = ~lon, lat = ~lat, group = ~Location) %>%
          addCircleMarkers(data = kml.coordinates[kml.coordinates$Location.Type != "DC", ],
                           lat = ~lat,
                           lng = ~lon,
                           popup = paste("Address: ", kml.coordinates$Address,"<br/>",
                                         "Order No: ", kml.coordinates$Order.No., "<br/>",
                                         "Quantity: ", kml.coordinates$Quantity.CBM., "<br/>"),
                           color = "red",
                           stroke = FALSE,
                           radius = 7,
                           fillOpacity = 0.8) %>%
          addCircleMarkers(data = kml.coordinates[kml.coordinates$Location.Type == "DC", ],
                     lng = ~lon,
                     lat = ~lat,
                     # icon = makeIcon("DC.png",20,20),
                     color = "green",
                     popup = paste("DC"))})
      
      

      # tour_order <- as.integer(tour)
      # kml.coordinates <- kml.coordinates[tour_order,]
      # route <- lapply(seq(nrow(kml.coordinates) - 1), function(n) {
      #   print(n)
      #   route(kml.coordinates$latlon[n], kml.coordinates$latlon[n+1], structure = "route") %>%
      #     mutate(section = n)
      # })
      # route <- route %>% bind_rows()
      # map <- get_map(location = c(lon = 79.863967, lat = 6.875159), zoom = 14, maptype = "roadmap")
      # 
      # kml.coordinates$lon <- as.numeric(as.character(kml.coordinates$lon))
      # kml.coordinates$lat <- as.numeric(as.character(kml.coordinates$lat))
      # 
      # trips <- osrmTrip(kml.coordinates, returnclass="sf")
      # trip <- trips[[1]]$trip
     
      # output$map1 <- renderLeaflet({leaflet(trip) %>% 
      #     addProviderTiles("OpenStreetMap", group = "OSM") %>% 
      #     addPolylines() %>%
      #     addCircleMarkers(lat = kml.coordinates$lat,
      #                      lng = kml.coordinates$lon,
      #                      popup = paste("Address: ", kml.coordinates$Address,"<br/>",
      #                                    "Order No: ", kml.coordinates$Order.No., "<br/>",
      #                                    "Quantity: ", kml.coordinates$Quantity..CBM., "<br/>"),
      #                      color = "red",
      #                      stroke = FALSE,
      #                      radius = 8,
      #                      fillOpacity = 0.8)})
      
      output$val1  <- renderText(tour_length(test5[[1]]))
      output$val2  <- renderText(sum(dis_time_tour$Time))
      output$val3  <- renderText(length(unique(kml.coordinates$Location)))
      output$val4  <- renderText(sum(kml.coordinates$Quantity.CBM.))
      
      output$table <- renderDataTable(kml.coordinates)
      
      output$download <- downloadHandler(
        filename = function(){"Optimzed Route.csv"}, 
        content = function(fname){
          write.csv(kml.coordinates, fname, row.names = F)
        }
      )
      removeModal()
    })
  })
  
}