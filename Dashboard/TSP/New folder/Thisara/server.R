setwd("~/KPMG/Dashboard/TSP/New folder/Thisara")
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
library(readxl)
library(data.table)
library(leaftime)
library(htmltools)
library(geojson)
library(geojsonio)
library(shinyWidgets)
library(datetime)
library(chron)
library(lubridate)

function(input, output, session){
  
  getData <- reactive({
    
    inFile <- input$file1
    
    if (is.null(input$file1))
      return(NULL)
    
    read_xlsx(inFile$datapath)
    
  })
  
  observeEvent(input$show, {
    showNotification("Please Upload your file in excel format. \n ", type = c("message"))
  })
  
  output$intable <- renderDataTable(getData(),options = list(
    autoWidth = FALSE, scrollX = TRUE))
  
  
  #TEST
  # output$intable <- renderDataTable({
  #  
  #     if(ncol(getData())<5)
  #     {
  #       shinyalert("Column Error","Uploaded Data has less than 5 Col",type="error")
  #       returnValue()
  #     }
  #     else if(ncol(getData())>5)
  #     {
  #       shinyalert("Column Error","Uploaded Data has more than 5 Col",type = "error")
  #       returnValue()
  #     }
  #     else
  #     {
  #       return(getData())
  #     }
  #   
  # 
  # })
  
  
  
  
  output$val1 <- renderText(sum(is.na(getData())))
  output$val2 <- renderText(sum(is.na(getData()$Link)))
  output$natab <- renderDataTable(getData()[rowSums(is.na(getData())) > 0,])
  
  # output$val3 <- renderText(sum(is.na(getData()$`Quantity(CBM)`)))
  # output$subdata <-  renderDataTable({getData()[complete.cases(getData()), ]})
  
  observe({
    if (!is.null(input$file1)) {
    kml.coordinates <- getData()
    kml.coordinates$Link <-  gsub(".*q=(.+)&z.*", "\\1", kml.coordinates$Link)
    kml.coordinates <- separate(data = kml.coordinates, col = Link, into = c("lat", "lon"), sep = "%2C")
    kml.coordinates$lat <- as.numeric(kml.coordinates$lat)
    kml.coordinates$lon <- as.numeric(kml.coordinates$lon)
    kml.coordinates$latlon <-paste(kml.coordinates$lat, kml.coordinates$lon, sep = "+") 
    
    output$map1 <- renderLeaflet({leaflet(kml.coordinates) %>%
      addProviderTiles("OpenStreetMap", group = "OSM") %>%
      addCircleMarkers(lat = kml.coordinates$lat,
                       lng = kml.coordinates$lon,
                       popup = kml.coordinates$Location,
                       color = "red",
                       stroke = FALSE,
                       radius = 8,
                       fillOpacity = 0.8) %>%
        addMarkers(data = kml.coordinates[kml.coordinates$Location.Type == "DC", ],
                         lng = ~lon,
                         lat = ~lat,
                         icon = makeIcon("DC.png",20,20),
                       
                         popup = paste("DC"))
                                  })
    
    output$map2 <- renderLeaflet({leaflet(kml.coordinates) %>%
        addProviderTiles("OpenStreetMap", group = "OSM") %>%
        addCircleMarkers(lat = kml.coordinates$lat,
                         lng = kml.coordinates$lon,
                         popup = kml.coordinates$Location,
                         color = "red",
                         stroke = FALSE,
                         radius = 8,
                         fillOpacity = 0.8) %>%
        addMarkers(data = kml.coordinates[kml.coordinates$Location.Type == "DC", ],
                         lng = ~lon,
                         lat = ~lat,
                         icon = makeIcon("DC.png",20,20),
                         
                         popup = paste("DC"))
    })


    }
    else {
      return(NULL)
    }
  })
  

  # coordvals <- eventReactive(input$goButton1, {
  #     if (is.null(input$file1)) return()
  #     # showModal(modalDialog("Calculating!", footer=NULL))
  #     kml.coordinates <- getData()
  #     kml.coordinates$Link <-  gsub(".*q=(.+)&z.*", "\\1", kml.coordinates$Link)
  #     kml.coordinates <- separate(data = kml.coordinates, col = Link, into = c("lat", "lon"), sep = "%2C")
  #     kml.coordinates$lat <- as.numeric(kml.coordinates$lat)
  #     kml.coordinates$lon <- as.numeric(kml.coordinates$lon)
  #     kml.coordinates$latlon <-paste(kml.coordinates$lat, kml.coordinates$lon, sep = "+")
  #     register_google(key = "AIzaSyC5LYaLNN7w5gQh3S6Sot6jgAYtV0a8raM")
      # set.api.key("AIzaSyC5LYaLNN7w5gQh3S6Sot6jgAYtV0a8raM")
  #     distances <- gmapsdistance(origin = kml.coordinates$latlon,
  #                                destination = kml.coordinates$latlon,
  #                                combinations = "all",
  #                                mode = "driving")$Distance[, -1]
  #     distances <- as.matrix(distances) / 1000
  #     colnames(distances) <- kml.coordinates$Location
  #     rownames(distances) <- kml.coordinates$Location
  #     return(distances)
  #     # distances2 <- setDT(distances, keep.rownames = TRUE)[] # check
  #     
  #     # output$mat1 <- renderTable(distances)
  #     # removeModal()
  #   })
  # output$mat1 <- renderDataTable(coordvals(), rownames = T,  options = list(
  #   autoWidth = FALSE, scrollX = TRUE))
  
  
    

 
    observeEvent(input$goButton1, {
      showModal(modalDialog(title = 'Calculating distance matrix',withSpinner(tagList(renderText("")),type=5,color.background = '#FFFFFF',size = .25,proxy.height = '50px'),footer = NULL,size = 's'))
      register_google(key = "AIzaSyC5LYaLNN7w5gQh3S6Sot6jgAYtV0a8raM")
      set.api.key("AIzaSyC5LYaLNN7w5gQh3S6Sot6jgAYtV0a8raM")
      distances2 <- read.csv("Distance_mat.csv")
      distances2 <- as.matrix(distances2)
      rownames(distances2) <- colnames(distances2)
      kml.coordinates <- read.csv("kml.coordinates.csv")
      dis_time_tour2 <- read.csv("Time_tour.csv")
      temp1 <- read_csv("time_map2.csv")
      
      output$mat1 <- renderDataTable({
       datatable(distances2, rownames = T,options = list(autoWidth = FALSE, scrollX = TRUE))
      })
      # distances3 <- coordvals()
      # kml.coordinates <- getData()
      # kml.coordinates$Link <-  gsub(".*q=(.+)&z.*", "\\1", kml.coordinates$Link)
      # kml.coordinates <- separate(data = kml.coordinates, col = Link, into = c("lat", "lon"), sep = "%2C")
      # kml.coordinates$lat <- as.numeric(kml.coordinates$lat)
      # kml.coordinates$lon <- as.numeric(kml.coordinates$lon)
      # kml.coordinates$latlon <-paste(kml.coordinates$lat, kml.coordinates$lon, sep = "+")
      distances <- as.dist(distances2)
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

      path <- cut_tour(test5[[1]], cut = "Point29", exclude_cut = FALSE)

      tour_order <- as.integer(path)
      kml.coordinates <- kml.coordinates[tour_order,]
      kml.coordinates$Tour_Number <- 1:nrow(kml.coordinates)
# check
      origin1 <- kml.coordinates[,c("Location","lon","lat")]
      destination <- rbind(kml.coordinates[2:nrow(kml.coordinates),], kml.coordinates[1, ])[,c("Location","lon","lat")]
      new_tour <- cbind(origin1, destination)
      colnames(new_tour)[4] <- "des"
      dis_time_tour <- new_tour[,c("Location", "des")]
      dis_time_tour <- base::merge(dis_time_tour, kml.coordinates[, c("Location","latlon")], by = "Location", all.x = T, sort = F)
      dis_time_tour <- base::merge(dis_time_tour, kml.coordinates[, c("Location","latlon")], by.x = "des", by.y = "Location", all.x = T, sort=F)
      dis_time_tour <- dis_time_tour[,c("Location", "des", "latlon.x", "latlon.y")]

      dis_time_tour <- dis_time_tour%>%
        nest(c(latlon.x,latlon.y)) %>%
        mutate(models = lapply(data,
                               function(df){gmapsdistance(origin = df$latlon.x,
                                                          destination = df$latlon.y,
                                                          mode = "driving",
                                                          combinations = "pairwise")})) %>%
        mutate( dfs = lapply(models, function(x){  bind_cols(x) })) %>%
        unnest(dfs)
      # maxi <- 50
      # for (i in 1:maxi) {
      #   updateProgressBar(session = session, id = "progress", value = (i/maxi)*100)
      #   Sys.sleep(0.2)
      # }
      dis_time_tour$Time <- dis_time_tour$Time/3600
      dis_time_tour$Distance <- dis_time_tour$Distance/1000


      # new_tour <- new_tour[,-4]
      # new_tour <- new_tour[,c("Location","lat","lon","lat.1","lon.1")]
      # mykey <- "AIzaSyC5LYaLNN7w5gQh3S6Sot6jgAYtV0a8raM"
      # lapply(1:nrow(new_tour), function(x){
      # 
      #   foo <- google_directions(origin = unlist(new_tour[x, 2:3]),
      #                            destination = unlist(new_tour[x, 4:5]),
      #                            key = mykey,
      #                            mode = "driving",
      #                            simplify = TRUE)
      # 
      #   pl <- decode_pl(foo$routes$overview_polyline$points)
      # 
      #   return(pl)
      # 
      # }
      # ) %>%
      #   bind_rows(.id = "Point") -> temp
      ###########CHECK#################
      kml.coordinates <- read.csv("kml.coordinates.csv")
      dis_time_tour2 <- read.csv("Time_tour.csv")
      temp1 <- read.csv("time_map2.csv")
      dis_time_tour2$Time <- dis_time_tour2$Time/60
      dis_time_tour2$tcumsum <- cumsum(dis_time_tour2$Time)
      dis_time_tour2$Time2 <- substr(times((dis_time_tour2$tcumsum%/%60 +  dis_time_tour2$tcumsum%%60 /60)/24), 1, 5)
      # dis_time_tour2$Time2 <- as.time(dis_time_tour2$Time2)
      dis_time_tour2$system <- Sys.time()
      # dis_time_tour2$system <- as.datetime(dis_time_tour2$system)
      dis_time_tour2$Time2 <- as.POSIXct(dis_time_tour2$Time2,format="%H:%M")
      dis_time_tour2$start <- dis_time_tour2$system + minutes(as.integer(dis_time_tour2$tcumsum))
      dis_time_tour2$end <- max(dis_time_tour2$start)
      power3 <- cbind(temp1[,c(1:3)], dis_time_tour2[,c("start", "end")])
      
      colnames(power3)[2] <- "Latitude"
      colnames(power3)[3] <- "Longitude"
      power3$start <- as.character(power3$start)
      power3$end <- as.character(power3$end)
     
      # power3$start <- paste0(power3$start,"HH:MM")
      # power$end <- paste0(power3$end, "HH:MM")
      # power3$start <- strptime(power3$start, format="%Y-%m-%d %H:%M:%S")
      
      power3$start <- as.datetime(power3$start, "%Y-%m-%d %H:%M:%S")
      power3$end <- as.datetime(power3$end, "%Y-%m-%d %H:%M:%S")
      
      
      # power <- temp
      # colnames(power)[2] <- c("Latitude")
      # colnames(power)[3] <- c("Longitude")
      # power$start <- seq.Date(as.Date("2015-01-01"), by = "day", length.out = nrow(power))
      # date <- max(power$start)
      # power$end <- seq.Date(as.Date(date), by = "day", along.with = nrow(power))
      power_geo <- geojsonio::geojson_json(power3,lat="Latitude",lon="Longitude")

      output$map1 <- renderLeaflet({leaflet(power_geo) %>%
          addProviderTiles("OpenStreetMap.Mapnik")%>%
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
          addMarkers(data = kml.coordinates[kml.coordinates$Location.Type == "DC", ],
                           lng = ~lon,
                           lat = ~lat,
                           icon = makeIcon("DC.png",20,20),
                          
                           popup = paste("DC"))%>%
          addTimeline(
            sliderOpts = sliderOptions(
              formatOutput = htmlwidgets::JS(
                "function(date) {return new Date(date).toTimeString()}"
                ),
              position = "bottomright",
              step = 500,
              duration = 200,
              showTicks = FALSE
              )
          )
          
        })
      output$map2 <- renderLeaflet({leaflet() %>%
        addProviderTiles("OpenStreetMap.Mapnik") %>%
        addPolylines(data = temp, lng = ~lon, lat = ~lat, group = ~Point) %>%
        addCircleMarkers(data = kml.coordinates[kml.coordinates$Location.Type != "DC", ],
                         lat = ~lat,
                         lng = ~lon,
                         popup = paste("Address: ", kml.coordinates$Adress,"<br/>",
                                       "Order No: ", kml.coordinates$Order.No., "<br/>",
                                       "Quantity: ", kml.coordinates$`Quantity(CBM)`, "<br/>"),
                         color = "red",
                         stroke = FALSE,
                         radius = 7,
                         fillOpacity = 0.8) %>%
        addMarkers(data = kml.coordinates[kml.coordinates$Location.Type == "DC", ],
                   lng = ~lon,
                   lat = ~lat,
                   icon = makeIcon("DC.png",20,20),
            
                   popup = paste("DC"))
          })
      output$val11  <- renderText( paste0(tour_length(test5[[1]]),'km'))
      output$val12  <- renderText( paste0(sum(round(dis_time_tour$Time, digits=2)),'hrs'))
      output$val13  <- renderText(length(unique(kml.coordinates$Location)))
      output$val14  <- renderText(sum(kml.coordinates$Quantity.CBM.))
      dtable <- kml.coordinates
      dtable <- dtable[-1,-c(2,3,4,8)]

      output$dwtable <- renderDataTable(dtable,options = list(
        autoWidth = FALSE, scrollX = TRUE))

      output$download <- downloadHandler(
        filename = function(){"Optimzed Route.csv"},
        content = function(fname){
          write.csv(dtable, fname, row.names = F)
        }
      )
     
      removeModal()
    
  })

}