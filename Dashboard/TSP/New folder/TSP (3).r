setwd("~/KPMG/Dashboard/TSP/New folder")
library(sp)
library(maptools)
library(dplyr)
library(ggplot2)
library(ggmap)
library(gmapsdistance)
library(TSP)
library(purrr)
library(leaflet)
library(readxl)
library(chron)
library(lubridate)
library(datetime)

load("TSP_data5.RData")
# tttttt <- dis_time_tour
# tttttt$Time <- tttttt$Time*60
# tttttt$Time <- substr(times((tttttt$Time%/%60 +  tttttt$Time%%60 /60)/24), 1, 5)
# tttttt$Time <- strptime(x = tttttt$Time, format = "%M:%S")
# KML.text <- readLines("TSP Data.kml")
# re <- " *([^<]+?) *<\\/coordinates>"
# coords <- grep(re,KML.text)
# coords <- coords-1
# re3 <- "*([^<]+?) *<\\/name>" 
# Name <- grep(re3, KML.text) 
# Name <- Name[Name > 47]
# kml.coordinates <- matrix(0,length(coords),4,dimnames=list(c(),c("Location","lon","lat","ELEV")))
# x <- "name"
# 
# for(i in 1:length(coords)){  
#   sub.coords <- coords[i]  
#   temp1 <- gsub(""," ",KML.text[sub.coords])  
#   #temp2 <- gsub(""," ",temp1)  
#   coordinates <-as.numeric( gsub("\\s", "", unlist(strsplit(temp1,","))))
#   
#   sub.Name <- Name[i]  
#   NAME <- gsub("","",KML.text[sub.Name])  
#   NAME <- as.character(gsub("", "", unlist(strsplit(NAME,"[>/<]"))))
#   NAME <- gsub("\\s", "", NAME)
#   NAME <- NAME[nchar(NAME) > 0]
#   NAME <- NAME[!NAME %in% x]
#   
#   kml.coordinates[i,] <- matrix(c(NAME,coordinates),ncol=4)  
# }  
# 
# kml.coordinates <- as.data.frame(kml.coordinates)
# kml.coordinates <- kml.coordinates[, -4]
# kml.coordinates$latlon <- paste0(kml.coordinates$lat,"+",kml.coordinates$lon)
# write.csv(kml.coordinates, "kml.coordinates.csv", row.names = F)
kml.coordinates <- read.csv("kml.coordinates.csv")
kml.coordinates <- read_xlsx("Test1_3.xlsx")
kml.coordinates$Link <-  gsub(".*q=(.+)&z.*", "\\1", kml.coordinates$Link)

kml.coordinates <- separate(data = kml.coordinates, col = Link, into = c("lat", "lon"), sep = "%2C")
kml.coordinates$lat <- as.numeric(kml.coordinates$lat, digits=16)
kml.coordinates$lon <- as.numeric(kml.coordinates$lon, digits=16)
kml.coordinates$latlon <-paste(kml.coordinates$lat, kml.coordinates$lon, sep = "+")
kml.coordinates$latlon <- paste0(kml.coordinates$Latitude,"+",kml.coordinates$Longitude)
register_google(key = "AIzaSyC5LYaLNN7w5gQh3S6Sot6jgAYtV0a8raM")
set.api.key("AIzaSyC5LYaLNN7w5gQh3S6Sot6jgAYtV0a8raM")
distances <- gmapsdistance(origin = kml.coordinates$latlon,
                           destination = kml.coordinates$latlon,
                           combinations = "all",
                           mode = "driving")$Distance[, -1]
distances <- as.matrix(distances) / 1000
colnames(distances) <- kml.coordinates$Location
rownames(distances) <- kml.coordinates$Location
write.csv(distances, "Distance_mat.csv", row.names = F)
distances2 <- as.dist(distances)
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



# tour_distance <- lapply(tours, function(i)tour_length(i))
# #tour_new <- ifelse(any(sapply(tour_distance,function(x) x < tour_length(tour))) == TRUE, )

tour_order <- as.integer(path)
kml.coordinates <- kml.coordinates[tour_order,]
kml.coordinates$Tour_Number <- 1:nrow(kml.coordinates) 

library(dplyr)
library(googleway)

origin1 <- kml.coordinates[,c(1:3)]
destination <- rbind(kml.coordinates[2:nrow(kml.coordinates),], kml.coordinates[1, ])[,c(1:3)]
new_tour <- cbind(origin1, destination)
colnames(new_tour)[4] <- "des"
dis_time_tour <- new_tour[,c("Location", "des")]
dis_time_tour <- base::merge(dis_time_tour, kml.coordinates[, c(1,4)], by = "Location", all.x = T, sort = F)
dis_time_tour <- base::merge(dis_time_tour, kml.coordinates[, c(1,4)], by.x = "des", by.y = "Location", all.x = T, sort=F)
dis_time_tour <- dis_time_tour[,c("Location", "des", "latlon.x", "latlon.y")]

library(tidyverse)

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
  bind_rows(.id = "Point") -> temp

m <- leaflet() %>%
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
  addMarkers(data = kml.coordinates[kml.coordinates$Location.Type == "DC", ],
             lng = ~lon,
             lat = ~lat,
             icon = makeIcon("DC.png",20,20),
             popup = paste("DC"))

######################################################################################################
test1 <- temp
test1$lat.1 <- test1$lat
test1$lon.1 <- test1$lon
reppp <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}

test1$lat.1 <- reppp(test1$lat.1, 1)
test1$lon.1 <- reppp(test1$lon.1, 1)
test1[is.na(test1)] <- c(test1[1,2], test1[1,3])
test1$Location <- 1:nrow(test1)
test1$des <- 1:nrow(test1)
test1$des <- reppp(test1$des, 1)
test1[is.na(test1)] <- 1
test1$latlon.x <-paste(test1$lat, test1$lon, sep = "+")
test1$latlon.y <-paste(test1$lat.1, test1$lon.1, sep = "+")
test11 <- test1[, -c(2:5)]
dis_time_tour2 <- test11 %>%
  nest(c(latlon.x,latlon.y)) %>%
  mutate(models = lapply(data, 
                         function(df){gmapsdistance(origin = df$latlon.x,
                                                    destination = df$latlon.y,
                                                    mode = "driving",
                                                    combinations = "pairwise")})) %>%
  mutate( dfs = lapply(models, function(x){  bind_cols(x) })) %>%
  unnest(dfs)

dis_time_tour3 <- dis_time_tour2[,-c(4,5)]
dis_time_tour2$Time <- dis_time_tour2$Time/60
dis_time_tour2$tcumsum <- cumsum(dis_time_tour2$Time)
dis_time_tour2$Time2 <- substr(times((dis_time_tour2$tcumsum%/%60 +  dis_time_tour2$tcumsum%%60 /60)/24), 1, 5)
dis_time_tour2$Time2 <- as.POSIXct(dis_time_tour2$Time2,format="%H:%M")
# dis_time_tour2$Time2 <- as.POSIXct(strptime(dis_time_tour2$tcumsum, format = "%M"))
# dis_time_tour2$tcumsum <- cumsum(dis_time_tour2$Time2)
dis_time_tour2$end <- max(dis_time_tour2$Time2)
power3 <- cbind(temp, dis_time_tour2[,c("Time2", "end")])
colnames(power3)[4] <- "start"
colnames(power3)[2] <- "Latitude"
colnames(power3)[3] <- "Longitude"
power3$start <- as.date(power3$start)
power3$end <- as.time(power3$end)
######################### Old_method #################################################################
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
# 
# # ggmap(map, extent = "device") +
# #   geom_path(data = route, aes(x = lon, y = lat),  colour = "blue", size = 1, alpha = 0.5) +
# #   geom_point(data = kml.coordinates, aes(x = lon, y = lat), size = 3, alpha = 0.75) + 
# #   labs(x = "", y = "")
# 
# library(osrm)
# library(leaflet)  
# 
# trips <- osrmTrip(kml.coordinates, returnclass="sf")
# trip <- trips[[1]]$trip
# trip2 <- base::merge(trip, kml.coordinates[, c(1,4)], by.x = "start", by.y = "Location", all.x = T)
# trip2 <- base::merge(trip2, kml.coordinates[, c(1,4)], by.x = "end", by.y = "Location", all.x = T)
# trip2 <- as.data.frame(trip2[,c("start","end", "duration", "distance", "latlon.x", "latlon.y")])
# trip2$Time <- trip2$Time/3600
# 
# library(tidyverse)
# 
# trip2 <- trip2 %>%
#   nest(c(latlon.x,latlon.y)) %>%
#   mutate(models = lapply(data, 
#                          function(df){gmapsdistance(origin = df$latlon.x,
#                                                     destination = df$latlon.y,
#                                                     mode = "driving",
#                                                     combinations = "pairwise")})) %>%
#   mutate( dfs = lapply(models, function(x){  bind_cols(x) })) %>%
#   unnest(dfs)
# trip2$Time <- trip2$Time/3600
# trip2$Distance <- trip2$Distance/1000
# 
# 
# rout_map <- leaflet(trip) %>% 
#             addProviderTiles("OpenStreetMap", group = "OSM") %>% 
#             addPolylines() %>%
#             addCircleMarkers(lat = kml.coordinates$lat,
#                    lng = kml.coordinates$lon,
#                    popup = paste("Address: ", kml.coordinates$Address,"<br/>",
#                                  "Order No: ", kml.coordinates$Order.No., "<br/>",
#                                  "Quantity: ", kml.coordinates$Quantity..CBM., "<br/>"),
#                    color = "red",
#                    stroke = FALSE,
#                    radius = 8,
#                    fillOpacity = 0.8)
# rout_map
