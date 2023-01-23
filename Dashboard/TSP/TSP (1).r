setwd("~/KPMG/Dashboard/TSP")
library(sp)
library(maptools)
library(dplyr)
library(ggplot2)
library(ggmap)
library(gmapsdistance)
library(TSP)
library(purrr)

load("TSP_data2.RData")

KML.text <- readLines("TSP Data.kml")
re <- " *([^<]+?) *<\\/coordinates>"
coords <- grep(re,KML.text)
coords <- coords-1
re3 <- "*([^<]+?) *<\\/name>" 
Name <- grep(re3, KML.text) 
Name <- Name[Name > 47]
kml.coordinates <- matrix(0,length(coords),4,dimnames=list(c(),c("Location","lon","lat","ELEV")))
x <- "name"

for(i in 1:length(coords)){  
  sub.coords <- coords[i]  
  temp1 <- gsub(""," ",KML.text[sub.coords])  
  #temp2 <- gsub(""," ",temp1)  
  coordinates <-as.numeric( gsub("\\s", "", unlist(strsplit(temp1,","))))
  
  sub.Name <- Name[i]  
  NAME <- gsub("","",KML.text[sub.Name])  
  NAME <- as.character(gsub("", "", unlist(strsplit(NAME,"[>/<]"))))
  NAME <- gsub("\\s", "", NAME)
  NAME <- NAME[nchar(NAME) > 0]
  NAME <- NAME[!NAME %in% x]
  
  kml.coordinates[i,] <- matrix(c(NAME,coordinates),ncol=4)  
}  

kml.coordinates <- as.data.frame(kml.coordinates)
kml.coordinates <- kml.coordinates[, -4]
kml.coordinates$latlon <- paste0(kml.coordinates$lat,"+",kml.coordinates$lon)

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
  solve_TSP(tsp, method, control = list(start=1L))
})
tour <- solve_TSP(tsp, control = list(start=1))
tour_order <- as.integer(tour)
kml.coordinates <- kml.coordinates[tour_order,]
route <- lapply(seq(nrow(kml.coordinates) - 1), function(n) {
  print(n)
  route(kml.coordinates$latlon[n], kml.coordinates$latlon[n+1], structure = "route") %>%
    mutate(section = n)
})
route <- route %>% bind_rows()
map <- get_map(location = c(lon = 79.863967, lat = 6.875159), zoom = 14, maptype = "roadmap")

kml.coordinates$lon <- as.numeric(as.character(kml.coordinates$lon))
kml.coordinates$lat <- as.numeric(as.character(kml.coordinates$lat))


# ggmap(map, extent = "device") +
#   geom_path(data = route, aes(x = lon, y = lat),  colour = "blue", size = 1, alpha = 0.5) +
#   geom_point(data = kml.coordinates, aes(x = lon, y = lat), size = 3, alpha = 0.75) + 
#   labs(x = "", y = "")

library(osrm)
library(leaflet)  

trips <- osrmTrip(kml.coordinates, returnclass="sf")
trip <- trips[[1]]$trip

rout_map <- leaflet(trip) %>% 
            addProviderTiles("OpenStreetMap", group = "OSM") %>% 
            addPolylines() %>%
            addCircleMarkers(lat = kml.coordinates$lat,
                   lng = kml.coordinates$lon,
                   popup = kml.coordinates$Location,
                   color = "red",
                   stroke = FALSE,
                   radius = 8,
                   fillOpacity = 0.8)
