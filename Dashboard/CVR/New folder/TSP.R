setwd("~/KPMG/Dashboard/CVR")
library(TSP)
library(leaflet)
library(plotly)
library(ggmap)
library(gmapsdistance)
library(geosphere)
library(dplyr)


data2 <- read.csv("Cities.csv")
map <- leaflet() %>% addTiles()
my_map <- map %>% addMarkers(lat = data2$lat, lng = data2$long)
my_map

# test <- gmapsdistance(origin = "79.86124+6.927079",
#                       destination = "81.10107+6.124593",
#                       mode = "walking")

dist_mat <- dist(data2)
atsp <- ATSP(dist_mat)
atsp
## use some methods
n_of_cities(atsp)
labels(atsp)
## calculate a tour
tour <- solve_TSP(atsp, method = "nn")
tour
tour_length(tour)
labels(tour)
image(atsp, tour)
