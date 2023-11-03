rm(list = ls())
load("../data/GPDDFiltered.RData")
library(maps)
#create the world map
world_map <- map("world", plot = TRUE, fill = TRUE)

#get latitudes and longitudes from our data
latitudes <- gpdd$lat
longitudes <- gpdd$long

#map this
points(longitudes, latitudes, pch = 20 ,col = 'red', cex = 0.6)
