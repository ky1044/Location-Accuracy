library(jsonlite)
library(ggplot2)
library(tidyverse)
library(ggmap)

#downloading map data from Google API, using personal key
ggmap::register_google(key = Sys.getenv("GOOGLE_API_KEY") )
map <- get_map("5th ave and 25th street", zoom = 13, source = 'stamen', maptype = "toner-lite")
#load location history from JSON
data <- fromJSON(readLines("Location History.json"))
loc = data$locations

#subset data for new york data points
nyloc <- subset(loc,loc$longitudeE7<(-739000000)&loc$longitudeE7>(-740500000)  & loc$accuracy<300, select = c("longitudeE7", "latitudeE7", "accuracy") )
#convert latitudeE7 and longitudeE7 to readable coordinates
nyloc$lat = nyloc$latitudeE7 / 1e7
nyloc$lon = nyloc$longitudeE7/ 1e7
#log accuracy so that high outliers don't mess up color scale
nyloc$logAccuracy = log(nyloc$accuracy,base=exp(2))

hist(as.numeric(loc$timestampMs))

ggmap(map) +
  geom_point(data=nyloc,aes(x = lon, y = lat, color = logAccuracy)) + scale_colour_gradient(low = "white", high = "blue")+
  labs( x = "Longitude", y = "Latitude", title = "Location Accuracy (lower value is more accurate)")

#ggsave("My Location Accuracy in Midtown.jpeg")


nyloc$distance <- function(i) {
  mapdist(as.numeric(a[i, c('nyloc.Lon','nyloc.lat')]), 
          as.numeric(a[i, c('-73.99','40.73')]), 
          mode='driving')$km
}

nyloc$dist_km <- mapdist(from = paste0(as.character(nyloc$lat),",",as.character(nyloc$lon)),
                     to = paste0(as.character('-73.99'),", ",as.character('40.73')))[,c('km')]



