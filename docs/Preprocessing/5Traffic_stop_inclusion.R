library(mapview)
library(dplyr)
library(plotly)
library(osmdata)
library(sf)

bb = c(-122.53000, 37.69702, -122.37000 ,  37.82600) #c(-122.51362, 37.69702, -122.35779 ,  37.81130)
data = opq(bbox = bb) %>%
  add_osm_feature(key="highway", value=c("traffic_signals", "bus_stop", "stop", "crossing")) %>%
  osmdata_sf()

datapoints<- data$osm_points%>%
             filter(highway %in% c("traffic_signals", "bus_stop", "stop", "crossing")) %>%
              select(geometry, highway)

save(datapoints, file = "Networks object/traffic_stopnew.RData")
