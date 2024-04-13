library(MetricGraph)
library(dplyr)
library(tidyr)
library(sf)
library(plotly)

load("Graph_objects/graph_construction_28_03_2024partialtomtomwhichlonglatsf.RData")
load("Networks object/traffic_stop.RData")
load("Data_files/data_day7142128_hour13_with_no_consecutive_zeros.RData") # just to get the crs  

################################################################################
#              build polygon to cut the network and the data                   #
################################################################################

# to define a polygon to cut the network and the data
polygon = st_multipoint(c(st_point(c(-122.42591, 37.80732)),
                          st_point(c(-122.40648, 37.81002)),
                          st_point(c(-122.38820, 37.78959)),
                          st_point(c(-122.38770, 37.78849)),
                          st_point(c(-122.38483, 37.76713)),
                          st_point(c(-122.39342, 37.76665)),
                          st_point(c(-122.39372, 37.76655)),
                          st_point(c(-122.41970, 37.76500)),
                          st_point(c(-122.42039, 37.77159)),
                          st_point(c(-122.42050, 37.77174)),
                          st_point(c(-122.42040, 37.77194)),
                          st_point(c(-122.41905, 37.77306)))) %>%
  st_cast("POLYGON") %>%
  st_sfc(crs = st_crs(df))

datapoints.reduced = datapoints %>% st_transform(crs = st_crs(df)) %>% st_filter(x = ., y = polygon, .predicate = st_within)


stops_obs <- sf_graph$add_observations(data = datapoints.reduced, tolerance = 0.003, clear_obs = TRUE) # tolerance 7m

traffic_stops = sf_graph$get_data()

save(traffic_stops, file="Data_files/stops_data_on_graph_partialtomtom.RData")
