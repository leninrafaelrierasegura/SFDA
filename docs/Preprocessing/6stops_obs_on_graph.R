library(MetricGraph)
library(dplyr)
library(tidyr)
library(sf)
library(plotly)

load("Graph_objects/graph_construction_21_03_2024alltomtomwhichlonglatsf.RData")
load("Networks object/traffic_stop.RData")


stops_obs <- sf_graph$add_observations(data = datapoints, tolerance = 0.007, clear_obs = TRUE) # tolerance 7m

traffic_stops = sf_graph$get_data()
save(traffic_stops, file="Data_files/stops_data_on_graphsubset.RData")