library(sf)
library(dplyr)
library(tidyr)
library(mapview)
library(MetricGraph)

# load network from tomtom
load("Data_files/tomtom.RData")

load("Data_files/data_day7142128_hour13_with_no_consecutive_zeros.RData") # just to get the crs
#
from.tomtom = tomtom %>%
  dplyr::select(-Id, -Segment.Id, -NewSegId, -timeSet, -dateRange, -standardDeviationSpeed, -travelTimeStandardDeviation) %>%
  filter(FRC != "7", FRC != "6") %>% # to remove tomtom class 6 and 7 
  mutate(value = SpeedLimit, road_type = paste("class_", FRC, sep = ""), aux = paste("class_", FRC, sep = "")) %>%
  pivot_wider(names_from = aux, values_from = value, values_fill = list(value = 0)) %>%
  mutate(upto1 = class_0 + class_1) %>%
  mutate(upto3 = upto1 + class_3) %>%
  mutate(upto4 = upto3 + class_4) %>%
  mutate(upto5 = upto4 + class_5) %>%
  #mutate(upto6 = upto5 + class_6) %>%
  mutate(Length  = Length/1000) %>%  # in km
  mutate(density = sampleSize/Length) %>% # density whole day
  mutate(density_per_hour = density/24) %>% # density per hour
  #mutate(density_per_hour_normalized = density_per_hour/max(density_per_hour)) # normalized density
  st_transform(crs = st_crs(df))

# weights.initial = from.tomtom %>% st_drop_geometry()
# 
# 
# p1 = st_point(c(-122.53000, 37.69702))
# p2 = st_point(c(-122.37000, 37.69702))
# p3 = st_point(c(-122.37000, 37.82600))
# p4 = st_point(c(-122.53000, 37.82600))
# 
# poly = st_multipoint(c(p1, p2, p3, p4)) %>%
#   st_cast("POLYGON") %>%
#   st_sfc(crs = st_crs(from.tomtom))
# 
# tomtom.subset = from.tomtom %>% st_intersection(poly) %>% st_cast("LINESTRING")
# 
# which(st_geometry_type(tomtom.subset) == "MULTILINESTRING")



# get the weights and edges
weights = from.tomtom %>% st_drop_geometry()
edges = from.tomtom$geometry

# building the graph
graph = graph_components$new(edges = edges, which_longlat = "sf", longlat = TRUE, edge_weights = weights)


# getting the largest connected component
sf_graph = graph$get_largest()

#############################################################
# please do not forget to change the date in the file name ##
#############################################################

save(sf_graph, file = "Graph_objects/graph_construction_21_03_2024alltomtomwhichlonglatsf.RData")
