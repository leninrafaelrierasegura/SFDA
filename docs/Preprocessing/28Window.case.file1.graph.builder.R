library(sf)
library(dplyr)
library(tidyr)
library(mapview)
library(MetricGraph)
library(here)

# load network from tomtom
load(here("Data_files/tomtom.RData"))
load(here("Data_files/data_day7142128_hour13_with_no_consecutive_zeros.RData")) # just to get the crs  

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

from.tomtom = tomtom %>%
  dplyr::select(-Id, -Segment.Id, -NewSegId, -timeSet, -dateRange, -standardDeviationSpeed, -travelTimeStandardDeviation) %>%
  filter(FRC != "7") %>% 
  mutate(value = SpeedLimit, road_type = paste("class_", FRC, sep = ""), aux = paste("class_", FRC, sep = "")) %>%
  pivot_wider(names_from = aux, values_from = value, values_fill = list(value = 0)) %>%
  mutate(upto1 = class_0 + class_1) %>%
  mutate(upto3 = upto1 + class_3) %>%
  mutate(upto4 = upto3 + class_4) %>%
  mutate(upto5 = upto4 + class_5) %>%
  mutate(upto6 = upto5 + class_6) %>%
  mutate(Length  = Length/1000) %>%  # in km
  mutate(density = sampleSize/Length) %>% # density whole day
  mutate(density_per_hour = density/24) %>% # density per hour
  st_transform(crs = st_crs(df)) %>%
  st_filter(x = ., y = polygon, .predicate = st_within)


# get the weights and edges
weights = from.tomtom %>% st_drop_geometry()
edges = from.tomtom$geometry

# building the graph
graph = graph_components$new(edges = edges, which_longlat = "sf", longlat = TRUE, edge_weights = weights)

# getting the largest connected component
sf_graph = graph$get_largest()

################################################################################
#                                   save the graph                             #
################################################################################

save(sf_graph, file = here("Graph_objects/graph_construction_30_04_2024partialtomtomwhichlonglatsf.RData"))


################################################################################
#                                 process the data                             #
################################################################################

data.reduced = st_filter(x = df, y = polygon, .predicate = st_within)

sf_graph$add_observations(data = data.reduced, group = "day", tolerance = 0.02, duplicated_strategy = "jitter") # tolerance = 20m

# getting the data from the graph (that is, in graph coordinates)
data_on_graph = sf_graph$get_data()

################################################################################
#                                   save the data                              #
################################################################################

save(data_on_graph, file = here("Data_files/data_day7142128_hour13_with_no_consecutive_zeros_partialtomtom_graph_30_04_2024_processed.RData"))




# For the graph built on 25_04_2024, the total number of points removed due do being far is 8396
# For the graph built on 30_04_2024, the total number of points removed due do being far is 8395
