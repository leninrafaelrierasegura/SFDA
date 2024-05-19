library(sf)
library(dplyr)
library(tidyr)
library(mapview)
library(MetricGraph)
library(here)

# load network from tomtom and data (the latter just to get the crs)
load(here("Data_files/tomtom.RData"))
load(here("Data_files/data_day7142128_hour13_with_no_consecutive_zeros.RData")) # just to get the crs


################################################################################
#              build polygon to cut the network and the data                   #
################################################################################

polygon = st_multipoint(c(st_point(c(-122.53000, 37.69702)),
                          st_point(c(-122.37000, 37.69702)),
                          st_point(c(-122.37000, 37.82600)),
                          st_point(c(-122.53000, 37.82600)))) %>%
  st_cast("POLYGON") %>%
  st_sfc(crs = st_crs(df))

################################################################################
#              some filtering to clean and prepare the data                    #
################################################################################

from.tomtom = tomtom %>%
  dplyr::select(-Id, -Segment.Id, -NewSegId, -timeSet, -dateRange, -standardDeviationSpeed, -travelTimeStandardDeviation) %>%
  filter(FRC != "7", FRC != "6", FRC != "5") %>% # to remove tomtom class 5, 6 and 7 
  mutate(value = SpeedLimit, road_type = paste("class_", FRC, sep = ""), aux = paste("class_", FRC, sep = "")) %>%
  pivot_wider(names_from = aux, values_from = value, values_fill = list(value = 0)) %>%
  mutate(upto1 = class_0 + class_1) %>%
  mutate(upto3 = upto1 + class_3) %>%
  mutate(upto4 = upto3 + class_4) %>%
  #mutate(upto5 = upto4 + class_5) %>%
  #mutate(upto6 = upto5 + class_6) %>%
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

save(sf_graph, file = here("Graph_objects/graph_construction_19MAY24_FRC0134.RData"))


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

save(data_on_graph, file = here("Data_files/data_day7142128_hour13_with_no_consecutive_zeros_19MAY24_FRC0134_graph_processed.RData"))
