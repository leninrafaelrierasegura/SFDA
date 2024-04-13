library(MetricGraph)
library(dplyr)
library(tidyr)
library(sf)
library(mapview)

load("Data_files/tomtom.RData")


weights_from_tomtom = tomtom %>%
  mutate(value = SpeedLimit, road_type = FRC) %>%
  pivot_wider(names_from = FRC, values_from = value, values_fill = list(value = 0)) %>% # one-hot for road types
  st_drop_geometry() # removing geometry column and attributes

# building the graph
graph = graph_components$new(edges = tomtom$geometry, longlat = TRUE, edge_weights = weights_from_tomtom)

# getting the largest connected component
sf_graph = graph$get_largest()

sf_graph$prune_vertices()

save(sf_graph, file = "Graph_objects/graph_using_TomTom_data.RData")