library(MetricGraph)
library(dplyr)
library(tidyr)
library(sf)
library(mapview)

load("Data_files/tomtom.RData")


from_tomtom = tomtom %>% 
  dplyr::select(Length, SpeedLimit, FRC, sampleSize) %>%
  filter(FRC != "7") %>%
  mutate(value = SpeedLimit, road_type = FRC) %>%
  pivot_wider(names_from = FRC, values_from = value, values_fill = list(value = 0)) 

weights_from_tomtom = from_tomtom %>% st_drop_geometry()


# building the graph
graph = graph_components$new(edges = from_tomtom$geometry, 
                             longlat = TRUE, 
                             edge_weights = weights_from_tomtom, 
                             verbose = 2, 
                             which_longlat = "sp", 
                             tolerance = list(vertex_vertex = 0,
                                              vertex_edge = 0, 
                                              edge_edge = 0))

# getting the largest connected component
sf_graph = graph$get_largest()

sf_graph$prune_vertices()

save(sf_graph, file = "Graph_objects/graph_rm7_TomTom_data.RData")

