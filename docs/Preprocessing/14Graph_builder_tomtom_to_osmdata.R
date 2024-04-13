library(dplyr) # to use %>%
library(MetricGraph)
library(mapview)
library(tidyr)

# loading the data to built the graph
load("Networks object/speed_limit_with_tomtom.RData")

weights = final_data_tomtom %>% 
  st_drop_geometry()



# building the graph
graph = graph_components$new(edges = final_data_tomtom$geometry, longlat = TRUE, edge_weights = weights)


# getting the largest connected component
sf_graph = graph$get_largest()

# sf_graph$prune_vertices()

#############################################################
# please do not forget to change the date in the file name ##
#############################################################

save(sf_graph, file = "Graph_objects/graph_construction_11_03_2024.RData")