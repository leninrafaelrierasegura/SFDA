library(dplyr) # to use %>%
library(MetricGraph)
# loading the data to built the graph
load("Networks object/speed_limit_with_close_dist.RData")

with_categories = final_data %>%
  mutate(value = maxspeed, road_type = highway) %>%
  pivot_wider(names_from = highway, values_from = value, values_fill = list(value = 0))


max.speed.data.frame = data.frame(speedlimit = with_categories$maxspeed,
                                  road_type = with_categories$road_type,
                                  motorway = with_categories$motorway,
                                  residential = with_categories$residential,
                                  motorway_link = with_categories$motorway_link,
                                  primary = with_categories$primary,
                                  secondary = with_categories$secondary,
                                  trunk = with_categories$trunk,
                                  tertiary = with_categories$tertiary,
                                  unclassified = with_categories$unclassified)

# building the graph
graph = graph_components$new(edges = final_data$geometry, longlat = TRUE, edge_weights = max.speed.data.frame)

#loading the the weights obtained from quantile
load("Data_files/weights_quantile_corrected.RData")

# getting the largest connected component
sf_graph = graph$get_largest()

sf_graph$prune_vertices()
# adding the weights
sf_graph$set_edge_weights(weights = weights_quantile_corrected)

#############################################################
# please do not forget to change the date in the file name ##
#############################################################

save(sf_graph, file = "Graph_objects/graph_construction_29_02_2024.RData")
