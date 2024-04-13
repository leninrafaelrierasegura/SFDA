library(mapview)
library(dplyr) # to use %>%
library(tidyr) # to use pivot_wider
library(MetricGraph)
# loading the data to built the graph
load("Networks object/speed_limit_with_close_distance.RData")

# check the weights are correct
mapview(final_data, zcol = "maxspeed")

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

# getting the largest connected component
sf_graph = graph$get_largest()

# plot to check that the weights are correct
sf_graph$plot(vertex_size=0, edge_weight = "motorway", edge_width_weight = "motorway", edge_width = 1)

# so far I have built the graph with weights  ------------------------------------------------------------------------------


# let us add the data ------------------------------------------------------------------------------------------------------

load("onlybuses.RData") # speed data corresponding to only the buses

obs_final = busdataset %>% 
  select(Average.Speed, day) %>% 
  filter(Average.Speed < 75, day == 7) %>% 
  dplyr::select(-day) %>% 
  distinct(geometry, .keep_all = TRUE)

sf_graph$prune_vertices()


sf_graph$add_observations(data = obs_final, tolerance = 0.2, duplicated_strategy = "jitter")

#save(sf_graph, file = "final_graph_with_1day_data_filter_less_75.RData")
load("Graph_objects/final_graph_wholeday_speed75_setlimit_with_dist.RData")

# so far I have added the data ----------------------------------------------------------------------------------------------

# let us get the weights from the added data using Alex's code --------------------------------------------------------------

# getting the data from sf_graph
df_graph <- sf_graph$get_data()
df_graph <- df_graph %>% group_by(.edge_number) %>% mutate(quantile = quantile(Average.Speed, 0.95)) %>% ungroup()

map_to_limit <- function(quant_value){
  out <- numeric(length(quant_value))
  speed_limits <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65)
  for(i in 1:length(quant_value)){
    if(quant_value[i] > 65){
      out[i] = 65
    } else {
    diff_tmp <- speed_limits - quant_value[i]
    diff_tmp[diff_tmp<0] <- 100
    idx <- which.min(diff_tmp)
    out[i] <- speed_limits[idx]
    }
  }
  return(out)
}

df_graph <- df_graph %>% mutate(speed_lim = map_to_limit(quantile))

speedlim_vec <- df_graph %>% group_by(.edge_number) %>% dplyr::summarise(max(speed_lim)) %>% arrange(.edge_number)
speedlim_vec <- speedlim_vec %>% ungroup()


weights <- sf_graph$get_edge_weights() 
weights$speedlimit[speedlim_vec[".edge_number"][[1]]] <- speedlim_vec[,2][[1]]
sf_graph$set_edge_weights(weights = weights)


#save(sf_graph, file = "final_graph_with_1day_data_filter_less_75_weights_fixed.RData")
save(sf_graph, file = "Graph_objects/final_graph_wholeday_speed75_setlimits_with_quantile2.RData")

# checking


sf_graph$edgeweight_to_data(data_loc = TRUE)
plot(sf_graph$get_data()$speedlimit,sf_graph$get_data()$Average.Speed)
abline(a = 0, b = 1)
