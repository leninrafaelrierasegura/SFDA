library(MetricGraph)
library(dplyr)
library(sf)
library(mapview)

# loading the data
load("Graph_objects/graph_construction_21_03_2024alltomtomwhichlonglatsf.RData")
load("Data_files/data_day7142128_hour13_with_no_consecutive_zeros.RData")

# df = df %>% distinct(geometry, .keep_all = TRUE) #slice_sample(n = 1000, replace = FALSE) #st_transform(crs = st_crs(tomtom))

# process the data
sf_graph$add_observations(data = df, group = "day", tolerance = 0.04, duplicated_strategy = "jitter") # tolerance = 40m

# getting the data from the graph (that is, in graph coordinates)
data_on_graph = sf_graph$get_data()

save(data_on_graph, file = "Data_files/data_day7142128_hour13_with_no_consecutive_zeros_graphsubset_graph_processed.RData")

