library(MetricGraph)
library(dplyr)


# load the data to be process with the graph
load("Data_files/data_day7142128_hour13_with_no_consecutive_zeros.RData")

# load the graph to process the data
load("Graph_objects/graph_construction_11_03_2024.RData")

# process the data
sf_graph$add_observations(data = df, group = "day", tolerance = 0.04, duplicated_strategy = "jitter") # tolerance = 40m

# getting the data from the graph (that is, in graph coordinates)
data_on_graph = sf_graph$get_data()

# save the processed data
save(data_on_graph, file = "Data_files/data_day7142128_hour13_with_no_consecutive_zeros_graph_processed.RData")