library(MetricGraph)
library(tidyr)
library(geosphere)

# loading the data
load("Data_files/data_on_graph_with_covariates_no_consecutive_zerosgraphsubset.RData")
loc = data.frame(lon = data_on_graph_with_covariates$.coord_x, 
                 lat = data_on_graph_with_covariates$.coord_y)

dist.mat = distm(loc, fun = distHaversine)
save(dist.mat, file = "~/Desktop/Spring 2024/dist.mat.RData")