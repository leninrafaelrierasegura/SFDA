library(MetricGraph)
library(plotly)
library(dplyr)
library(tidyr)


# helper functions
standardize <- function(vector) {return((vector - mean(vector)) / sd(vector))}
convert_to_binary <- function(input_vector) {return(ifelse(input_vector != 0, 1, 0))}


h = 0.05

source("Preprocessing/28Window.case.file4.addcovariatesonmesh.R")

creates_covariates_on_mesh(h)

Sys.sleep(60)

load("Data_files/data_on_graph_with_covariates_no_consecutive_zeros_partialtomtom.RData")
load("Graph_objects/graph_construction_28_03_2024partialtomtomwhichlonglatsf.RData")
load("Data_files/data_on_mesh_with_covariates_partialtomtom.RData")


data = data_on_graph_with_covariates %>% 
  mutate(across(starts_with(c("class_", "upto")), list(ind = convert_to_binary))) %>%
  mutate(across(c("bus", "signal", "stop", "crossing"), ~round(., 5))) %>%
  mutate(across(c("density_per_hour"), standardize)) # or ~standardize(.)


mesh = data_on_mesh_with_covariates %>% 
  mutate(across(starts_with(c("class_", "upto")), list(ind = convert_to_binary))) %>% # this creates new columns
  mutate(across(c("bus", "signal", "stop", "crossing"), ~round(., 5))) %>%
  mutate(across(c("SpeedLimit", "density_per_hour"), standardize)) # or ~standardize(.)


sf_graph$build_mesh(h = h)

sf_graph$add_observations(data = data, group = "day", clear_obs = TRUE)



sf_graph$plot(data = "SpeedLimit", newdata = mesh, vertex_size = 0, edge_weight = "SpeedLimit", add_new_scale_weights = TRUE)

sf_graph$plot(data = "SpeedLimit", group = 1:4, 
              vertex_size = 0, 
              edge_weight = "SpeedLimit",
              edge_color = "SpeedLimit", 
              edge_width_weight = "SpeedLimit",
              edge_width = 1,
              add_new_scale_weights = FALSE)


mapview::mapview(tomtom %>% filter(FRC <=6), zcol = "SpeedLimit")