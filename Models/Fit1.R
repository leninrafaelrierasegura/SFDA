library(MetricGraph)
library(rSPDE)
library(plotly)
library(dplyr)
library(MASS)
library(glmnet)
library(car)
library(inlabru)
library(INLA)

# loading the data
load("Data_files/data_on_graph_with_bus_signals_stop_density_and_numbers_covariates.RData")
load("Graph_objects/graph_construction_03_03_2024.RData")
load("Data_files/stops_data_on_graph.RData")

data = data_on_graph_with_bus_signals_stop_density_covariates %>% 
  mutate(density_per_hour = density_per_hour/max(density_per_hour)) %>%
  dplyr::select(speed, 
                cov_bus_no_normalized, 
                cov_signals_no_normalized,
                cov_stop_no_normalized,
                cov_crossing_no_normalized,
                SpeedLimit,
                highway,
                density_per_hour,
                day) %>%
  sample_n(10000, replace = FALSE) %>%
  filter(speed >= 0)


sf_graph$add_observations(data = data, group = "day", clear_obs = TRUE)
sf_graph$build_mesh(h = 0.2)

# STATIONARY MODEL

rspde_model_stat <- rspde.metric_graph(sf_graph, parameterization = "matern")

data_rspde_bru_s <- graph_data_rspde(rspde_model_stat, loc_name = "loc")

cmp_stat <- speed ~ -1 + 
  Intercept(1) + 
  cov_bus_no_normalized +  
  cov_signals_no_normalized + 
  cov_stop_no_normalized + 
  cov_crossing_no_normalized +
  SpeedLimit + 
  #highway + 
  density_per_hour +
  field(loc, model = rspde_model_stat)

rspde_fit_stat <-
  bru(cmp_stat,
      data = data_rspde_bru_s[["data"]],
      family = "gaussian",
      options = list(verbose = FALSE)
  )

summary(rspde_fit_stat)
summary(rspde.result(rspde_fit_stat, "field", rspde_model_stat))



