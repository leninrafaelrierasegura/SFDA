library(MetricGraph)
library(rSPDE)
library(plotly)
library(dplyr)
library(MASS)
library(glmnet)
library(car)
library(inlabru)
library(INLA)
library(tidyr)

# loading the data
load("Data_files/data_on_graph_with_covariates.RData")
load("Graph_objects/graph_construction_11_03_2024.RData")

set.seed(2024)

data = data_on_graph_with_covariates %>% 
  dplyr::select(speed, 
                day,
                SpeedLimit,
                density_per_hour_normalized,
                bus,
                signal,
                stop,
                crossing) %>%
  sample_n(10000, replace = FALSE) %>%
  filter(speed > 0)


sf_graph$add_observations(data = data, group = "day", clear_obs = TRUE)
sf_graph$build_mesh(h = 0.2)




# STATIONARY MODEL


# ------------------------------------------------------------------------------

# WM1

# ------------------------------------------------------------------------------

rspde_model_statWM1 <- rspde.metric_graph(sf_graph, parameterization = "matern", nu = 0.5)

data_rspde_bru_s_WM1 <- graph_data_rspde(rspde_model_statWM1, repl = ".all", loc_name = "loc")

cmp_statWM1 = speed ~ -1 +
  Intercept(1) +
  SpeedLimit + 
  density_per_hour_normalized +
  bus +
  signal +
  stop +
  crossing +
  field(loc, model = rspde_model_statWM1, replicate = day)

rspde_fit_stat_WM1 <-
  bru(cmp_statWM1,
      data = data_rspde_bru_s_WM1[["data"]],
      family = "gaussian",
      options = list(verbose = FALSE)
  )

summary(rspde_fit_stat_WM1)
summary(rspde.result(rspde_fit_stat_WM1, "field", rspde_model_statWM1))


# ------------------------------------------------------------------------------

# WM2

# ------------------------------------------------------------------------------

rspde_model_statWM2 <- rspde.metric_graph(sf_graph, parameterization = "matern", nu = 1.5)

data_rspde_bru_s_WM2 <- graph_data_rspde(rspde_model_statWM2, repl = ".all", loc_name = "loc")

cmp_statWM2 = speed ~ -1 +
  Intercept(1) +
  SpeedLimit + 
  density_per_hour_normalized +
  bus +
  signal +
  stop +
  crossing +
  field(loc, model = rspde_model_statWM2, replicate = day)

rspde_fit_stat_WM2 <-
  bru(cmp_statWM2,
      data = data_rspde_bru_s_WM2[["data"]],
      family = "gaussian",
      options = list(verbose = FALSE)
  )

summary(rspde_fit_stat_WM2)
summary(rspde.result(rspde_fit_stat_WM2, "field", rspde_model_statWM2))


# -----------------------------------------------------------------------------------

# WM

# -----------------------------------------------------------------------------------

rspde_model_statWM <- rspde.metric_graph(sf_graph, parameterization = "matern")

data_rspde_bru_s_WM <- graph_data_rspde(rspde_model_statWM, repl = ".all", loc_name = "loc")

cmp_statWM = speed ~ -1 +
  Intercept(1) +
  SpeedLimit + 
  density_per_hour_normalized +
  bus +
  signal +
  stop +
  crossing +
  field(loc, model = rspde_model_statWM, replicate = day)

rspde_fit_stat_WM <-
  bru(cmp_statWM,
      data = data_rspde_bru_s_WM[["data"]],
      family = "gaussian",
      options = list(verbose = FALSE)
  )

summary(rspde_fit_stat_WM)
summary(rspde.result(rspde_fit_stat_WM, "field", rspde_model_statWM))


# cross validation ----------------------------------------------------------------------

models <- list(WM = rspde_fit_stat_WM, WM1 = rspde_fit_stat_WM1, WM2 = rspde_fit_stat_WM2)

cv_result <- cross_validation(models, cv_type = "k-fold", print = FALSE)
cv_result

save.image("stationary_with_replicates_gaussian.RData")



