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

h = 0.1

source("Covariates/6Creates_covariates_on_mesh.R")

creates_covariates_on_mesh(h)

Sys.sleep(60)

load("Data_files/data_on_mesh_with_covariates.RData")


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
  #sample_n(1000, replace = FALSE) %>%
  filter(speed > 0)


sf_graph$add_observations(data = data, group = "day", clear_obs = TRUE)
sf_graph$build_mesh(h = h)


# NON STATIONARY MODEL


mesh = data_on_mesh_with_covariates

B.sigma = cbind(0, 1, 0, 
                mesh$SpeedLimit, 0
                #scale(mesh$density_per_hour), 0
                #round(mesh$bus, 5), 0,
                #round(mesh$signal, 5), 0,
                #round(mesh$stop, 5), 0,
                #round(mesh$crossing, 5), 0
                ) 

B.range = cbind(0, 0, 1, 
                0, mesh$SpeedLimit
                #0, scale(mesh$density_per_hour)
                #0, round(mesh$bus, 5),
                #0, round(mesh$signal, 5),
                #0, round(mesh$stop, 5),
                #0, round(mesh$crossing, 5)
                ) 


# -----------------------------------------------------------------------------

# WM1

# ----------------------------------------------------------------------------

rspde_model_nonstatWM1 <- rspde.metric_graph(sf_graph,
                                          B.sigma = B.sigma,
                                          B.range = B.range,
                                          parameterization = "matern",
                                          nu = 0.5)

data_rspde_bru_nsWM1 <- graph_data_rspde(rspde_model_nonstatWM1, loc_name = "loc")

cmp_nonstatWM1 = speed ~ -1 +
  Intercept(1) +
  SpeedLimit + 
  #density_per_hour_normalized +
  #bus +
  #signal +
  #stop +
  #crossing +
  field(loc, model = rspde_model_nonstatWM1)

rspde_fit_nonstat_WM1 <-
  bru(cmp_nonstatWM1,
      data = data_rspde_bru_nsWM1[["data"]],
      family = "gaussian",
      options = list(verbose = FALSE)
  )

summary(rspde_fit_nonstat_WM1)
summary(rspde.result(rspde_fit_nonstat_WM1, "field", rspde_model_nonstatWM1))

print("run WM1")

# -----------------------------------------------------------------------------

# WM2

# -----------------------------------------------------------------------------

rspde_model_nonstatWM2 <- rspde.metric_graph(sf_graph,
                                          B.sigma = B.sigma,
                                          B.range = B.range,
                                          parameterization = "matern",
                                          nu = 1.5)

data_rspde_bru_nsWM2 <- graph_data_rspde(rspde_model_nonstatWM2, loc_name = "loc")

cmp_nonstatWM2 = speed ~ -1 +
  Intercept(1) +
  SpeedLimit + 
  #density_per_hour_normalized +
  #bus +
  #signal +
  #stop +
  #crossing +
  field(loc, model = rspde_model_nonstatWM2)

rspde_fit_nonstat_WM2 <-
  bru(cmp_nonstatWM2,
      data = data_rspde_bru_nsWM2[["data"]],
      family = "gaussian",
      options = list(verbose = FALSE)
  )

summary(rspde_fit_nonstat_WM2)
summary(rspde.result(rspde_fit_nonstat_WM2, "field", rspde_model_nonstatWM2))


print("run WM2")
# -----------------------------------------------------------------------------

# WM

# -----------------------------------------------------------------------------

rspde_model_nonstatWM <- rspde.metric_graph(sf_graph,
                                          B.sigma = B.sigma,
                                          B.range = B.range,
                                          parameterization = "matern")

data_rspde_bru_nsWM <- graph_data_rspde(rspde_model_nonstatWM, loc_name = "loc")

cmp_nonstatWM = speed ~ -1 +
  Intercept(1) +
  SpeedLimit + 
  #density_per_hour_normalized +
  #bus +
  #signal +
  #stop +
  #crossing +
  field(loc, model = rspde_model_nonstatWM)

rspde_fit_nonstat_WM <-
  bru(cmp_nonstatWM,
      data = data_rspde_bru_nsWM[["data"]],
      family = "gaussian",
      options = list(verbose = FALSE)
  )

summary(rspde_fit_nonstat_WM)
summary(rspde.result(rspde_fit_nonstat_WM, "field", rspde_model_nonstatWM))



# cross validation

models <- list(WM = rspde_fit_nonstat_WM, WM1 = rspde_fit_nonstat_WM1, WM2 = rspde_fit_nonstat_WM2)

cv_result <- cross_validation(models, cv_type = "k-fold", print = FALSE)
cv_result
# 

save.image("nonstationary_gaussian0.2.RData")


