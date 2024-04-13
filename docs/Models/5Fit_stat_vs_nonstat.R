# Open a connection to a text file
sink("~/Desktop/Spring 2024/Model_runs/stat_vs_nonstat_gaussian_all_data_h0.2_no_consecutuve_zeros_nu.upper.bound1.5.with.initial.val.from.stat2rerun2.txt")

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
load("Data_files/data_on_graph_with_covariates_no_consecutive_zeros.RData")
load("Graph_objects/graph_construction_11_03_2024.RData")

h = 0.2

source("Covariates/6Creates_covariates_on_mesh.R")

creates_covariates_on_mesh(h)

Sys.sleep(60)

load("Data_files/data_on_mesh_with_covariates.RData")

start_time <- Sys.time()
#set.seed(2024)

data = data_on_graph_with_covariates %>% 
  dplyr::select(speed, 
                day,
                SpeedLimit,
                density_per_hour_normalized,
                bus,
                signal,
                stop,
                crossing) %>%
  mutate(bus = round(bus, 5), signal = round(signal, 5), stop = round(stop, 5), crossing = round(crossing, 5)) #%>%
  #slice_sample(n = 100000, replace = FALSE)


sf_graph$add_observations(data = data, group = "day", clear_obs = TRUE)
sf_graph$build_mesh(h = h)


mesh = data_on_mesh_with_covariates %>% mutate(bus = round(bus, 5), signal = round(signal, 5), stop = round(stop, 5), crossing = round(crossing, 5))



################################################################################
################################# STATIONARY MODEL #############################
################################################################################


rspde_model_statWM <- rspde.metric_graph(sf_graph, 
                                         parameterization = "matern", nu.upper.bound = 1.5)

data_rspde_bru_s_WM <- graph_data_rspde(rspde_model_statWM, loc_name = "loc")

cmp_statWM = speed ~ -1 +
  Intercept(1) +
  #SpeedLimit + 
  #density_per_hour_normalized +
  #bus +
  #signal +
  #stop +
  #crossing +
  field(loc, model = rspde_model_statWM)

rspde_fit_stat_WM <-
  bru(cmp_statWM,
      data = data_rspde_bru_s_WM[["data"]],
      family = "gaussian",
      options = list(verbose = FALSE)
  )

summary(rspde_fit_stat_WM)

fit.rspde = rspde.result(rspde_fit_stat_WM, "field", rspde_model_statWM)
summary(fit.rspde)



################################################################################
############################# NON STATIONARY MODEL #############################
################################################################################

B.sigma = cbind(0, 1, 0, 
                mesh$SpeedLimit, 0
                #scale(mesh$density_per_hour), 0
                #mesh$bus, 0
                #mesh$signal, 0,
                #mesh$stop, 0,
                #mesh$crossing, 0
) 

B.range = cbind(0, 0, 1, 
                0, mesh$SpeedLimit
                #0, scale(mesh$density_per_hour)
                #0, mesh$bus
                #0, mesh$signal,
                #0, mesh$stop,
                #0, mesh$crossing
) 

# rspde_model_nonstatWM <- rspde.metric_graph(sf_graph,
#                                             start.theta = c(fit.rspde$summary.log.tau$mode, fit.rspde$summary.log.kappa$mode, 0, 0),
#                                             B.tau = B.sigma,
#                                             B.kappa = B.range,
#                                             parameterization = "spde", nu.upper.bound = 1.5)

rspde_model_nonstatWM <- rspde.metric_graph(sf_graph,
                                            start.theta = c(fit.rspde$summary.log.std.dev$mode, fit.rspde$summary.log.range$mode, 0, 0),
                                            B.sigma = B.sigma,
                                            B.range = B.range,
                                            parameterization = "matern", nu.upper.bound = 1.5)

data_rspde_bru_nsWM <- graph_data_rspde(rspde_model_nonstatWM, loc_name = "loc")

cmp_nonstatWM = speed ~ -1 +
  Intercept(1) +
  #SpeedLimit + 
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


################################################################################
################################ RERUN #########################################
################################################################################

for (n in 1:3) {
  if(rspde_fit_nonstat_WM$mlik[2,1] < rspde_fit_stat_WM$mlik[2,1]){
    rspde_fit_nonstat_WM = bru_rerun(rspde_fit_nonstat_WM)
    summary(rspde_fit_nonstat_WM)
    summary(rspde.result(rspde_fit_nonstat_WM, "field", rspde_model_nonstatWM))
  }
}


################################################################################
############################## PREDICTION ######################################
################################################################################

#data_locations = sf_graph$get_mesh_locations(bru = TRUE, loc = "loc")

################################################################################
################################ CROSSVALIDATION ###############################
################################################################################

#inla.group.cv

models <- list(nonstatWM = rspde_fit_nonstat_WM, statWM = rspde_fit_stat_WM)

cv_result <- cross_validation(models, cv_type = "k-fold", print = FALSE)
cv_result

# 
end_time <- Sys.time()
execution_time <- end_time - start_time



save.image("~/Desktop/Spring 2024/Model_runs/stat_vs_nonstat_gaussian_all_data_h0.2_no_consecutive_zeros_nu.upper.bound1.5.with.initial.val.from.stat2rerun2.RData")


# Close the connection
sink()
