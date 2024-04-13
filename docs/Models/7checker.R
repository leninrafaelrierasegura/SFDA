#### CODE

# Open a connection to a text file
sink("~/Desktop/Spring 2024/Model_runs/stat_vs_nonstat_gaussian_all_data_h0.2_no_consecutuve_zeros_nu.upper.bound1.5.with.initial.val.from.stat2.check30.txt")

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

# helper functions
standardize <- function(vector) {return((vector - mean(vector)) / sd(vector))}
minmax_scaling <- function(x) {return((x - min(x)) / (max(x) - min(x)))}
convert_to_binary <- function(input_vector) {return(ifelse(input_vector != 0, 1, 0))}


h = 0.05

source("Covariates/6Creates_covariates_on_mesh.R")

creates_covariates_on_mesh(h)

Sys.sleep(60)

################################################################################
################################# PREPARE THE DATA #############################
################################################################################


# loading the data
load("Data_files/data_on_graph_with_covariates_no_consecutive_zerosgraphsubset.RData")
load("Graph_objects/graph_construction_21_03_2024alltomtomwhichlonglatsf.RData")
load("Data_files/data_on_mesh_with_covariates_graphsubset.RData")


data = data_on_graph_with_covariates %>% 
  mutate(across(starts_with(c("class_", "upto")), list(ind = convert_to_binary))) %>%
  mutate(across(c("bus", "signal", "stop", "crossing"), ~round(., 5))) %>%
  mutate(across(c("density_per_hour"), standardize)) # or ~standardize(.)


mesh = data_on_mesh_with_covariates %>% 
  mutate(across(starts_with(c("class_", "upto")), list(ind = convert_to_binary))) %>% # this creates new columns
  mutate(across(c("bus", "signal", "stop", "crossing"), ~round(., 5))) %>%
  mutate(across(c("SpeedLimit", "density_per_hour"), standardize)) # or ~standardize(.)


# prediction.data <- list()
# AAA = as.matrix(data.frame(data$.edge_number, data$.distance_on_edge))
# colnames(AAA) = c("", "d.e")
# prediction.data[["loc"]] = AAA
# covariate.names = c("SpeedLimit", 
#                     "density_per_hour", 
#                     "bus", 
#                     "signal", 
#                     "stop", 
#                     "crossing", 
#                     "upto1_ind",
#                     "bus_number"
#                     #"signal_number",
#                     #"stop_number",
#                     #"crossing_number"
#                     )
# for (i in 1:length(covariate.names)) {
#   prediction.data[[covariate.names[i]]] = data[[covariate.names[i]]]
# }

start_time <- Sys.time()
sf_graph$add_observations(data = data, group = "day", clear_obs = TRUE)
sf_graph$build_mesh(h = h)

day_rep = sf_graph$get_data()$day

################################################################################
################################# STATIONARY MODEL #############################
################################################################################


rspde_model_statWM <- rspde.metric_graph(sf_graph, 
                                         parameterization = "matern", nu = 0.5)

data_rspde_bru_s_WM <- graph_data_rspde(rspde_model_statWM, repl = ".all", loc_name = "loc")

cmp_statWM = speed ~ -1 +
  Intercept(1) +
  SpeedLimit + 
  density_per_hour +
  bus +
  signal +
  stop +
  crossing +
  upto1_ind +
  bus_number + 
  #signal_number +
  #stop_number +
  #crossing_number +
  field(loc, model = rspde_model_statWM, replicate = data_rspde_bru_s_WM[["repl"]])

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
                #mesh$density_per_hour, 0,
                #mesh$bus, 0
                #mesh$signal, 0,
                #mesh$stop, 0,
                #mesh$crossing, 0
                #mesh$upto1_ind, 0
                #mesh$bus_number, 0
                #mesh$signal_number, 0
                #mesh$stop_number, 0,
                #mesh$crossing_number, 0
) 

B.range = cbind(0, 0, 1, 
                0, mesh$SpeedLimit
                #0, mesh$density_per_hour,
                #0, mesh$bus
                #0, mesh$signal,
                #0, mesh$stop,
                #0, mesh$crossing
                #0, mesh$upto1_ind
                #0, mesh$bus_number
                #0, mesh$signal_number
                #0, mesh$stop_number,
                #0, mesh$crossing_number
) 

rspde_model_nonstatWM <- rspde.metric_graph(sf_graph,
                                            start.theta = c(fit.rspde$summary.log.std.dev$mode, fit.rspde$summary.log.range$mode, rep(0, 2)),
                                            B.sigma = B.sigma,
                                            B.range = B.range,
                                            parameterization = "matern", nu = 0.5)

data_rspde_bru_nsWM <- graph_data_rspde(rspde_model_nonstatWM, repl = ".all", loc_name = "loc")

cmp_nonstatWM = speed ~ -1 +
  Intercept(1) +
  SpeedLimit + 
  density_per_hour +
  bus +
  signal +
  stop +
  crossing +
  upto1_ind + 
  bus_number + 
  #signal_number +
  #stop_number +
  #crossing_number +
  field(loc, model = rspde_model_nonstatWM, replicate = data_rspde_bru_nsWM[["repl"]])

rspde_fit_nonstat_WM <-
  bru(cmp_nonstatWM,
      data = data_rspde_bru_nsWM[["data"]],
      family = "gaussian",
      options = list(verbose = FALSE)
  )

summary(rspde_fit_nonstat_WM)
summary(rspde.result(rspde_fit_nonstat_WM, "field", rspde_model_nonstatWM))

################################################################################
############################## PREDICTION ######################################
################################################################################     

# replicate.number = 1
# data_aux = data %>% filter(day == replicate.number)
# 
# prediction.data <- list()
# AAA = as.matrix(data.frame(data_aux$.edge_number, data_aux$.distance_on_edge))
# colnames(AAA) = c("", "d.e")
# prediction.data[["loc"]] = AAA
# prediction.data[["repl"]] = data_aux$day
# covariate.names = c("SpeedLimit", 
#                     "density_per_hour", 
#                     "bus", 
#                     "signal", 
#                     "stop", 
#                     "crossing", 
#                     "upto1_ind",
#                     "bus_number"
#                     #"signal_number",
#                     #"stop_number",
#                     #"crossing_number"
# )
# for (i in 1:length(covariate.names)) {
#   prediction.data[[covariate.names[i]]] = data_aux[[covariate.names[i]]]
# }
# 
# predicted.values.stat = predict(rspde_model_statWM, 
#                                 cmp_statWM, 
#                                 rspde_fit_stat_WM, 
#                                 newdata = prediction.data, 
#                                 formula = ~ Intercept +
#                                   SpeedLimit + 
#                                   density_per_hour +
#                                   bus +
#                                   signal +
#                                   stop +
#                                   crossing +
#                                   upto1_ind + 
#                                   bus_number + 
#                                   #signal_number +
#                                   #stop_number +
#                                   #crossing_number +
#                                   field_eval(loc, replicate = repl))
# 
# predicted.values.nonstat = predict(rspde_model_nonstatWM, 
#                                 cmp_nonstatWM, 
#                                 rspde_fit_nonstat_WM, 
#                                 newdata = prediction.data, 
#                                 formula = ~ Intercept +
#                                   SpeedLimit + 
#                                   density_per_hour +
#                                   bus +
#                                   signal +
#                                   stop +
#                                   crossing +
#                                   upto1_ind + 
#                                   bus_number + 
#                                   #signal_number +
#                                   #stop_number +
#                                   #crossing_number +
#                                   field_eval(loc, replicate = repl))

################################################################################
################################ CROSSVALIDATION ###############################
################################################################################


models <- list(nonstatWM = rspde_fit_nonstat_WM, statWM = rspde_fit_stat_WM)

cv_result <- cross_validation(models, cv_type = "k-fold", print = FALSE)
cv_result

# 
end_time <- Sys.time()
execution_time <- end_time - start_time


save.image("~/Desktop/Spring 2024/Model_runs/stat_vs_nonstat_gaussian_all_data_h0.2_no_consecutive_zeros_nu.upper.bound1.5.with.initial.val.from.stat2.check30.RData")


# Close the connection
sink()
