#### CODE

# Open a connection to a text file
sink("~/Desktop/Spring 2024/Model_runs/stat_vs_nonstat_gaussian_all_data_h0.2_no_consecutuve_zeros_nu.upper.bound1.5.with.initial.val.from.stat2.check31.txt")

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
library(sf)

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


start_time <- Sys.time()
sf_graph$add_observations(data = data, group = "day", clear_obs = TRUE)
sf_graph$build_mesh(h = h)

################################################################################
################################# STATIONARY MODEL #############################
################################################################################


rspde_model_statWM <- rspde.metric_graph(sf_graph, 
                                         parameterization = "matern", nu = 0.5)

data_rspde_bru_s_WM <- graph_data_rspde(rspde_model_statWM, repl = ".all", loc_name = "loc")

cmp_statWM = speed ~ -1 +
  Intercept(1) +
  SpeedLimit + 
  upto1_ind +
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

B.sigma = cbind(0, 1, 0, mesh$SpeedLimit, 0) 
B.range = cbind(0, 0, 1, 0, mesh$SpeedLimit) 

rspde_model_nonstatWM <- rspde.metric_graph(sf_graph,
                                            start.theta = c(fit.rspde$summary.log.std.dev$mode, fit.rspde$summary.log.range$mode, rep(0, 2)),
                                            B.sigma = B.sigma,
                                            B.range = B.range,
                                            parameterization = "matern", nu = 0.5)

data_rspde_bru_nsWM <- graph_data_rspde(rspde_model_nonstatWM, repl = ".all", loc_name = "loc")

cmp_nonstatWM = speed ~ -1 +
  Intercept(1) +
  SpeedLimit + 
  upto1_ind + 
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
################################ CROSSVALIDATION ###############################
################################################################################

points = data_on_graph_with_covariates %>% 
  as.data.frame() %>%
  st_as_sf(coords = c(".coord_x", ".coord_y"), crs = 4326) %>%
  mutate(., index = 1:nrow(.)) %>%
  dplyr::select(index)


# to define a polygon
p01 = st_point(c(-122.47607, 37.80832))
p02 = st_point(c(-122.40655, 37.81065))
p03 = st_point(c(-122.38649, 37.79022))
p04 = st_point(c(-122.38224, 37.74355))
p05 = st_point(c(-122.40355, 37.70275))
p06 = st_point(c(-122.43990, 37.71142))
p07 = st_point(c(-122.47231, 37.70100))
p08 = st_point(c(-122.50785, 37.73528))
p09 = st_point(c(-122.51519, 37.78053))
p10 = st_point(c(-122.48840, 37.78684))
polygon = st_multipoint(c(p01, p02, p03, p04, p05, p06, p07, p08, p09, p10)) %>%
  st_cast("POLYGON") %>%
  st_sfc(crs = st_crs(points))


grid100 = st_make_grid(polygon, n = c(100, 100)) %>% 
  as.data.frame() %>% 
  st_as_sf(crs = st_crs(points)) %>% 
  st_centroid() %>%
  mutate(dummy = 1)

grid10 = st_make_grid(polygon, n = c(10, 10)) %>% 
  as.data.frame() %>% 
  st_as_sf(crs = st_crs(points)) %>% 
  st_centroid() %>%
  mutate(dummy = 1)

get.index = points %>% 
  st_join(x = grid100, y = ., join = st_nearest_feature) %>% 
  dplyr::select(index) %>%
  st_join(x = grid10, y = ., join = st_nearest_feature) %>%
  dplyr::select(index) %>%
  st_filter(polygon)

chosen.points = points[get.index$index, ]

distance = seq(from = 0, to = 1000, by = 50)
mse.stat <- mse.nonstat <- ls.stat <- ls.nonstat <- rep(0,length(distance))
# cross-validation for-loop
for(j in 1:length(distance)){
  print(j)
  groups <- list()
  for(i in 1:nrow(get.index)) {
    # get index of the points that should be removed
    groups[[i]] <- (chosen.points[i,2] %>% 
                      st_buffer(dist = distance[j], nQuadSegs = 1000000) %>%
                      st_filter(x = points, y = .) %>%
                      as.data.frame())$index
  }
  # cross-validation of the fractional model 
  cv.stat <- inla.group.cv(rspde_fit_stat_WM, groups = groups)
  # cross-validation of the integer model
  cv.nonstat <- inla.group.cv(rspde_fit_nonstat_WM, groups = groups)
  # obtain MSE and LS
  mse.stat[j] <- mean((cv.stat$mean - data$speed)^2)
  mse.nonstat[j] <- mean((cv.nonstat$mean - data$speed)^2)
  ls.stat[j] <- mean(log(cv.stat$cv))
  ls.nonstat[j] <- mean(log(cv.nonstat$cv))
}

## plot results
par(mfrow=c(1,2))
# plot MSE of the integer model
plot(distance, mse.stat, main = "MSE", ylim = c(min(mse.nonstat),max(mse.stat)), type = "l", ylab = "MSE", xlab = "distance in km")
# plot MSE of the fractional model
lines(distance, mse.nonstat, col = 2)
# plot negative LS of the integer model
plot(distance, -ls.stat, main = "log-score", ylim = c(min(-ls.nonstat),max(-ls.stat)), type = "l", ylab = "log-score", xlab = "distance in km")
# plot the negative LS of the fractional model
lines(distance, -ls.nonstat, col = 2)

end_time <- Sys.time()
execution_time <- end_time - start_time


save.image("~/Desktop/Spring 2024/Model_runs/stat_vs_nonstat_gaussian_all_data_h0.2_no_consecutive_zeros_nu.upper.bound1.5.with.initial.val.from.stat2.check31.RData")


# Close the connection
sink()
