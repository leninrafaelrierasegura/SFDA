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

# select some covariates
data = data_on_graph_with_covariates %>% 
  dplyr::select(speed, 
                day,
                SpeedLimit,
                density_per_hour_normalized,
                bus,
                signal,
                stop,
                crossing) %>%
  mutate(bus = round(bus, 5), signal = round(signal, 5), stop = round(stop, 5), crossing = round(crossing, 5)) 

# add data to the graph
sf_graph$add_observations(data = data, group = "day", clear_obs = TRUE)


# create mesh
h = 0.2
sf_graph$build_mesh(h = h)

# compute observation matrix
start_time1 <- Sys.time()
sf_graph$compute_fem()
end_time1 <- Sys.time()
execution_time1 <- end_time1 - start_time1

start_time2 <- Sys.time()
A = sf_graph$fem_basis(PtE = data.frame(.edge_number = data$.edge_number, .distance_on_edge = data$.distance_on_edge))
end_time2 <- Sys.time()
execution_time2 <- end_time2 - start_time2

start_time3 <- Sys.time()
# get the index of the columns in A with at least one nonzero entry
non.zero = which(apply(A, 2, function(column) {any(column != 0)}))
end_time3 <- Sys.time()
execution_time3 <- end_time3 - start_time3

start_time4 <- Sys.time()
op = matern.operators(alpha = 1.5,
                      kappa = 10, 
                      tau = 1,
                      m = 0,
                      graph = sf_graph)
end_time4 <- Sys.time()
execution_time4 <- end_time4 - start_time4

# start_time5 <- Sys.time()
# # covariance matrix
# cov = op$covariance_mesh()
# end_time5 <- Sys.time()
# execution_time5 <- end_time5 - start_time5

# start_time6 <- Sys.time()
# # get the precision matrix for the corresponding nodes with at least one observation
# prec.from.cov = solve(cov[non.zero, non.zero])
# end_time6 <- Sys.time()
# execution_time6 <- end_time6 - start_time6

start_time7 <- Sys.time()
# compute the precision matrix
precision = precision(op)
end_time7 <- Sys.time()
execution_time7 <- end_time7 - start_time7


start_time8 <- Sys.time()
# R = chol(precision[-non.zero, -non.zero])
# v = solve(R,t(precision[non.zero, -non.zero]))
# prec.from.formula = precision[non.zero, non.zero] - t(v)%*%v


prec.from.formula = precision[non.zero, non.zero] - precision[non.zero, -non.zero]%*%solve(precision[-non.zero, -non.zero])%*%t(precision[non.zero, -non.zero])
end_time8 <- Sys.time()
execution_time8 <- end_time8 - start_time8

# cholesky factor
start_time9 <- Sys.time()
L.precision = chol(precision)
end_time9 <- Sys.time()
execution_time9 <- end_time9 - start_time9


start_time10 <- Sys.time()
L.prec.from.formula = chol(prec.from.formula)
# Error message
# Error in h(simpleError(msg, call)) : 
#   error in evaluating the argument 'x' in selecting a method for function 't': leading principal minor of order 1 is not positive
# In addition: Warning message:
#   In .local(A, ...) :
#   Cholmod warning 'not positive definite' at file ../Cholesky/t_cholmod_rowfac.c, line 430


end_time10 <- Sys.time()
execution_time10 <- end_time10 - start_time10


start_time11 <- Sys.time()
posterior.prec = precision + t(A)%*%A
L.post.prec = chol(posterior.prec)
end_time11 <- Sys.time()
execution_time11 <- end_time11 - start_time11


start_time12 <- Sys.time()
prec.prec = posterior.prec[non.zero, non.zero] - posterior.prec[non.zero, -non.zero]%*%solve(posterior.prec[-non.zero, -non.zero])%*%t(posterior.prec[non.zero, -non.zero])
end_time12 <- Sys.time()
execution_time12 <- end_time12 - start_time12


start_time13 <- Sys.time()
someting = chol(prec.prec)
end_time13 <- Sys.time()
execution_time13 <- end_time13 - start_time13


execution_time4 + execution_time7 + execution_time9
execution_time4 + execution_time7 + execution_time8 + execution_time10

execution_time4 + execution_time7 + execution_time11
execution_time4 + execution_time7 + execution_time12 + execution_time13

