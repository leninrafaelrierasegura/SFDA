library(MetricGraph)

# loading the data
load("Graph_objects/graph_construction_03_03_2024.RData")
load("Data_files/Data_in_graph_coordinates_for_every7days/data_on_graph_hour_13.RData")
load("Data_files/stops_data_on_graph.RData")

# length of each edge
edge_length = sf_graph$get_edge_lengths()

# matrix of vertices, the vertices in the i-th are the vertices that define edge i
vertex_matrix = sf_graph$E 

# getting the edge_number and distance_on_edge columns from data
data_on_graph_red = data.frame(edge_number = data_on_graph$.edge_number, 
                               distance_on_edge = data_on_graph$.distance_on_edge)


# defining function that decays from 1
fun = function(x){return(exp(-((x/0.02)^2)))}


# ------------------------------------------------------------------------------------------------------------------
# For bus_stop
# ------------------------------------------------------------------------------------------------------------------



# getting the sign_data (this could be bus_stop, traffic_signals, or stop)
sign_data = traffic_stops %>% 
  filter(highway == "bus_stop") %>% 
  as.data.frame() %>% 
  dplyr::select(.edge_number, .distance_on_edge) %>%
  rename(edge_number = .edge_number, distance_on_edge = .distance_on_edge)


# initializing lists to store values computed with normalize and no normalize distance
list_normalized = list()
list_no_normalized = list()

# initializing a vector to store the edges numbers from sign_data (they are not necesarily unique)
edges = c()

# looping over the signs
for (i in 1:nrow(sign_data)) {
  
  # getting the sign at iteration i
  sign = sign_data[i,]
  
  # getting the edge that contains the sign
  edge_number = sign[[1]]
  
  # storing the edge_number into edges vector
  edges[i] = edge_number
  
  # getting the distance on the edge of the sign
  distance_on_edge = sign[[2]]
  
  # indices of the data that has edge numbers equal to edge_number (in practice, getting the identity of the point the edge with number edge_number)
  idx = which(data_on_graph_red$edge_number == edge_number)
  
  # getting the distance of the points in the edge of interest
  aux = data_on_graph_red$distance_on_edge[idx]
  
  # computing the value of the covariate (with normalized and no normalized distance)
  local_cov_normalized = fun(aux - distance_on_edge)
  local_cov_no_normalized = fun(as.numeric(edge_length[edge_number])*(aux - distance_on_edge))
  
  # storing the indices and the value of the covariate
  list_normalized[[i]] = cbind(idx, local_cov_normalized)
  list_no_normalized[[i]] = cbind(idx, local_cov_no_normalized)
  
}

# getting the unique edges from edges
unique = unique(edges)

# initialize list
list_new_normalized = list()
list_new_no_normalized = list()

# this for loop cbind according to the same edge 
for (k in 1:length(unique)) {
  # getting the positions on vector edges of unique edge numbers 
  positions = which(unique[k] == edges)
  list_new_normalized[[k]] = do.call(cbind, list_normalized[positions])
  list_new_no_normalized[[k]] = do.call(cbind, list_no_normalized[positions])
}


filtered_normalized = list_new_normalized[sapply(list_new_normalized, function(x) nrow(x) == 0) == FALSE]
filtered_no_normalized = list_new_no_normalized[sapply(list_new_no_normalized, function(x) nrow(x) == 0) == FALSE]


# Creating number of bus stops covariate ----------------------------------------------------------

list_aux_variable = list()
vector_aux = c()
for (i in 1:length(filtered_normalized)) {
  list_aux_variable[[i]] = as.vector(filtered_normalized[[i]][,1])
  vector_aux[i] = ncol(filtered_normalized[[i]])/2
}


cov_number_bus = rep(0, nrow(data_on_graph))


for (i in 1:length(vector_aux)) {
  cov_number_bus[list_aux_variable[[i]]] = vector_aux[i]
}

# -------------------------------------------------------------------------------------------------

list_final_normalized = list()
list_final_no_normalized = list()


new_L = length(filtered_normalized)
for (i in 1:new_L) {
  A_normalized = filtered_normalized[[i]]
  A_no_normalized = filtered_no_normalized[[i]]
  if (ncol(A_normalized) > 2){
    even_cols = seq(2, ncol(A_normalized), by = 2)
    B_normalized = matrix(A_normalized[, even_cols], ncol = length(even_cols))
    B_no_normalized = matrix(A_no_normalized[, even_cols], ncol = length(even_cols))
    list_final_normalized[[i]] = cbind(A_normalized[, 1], apply(B_normalized, 1, max))
    list_final_no_normalized[[i]] = cbind(A_no_normalized[, 1], apply(B_no_normalized, 1, max))
  } else {
    list_final_normalized[[i]] = filtered_normalized[[i]]
    list_final_no_normalized[[i]] = filtered_no_normalized[[i]]
  }
}


normalized = do.call(rbind, list_final_normalized) %>% as.data.frame()
no_normalized = do.call(rbind, list_final_no_normalized) %>% as.data.frame()

cov_bus_normalized = rep(0, nrow(data_on_graph))
cov_bus_no_normalized = rep(0, nrow(data_on_graph))

cov_bus_normalized[normalized$idx] = normalized$local_cov_normalized
cov_bus_no_normalized[no_normalized$idx] = no_normalized$local_cov_no_normalized
                     
final1 = data_on_graph %>% mutate(cov_bus_normalized = cov_bus_normalized, cov_bus_no_normalized = cov_bus_no_normalized, cov_number_bus = cov_number_bus)


# ------------------------------------------------------------------------------------------------------------------
# For traffic_signals
# ------------------------------------------------------------------------------------------------------------------



# getting the sign_data (this could be bus_stop, traffic_signals, or stop)
sign_data = traffic_stops %>% 
  filter(highway == "traffic_signals") %>% 
  as.data.frame() %>% 
  dplyr::select(.edge_number, .distance_on_edge) %>%
  rename(edge_number = .edge_number, distance_on_edge = .distance_on_edge)


# initializing lists to store values computed with normalize and no normalize distance
list_normalized = list()
list_no_normalized = list()

# initializing a vector to store the edges numbers from sign_data (they are not necesarily unique)
edges = c()

# looping over the signs
for (i in 1:nrow(sign_data)) {
  
  # getting the sign at iteration i
  sign = sign_data[i,]
  
  # getting the edge that contains the sign
  edge_number = sign[[1]]
  
  # storing the edge_number into edges vector
  edges[i] = edge_number
  
  # getting the distance on the edge of the sign
  distance_on_edge = sign[[2]]
  
  # indices of the data that has edge numbers equal to edge_number (in practice, getting the identity of the point the edge with number edge_number)
  idx = which(data_on_graph_red$edge_number == edge_number)
  
  # getting the distance of the points in the edge of interest
  aux = data_on_graph_red$distance_on_edge[idx]
  
  # computing the value of the covariate (with normalized and no normalized distance)
  local_cov_normalized = fun(aux - distance_on_edge)
  local_cov_no_normalized = fun(as.numeric(edge_length[edge_number])*(aux - distance_on_edge))
  
  # storing the indices and the value of the covariate
  list_normalized[[i]] = cbind(idx, local_cov_normalized)
  list_no_normalized[[i]] = cbind(idx, local_cov_no_normalized)
  
}

# getting the unique edges from edges
unique = unique(edges)

# initialize list
list_new_normalized = list()
list_new_no_normalized = list()

# this for loop cbind according to the same edge 
for (k in 1:length(unique)) {
  # getting the positions on vector edges of unique edge numbers 
  positions = which(unique[k] == edges)
  list_new_normalized[[k]] = do.call(cbind, list_normalized[positions])
  list_new_no_normalized[[k]] = do.call(cbind, list_no_normalized[positions])
}


filtered_normalized = list_new_normalized[sapply(list_new_normalized, function(x) nrow(x) == 0) == FALSE]
filtered_no_normalized = list_new_no_normalized[sapply(list_new_no_normalized, function(x) nrow(x) == 0) == FALSE]

# Creating number of traffic signals covariate ----------------------------------------------------------

list_aux_variable = list()
vector_aux = c()
for (i in 1:length(filtered_normalized)) {
  list_aux_variable[[i]] = as.vector(filtered_normalized[[i]][,1])
  vector_aux[i] = ncol(filtered_normalized[[i]])/2
}


cov_number_bus = rep(0, nrow(data_on_graph))


for (i in 1:length(vector_aux)) {
  cov_number_bus[list_aux_variable[[i]]] = vector_aux[i]
}

# -------------------------------------------------------------------------------------------------


list_final_normalized = list()
list_final_no_normalized = list()


new_L = length(filtered_normalized)
for (i in 1:new_L) {
  A_normalized = filtered_normalized[[i]]
  A_no_normalized = filtered_no_normalized[[i]]
  if (ncol(A_normalized) > 2){
    even_cols = seq(2, ncol(A_normalized), by = 2)
    B_normalized = matrix(A_normalized[, even_cols], ncol = length(even_cols))
    B_no_normalized = matrix(A_no_normalized[, even_cols], ncol = length(even_cols))
    list_final_normalized[[i]] = cbind(A_normalized[, 1], apply(B_normalized, 1, max))
    list_final_no_normalized[[i]] = cbind(A_no_normalized[, 1], apply(B_no_normalized, 1, max))
  } else {
    list_final_normalized[[i]] = filtered_normalized[[i]]
    list_final_no_normalized[[i]] = filtered_no_normalized[[i]]
  }
}


normalized = do.call(rbind, list_final_normalized) %>% as.data.frame()
no_normalized = do.call(rbind, list_final_no_normalized) %>% as.data.frame()

cov_bus_normalized = rep(0, nrow(data_on_graph))
cov_bus_no_normalized = rep(0, nrow(data_on_graph))

cov_bus_normalized[normalized$idx] = normalized$local_cov_normalized
cov_bus_no_normalized[no_normalized$idx] = no_normalized$local_cov_no_normalized

final2 = data_on_graph %>% mutate(cov_signals_normalized = cov_bus_normalized, cov_signals_no_normalized = cov_bus_no_normalized, cov_number_signals = cov_number_bus)


# ------------------------------------------------------------------------------------------------------------------
# For stop
# ------------------------------------------------------------------------------------------------------------------



# getting the sign_data (this could be bus_stop, traffic_signals, or stop)
sign_data = traffic_stops %>% 
  filter(highway == "stop") %>% 
  as.data.frame() %>% 
  dplyr::select(.edge_number, .distance_on_edge) %>%
  rename(edge_number = .edge_number, distance_on_edge = .distance_on_edge)


# initializing lists to store values computed with normalize and no normalize distance
list_normalized = list()
list_no_normalized = list()

# initializing a vector to store the edges numbers from sign_data (they are not necesarily unique)
edges = c()

# looping over the signs
for (i in 1:nrow(sign_data)) {
  
  # getting the sign at iteration i
  sign = sign_data[i,]
  
  # getting the edge that contains the sign
  edge_number = sign[[1]]
  
  # storing the edge_number into edges vector
  edges[i] = edge_number
  
  # getting the distance on the edge of the sign
  distance_on_edge = sign[[2]]
  
  # indices of the data that has edge numbers equal to edge_number (in practice, getting the identity of the point the edge with number edge_number)
  idx = which(data_on_graph_red$edge_number == edge_number)
  
  # getting the distance of the points in the edge of interest
  aux = data_on_graph_red$distance_on_edge[idx]
  
  # computing the value of the covariate (with normalized and no normalized distance)
  local_cov_normalized = fun(aux - distance_on_edge)
  local_cov_no_normalized = fun(as.numeric(edge_length[edge_number])*(aux - distance_on_edge))
  
  # storing the indices and the value of the covariate
  list_normalized[[i]] = cbind(idx, local_cov_normalized)
  list_no_normalized[[i]] = cbind(idx, local_cov_no_normalized)
  
}

# getting the unique edges from edges
unique = unique(edges)

# initialize list
list_new_normalized = list()
list_new_no_normalized = list()

# this for loop cbind according to the same edge 
for (k in 1:length(unique)) {
  # getting the positions on vector edges of unique edge numbers 
  positions = which(unique[k] == edges)
  list_new_normalized[[k]] = do.call(cbind, list_normalized[positions])
  list_new_no_normalized[[k]] = do.call(cbind, list_no_normalized[positions])
}


filtered_normalized = list_new_normalized[sapply(list_new_normalized, function(x) nrow(x) == 0) == FALSE]
filtered_no_normalized = list_new_no_normalized[sapply(list_new_no_normalized, function(x) nrow(x) == 0) == FALSE]


# Creating number of stop signs covariate ----------------------------------------------------------

list_aux_variable = list()
vector_aux = c()
for (i in 1:length(filtered_normalized)) {
  list_aux_variable[[i]] = as.vector(filtered_normalized[[i]][,1])
  vector_aux[i] = ncol(filtered_normalized[[i]])/2
}


cov_number_bus = rep(0, nrow(data_on_graph))


for (i in 1:length(vector_aux)) {
  cov_number_bus[list_aux_variable[[i]]] = vector_aux[i]
}

# -------------------------------------------------------------------------------------------------


list_final_normalized = list()
list_final_no_normalized = list()


new_L = length(filtered_normalized)
for (i in 1:new_L) {
  A_normalized = filtered_normalized[[i]]
  A_no_normalized = filtered_no_normalized[[i]]
  if (ncol(A_normalized) > 2){
    even_cols = seq(2, ncol(A_normalized), by = 2)
    B_normalized = matrix(A_normalized[, even_cols], ncol = length(even_cols))
    B_no_normalized = matrix(A_no_normalized[, even_cols], ncol = length(even_cols))
    list_final_normalized[[i]] = cbind(A_normalized[, 1], apply(B_normalized, 1, max))
    list_final_no_normalized[[i]] = cbind(A_no_normalized[, 1], apply(B_no_normalized, 1, max))
  } else {
    list_final_normalized[[i]] = filtered_normalized[[i]]
    list_final_no_normalized[[i]] = filtered_no_normalized[[i]]
  }
}


normalized = do.call(rbind, list_final_normalized) %>% as.data.frame()
no_normalized = do.call(rbind, list_final_no_normalized) %>% as.data.frame()

cov_bus_normalized = rep(0, nrow(data_on_graph))
cov_bus_no_normalized = rep(0, nrow(data_on_graph))

cov_bus_normalized[normalized$idx] = normalized$local_cov_normalized
cov_bus_no_normalized[no_normalized$idx] = no_normalized$local_cov_no_normalized

final3 = data_on_graph %>% mutate(cov_stop_normalized = cov_bus_normalized, cov_stop_no_normalized = cov_bus_no_normalized, cov_number_stop = cov_number_bus)


# ------------------------------------------------------------------------------------------------------------------
# For crossing
# ------------------------------------------------------------------------------------------------------------------



# getting the sign_data (this could be bus_stop, traffic_signals, or stop)
sign_data = traffic_stops %>% 
  filter(highway == "crossing") %>% 
  as.data.frame() %>% 
  dplyr::select(.edge_number, .distance_on_edge) %>%
  rename(edge_number = .edge_number, distance_on_edge = .distance_on_edge)


# initializing lists to store values computed with normalize and no normalize distance
list_normalized = list()
list_no_normalized = list()

# initializing a vector to store the edges numbers from sign_data (they are not necesarily unique)
edges = c()

# looping over the signs
for (i in 1:nrow(sign_data)) {
  
  # getting the sign at iteration i
  sign = sign_data[i,]
  
  # getting the edge that contains the sign
  edge_number = sign[[1]]
  
  # storing the edge_number into edges vector
  edges[i] = edge_number
  
  # getting the distance on the edge of the sign
  distance_on_edge = sign[[2]]
  
  # indices of the data that has edge numbers equal to edge_number (in practice, getting the identity of the point the edge with number edge_number)
  idx = which(data_on_graph_red$edge_number == edge_number)
  
  # getting the distance of the points in the edge of interest
  aux = data_on_graph_red$distance_on_edge[idx]
  
  # computing the value of the covariate (with normalized and no normalized distance)
  local_cov_normalized = fun(aux - distance_on_edge)
  local_cov_no_normalized = fun(as.numeric(edge_length[edge_number])*(aux - distance_on_edge))
  
  # storing the indices and the value of the covariate
  list_normalized[[i]] = cbind(idx, local_cov_normalized)
  list_no_normalized[[i]] = cbind(idx, local_cov_no_normalized)
  
}

# getting the unique edges from edges
unique = unique(edges)

# initialize list
list_new_normalized = list()
list_new_no_normalized = list()

# this for loop cbind according to the same edge 
for (k in 1:length(unique)) {
  # getting the positions on vector edges of unique edge numbers 
  positions = which(unique[k] == edges)
  list_new_normalized[[k]] = do.call(cbind, list_normalized[positions])
  list_new_no_normalized[[k]] = do.call(cbind, list_no_normalized[positions])
}


filtered_normalized = list_new_normalized[sapply(list_new_normalized, function(x) nrow(x) == 0) == FALSE]
filtered_no_normalized = list_new_no_normalized[sapply(list_new_no_normalized, function(x) nrow(x) == 0) == FALSE]



# Creating number of stop signs covariate ----------------------------------------------------------

list_aux_variable = list()
vector_aux = c()
for (i in 1:length(filtered_normalized)) {
  list_aux_variable[[i]] = as.vector(filtered_normalized[[i]][,1])
  vector_aux[i] = ncol(filtered_normalized[[i]])/2
}


cov_number_bus = rep(0, nrow(data_on_graph))


for (i in 1:length(vector_aux)) {
  cov_number_bus[list_aux_variable[[i]]] = vector_aux[i]
}

# -------------------------------------------------------------------------------------------------


list_final_normalized = list()
list_final_no_normalized = list()


new_L = length(filtered_normalized)
for (i in 1:new_L) {
  A_normalized = filtered_normalized[[i]]
  A_no_normalized = filtered_no_normalized[[i]]
  if (ncol(A_normalized) > 2){
    even_cols = seq(2, ncol(A_normalized), by = 2)
    B_normalized = matrix(A_normalized[, even_cols], ncol = length(even_cols))
    B_no_normalized = matrix(A_no_normalized[, even_cols], ncol = length(even_cols))
    list_final_normalized[[i]] = cbind(A_normalized[, 1], apply(B_normalized, 1, max))
    list_final_no_normalized[[i]] = cbind(A_no_normalized[, 1], apply(B_no_normalized, 1, max))
  } else {
    list_final_normalized[[i]] = filtered_normalized[[i]]
    list_final_no_normalized[[i]] = filtered_no_normalized[[i]]
  }
}


normalized = do.call(rbind, list_final_normalized) %>% as.data.frame()
no_normalized = do.call(rbind, list_final_no_normalized) %>% as.data.frame()

cov_bus_normalized = rep(0, nrow(data_on_graph))
cov_bus_no_normalized = rep(0, nrow(data_on_graph))

cov_bus_normalized[normalized$idx] = normalized$local_cov_normalized
cov_bus_no_normalized[no_normalized$idx] = no_normalized$local_cov_no_normalized

final4 = data_on_graph %>% mutate(cov_crossing_normalized = cov_bus_normalized, cov_crossing_no_normalized = cov_bus_no_normalized, cov_number_crossing = cov_number_bus)




data_on_graph_with_bus_signals_stop_cov = data_on_graph %>% mutate(cov_bus_normalized = final1$cov_bus_normalized, 
                                                                   cov_bus_no_normalized = final1$cov_bus_no_normalized,
                                                                   cov_number_bus = final1$cov_number_bus,
                                                                   cov_signals_normalized = final2$cov_signals_normalized,
                                                                   cov_signals_no_normalized = final2$cov_signals_no_normalized,
                                                                   cov_number_signals = final2$cov_number_signals,
                                                                   cov_stop_normalized = final3$cov_stop_normalized,
                                                                   cov_stop_no_normalized = final3$cov_stop_no_normalized,
                                                                   cov_number_stop = final3$cov_number_stop,
                                                                   cov_crossing_normalized = final4$cov_crossing_normalized,
                                                                   cov_crossing_no_normalized = final4$cov_crossing_no_normalized,
                                                                   cov_number_crossing = final4$cov_number_crossing)




cov_obs <- data_on_graph[[".distance_on_edge"]]
cov_obs <- 2 * ifelse(cov_obs > 0.5,  1 - cov_obs, cov_obs)

weights = sf_graph$get_edge_weights() %>% 
  mutate(Length  = Length/1000) %>% 
  mutate(density = sampleSize/Length) %>% 
  mutate(density_per_hour = density/24) 


sf_graph$set_edge_weights(weights = weights)



sf_graph$add_observations(data = data_on_graph_with_bus_signals_stop_cov, group = "day", clear_obs = TRUE)
sf_graph$edgeweight_to_data(data_loc = TRUE)

data_on_graph_with_bus_signals_stop_density_covariates = sf_graph$get_data() %>% na.omit() #%>% mutate(cov_obs = cov_obs)

save(data_on_graph_with_bus_signals_stop_density_covariates, file = "Data_files/data_on_graph_with_bus_signals_stop_density_and_numbers_covariates.RData")


