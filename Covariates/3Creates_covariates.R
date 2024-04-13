library(MetricGraph)
library(dplyr)
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

# initializing a vector to store the edges numbers from sign_data (they are not necessarily unique)
edges = c()

borderline = 0.05
# looping over the signs

edges_unique = unique(sign_data$edge_number)

list_for_indices = list()
list_for_indices[[(sf_graph$nE + 1)]] = "tmp1"
list_for_max_values = list()
list_for_max_values[[(sf_graph$nE + 1)]] = "tmp2"

for (k in 1:nrow(sign_data)) {
  
  # getting the sign at iteration i
  sign = sign_data[k,]
  
  # getting the edge that contains the sign
  edge_number = sign[[1]]
  
  # storing the edge_number into edges vector
  edges[k] = edge_number
  
  # getting the distance on the edge of the sign
  distance_on_edge = sign[[2]]
  
  original_distance = as.numeric(edge_length[edge_number])
  center_sign = original_distance*distance_on_edge
  
  vertices = vertex_matrix[edge_number,]
  
  if(center_sign <= borderline || center_sign >= (original_distance- borderline)){
    
    if(center_sign <= borderline){
      going_edges = which(vertex_matrix[,1] %in% vertices[1])
      for (i in 1:length(going_edges)) {
        center_going = -center_sign
        idx = which(data_on_graph_red$edge_number == going_edges[i])
        aux = data_on_graph_red$distance_on_edge[idx]*as.numeric(edge_length[going_edges[i]])
        eval = matrix(fun(aux - center_going), ncol = 1)
        list_for_max_values[[going_edges[i]]] = apply(cbind(eval, list_for_max_values[[going_edges[i]]]) , 1, max)
      }
      
      coming_edges = which(vertex_matrix[,2] %in% vertices[1])
      for (i in 1:length(coming_edges)) {
        center_coming = as.numeric(edge_length[coming_edges[i]]) + center_sign
        idx = which(data_on_graph_red$edge_number == coming_edges[i])
        aux = data_on_graph_red$distance_on_edge[idx]*as.numeric(edge_length[coming_edges[i]])
        eval = matrix(fun(aux - center_coming), ncol = 1)
        list_for_max_values[[coming_edges[i]]] = apply(cbind(eval, list_for_max_values[[coming_edges[i]]]) , 1, max)
      }
    }else{
      going_edges = which(vertex_matrix[,1] %in% vertices[2])
      for (i in 1:length(going_edges)) {
        center_going = -center_sign
        idx = which(data_on_graph_red$edge_number == going_edges[i])
        aux = data_on_graph_red$distance_on_edge[idx]*as.numeric(edge_length[going_edges[i]])
        eval = matrix(fun(aux - center_going), ncol = 1)
        list_for_max_values[[going_edges[i]]] = apply(cbind(eval, list_for_max_values[[going_edges[i]]]) , 1, max)
      }
      
      coming_edges = which(vertex_matrix[,2] %in% vertices[2])
      for (i in 1:length(coming_edges)) {
        center_coming = as.numeric(edge_length[coming_edges[i]]) + center_sign
        idx = which(data_on_graph_red$edge_number == coming_edges[i])
        aux = data_on_graph_red$distance_on_edge[idx]*as.numeric(edge_length[coming_edges[i]])
        eval = matrix(fun(aux - center_coming), ncol = 1)
        list_for_max_values[[coming_edges[i]]] = apply(cbind(eval, list_for_max_values[[coming_edges[i]]]) , 1, max)
    }
    }
  }
  
  
  
  
  # indices of the data that has edge numbers equal to edge_number (in practice, getting the identity of the point the edge with number edge_number)
  idx = which(data_on_graph_red$edge_number == edge_number)
  
  list_for_indices[[edge_number]] = idx
  
  # getting the distance of the points in the edge of interest
  aux = data_on_graph_red$distance_on_edge[idx]
  
  # computing the value of the covariate (with normalized and no normalized distance)
  
  eval = matrix(fun(as.numeric(edge_length[edge_number])*(aux - distance_on_edge)), ncol = 1)
  
  list_for_max_values[[edge_number]] = apply(cbind(eval, list_for_max_values[[edge_number]]) , 1, max)
  
   
  
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


