creates_covariates_on_mesh = function(h){
  
  library(MetricGraph)
  library(dplyr)
  # loading the data
  load("Graph_objects/graph_construction_28_03_2024partialtomtomwhichlonglatsf.RData")
  load("Data_files/data_day7142128_hour13_with_no_consecutive_zeros_partialtomtom_graph_processed.RData")
  load("Data_files/stops_data_on_graph_partialtomtom.RData")
  
  # length of each edge
  edge_length = sf_graph$get_edge_lengths()
  
  # matrix of vertices, the vertices in the i-th are the vertices that define edge i
  vertex_matrix = sf_graph$E 
  
  sf_graph$add_observations(data = data_on_graph, group = "day", clear_obs = TRUE)
  
  sf_graph$build_mesh(h = h)
  
  data_on_mesh = sf_graph$edgeweight_to_data(mesh = TRUE, add = FALSE, return = TRUE) %>% filter(.group == 1)
  
  data_on_graph = data_on_mesh
  
  # getting the edge_number and distance_on_edge columns from data
  data_on_graph_red = data.frame(edge_number = data_on_graph$.edge_number, 
                                 distance_on_edge = data_on_graph$.distance_on_edge)
  
  
  # defining function that decays from 1
  fun = function(x){return(exp(-((x/0.05)^2)))}
  
  
  # ------------------------------------------------------------------------------------------------------------------
  # For bus_stop
  # ------------------------------------------------------------------------------------------------------------------
  
  
  
  # getting the sign_data (this could be bus_stop, traffic_signals, stop or crossing)
  sign_data = traffic_stops %>% 
    filter(highway == "bus_stop") %>% 
    as.data.frame() %>% 
    dplyr::select(.edge_number, .distance_on_edge) %>%
    rename(edge_number = .edge_number, distance_on_edge = .distance_on_edge)
  
  # initializing a vector to store the edges numbers from sign_data (they are not necessarily unique)
  edges = c()
  
  borderline = 0.1
  # looping over the signs
  
  
  list_for_indices = list()
  list_for_indices[[(sf_graph$nE + 1)]] = "tmp1"
  list_for_max_values = list()
  list_for_max_values[[(sf_graph$nE + 1)]] = "tmp2"
  
  # ----------------------------------------------------------------------------------------------
  
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
    
    if(center_sign <= borderline){
      # going
      going_edges = which(vertex_matrix[,1] %in% vertices[1])
      if(length(going_edges) > 0){
        for (i in 1:length(going_edges)) {
          center_going = -center_sign
          idx = which(data_on_graph_red$edge_number == going_edges[i])
          list_for_indices[[going_edges[i]]] = idx
          aux = data_on_graph_red$distance_on_edge[idx]*as.numeric(edge_length[going_edges[i]])
          eval = matrix(fun(aux - center_going), ncol = 1)
          list_for_max_values[[going_edges[i]]] = apply(cbind(eval, list_for_max_values[[going_edges[i]]]) , 1, max)
        }
      }
      
      # coming
      coming_edges = which(vertex_matrix[,2] %in% vertices[1])
      if(length(coming_edges) > 0){
        for (i in 1:length(coming_edges)) {
          center_coming = as.numeric(edge_length[coming_edges[i]]) + center_sign
          idx = which(data_on_graph_red$edge_number == coming_edges[i])
          list_for_indices[[coming_edges[i]]] = idx
          aux = data_on_graph_red$distance_on_edge[idx]*as.numeric(edge_length[coming_edges[i]])
          eval = matrix(fun(aux - center_coming), ncol = 1)
          list_for_max_values[[coming_edges[i]]] = apply(cbind(eval, list_for_max_values[[coming_edges[i]]]) , 1, max)
        }
      }
      
    }
    if(center_sign >= (original_distance- borderline)){
      # going
      going_edges = which(vertex_matrix[,1] %in% vertices[2])
      if(length(going_edges) > 0){
        for (i in 1:length(going_edges)) {
          center_going = -(original_distance - center_sign)
          idx = which(data_on_graph_red$edge_number == going_edges[i])
          list_for_indices[[going_edges[i]]] = idx
          aux = data_on_graph_red$distance_on_edge[idx]*as.numeric(edge_length[going_edges[i]])
          eval = matrix(fun(aux - center_going), ncol = 1)
          list_for_max_values[[going_edges[i]]] = apply(cbind(eval, list_for_max_values[[going_edges[i]]]) , 1, max)
        }
      }
      # coming
      coming_edges = which(vertex_matrix[,2] %in% vertices[2])
      if(length(coming_edges) > 0){
        for (i in 1:length(coming_edges)) {
          center_coming = as.numeric(edge_length[coming_edges[i]]) + (original_distance - center_sign)
          idx = which(data_on_graph_red$edge_number == coming_edges[i])
          list_for_indices[[coming_edges[i]]] = idx
          aux = data_on_graph_red$distance_on_edge[idx]*as.numeric(edge_length[coming_edges[i]])
          eval = matrix(fun(aux - center_coming), ncol = 1)
          list_for_max_values[[coming_edges[i]]] = apply(cbind(eval, list_for_max_values[[coming_edges[i]]]) , 1, max)
        } # end for
      }# end if
    } # end else
  }
  
  
  #----------------------------------------------------------------------------------------------------
  # we compute covariate at the points on each edge just taking into account only signs in the same edge
  for (k in 1:nrow(sign_data)) {
    # getting the sign at iteration i
    sign = sign_data[k,]
    # getting the edge that contains the sign
    edge_number = sign[[1]]
    # storing the edge_number into edges vector
    edges[k] = edge_number
    # getting the distance on the edge of the sign
    distance_on_edge = sign[[2]]
    
    # indices of the data that has edge numbers equal to edge_number (in practice, getting the identity of the point the edge with number edge_number)
    idx = which(data_on_graph_red$edge_number == edge_number)
    list_for_indices[[edge_number]] = cbind(idx, list_for_indices[[edge_number]]) # to repeat the columns
    # getting the distance of the points in the edge of interest
    aux = data_on_graph_red$distance_on_edge[idx]
    # computing the value of the covariate (with normalized and no normalized distance)
    eval = matrix(fun(as.numeric(edge_length[edge_number])*(aux - distance_on_edge)), ncol = 1)
    list_for_max_values[[edge_number]] = apply(cbind(eval, list_for_max_values[[edge_number]]) , 1, max)
  }
  
  list_for_indices[[(sf_graph$nE + 1)]] = NULL
  list_for_max_values[[(sf_graph$nE + 1)]] = NULL
  
  # drop NULL members
  filtered_max_values = list_for_max_values[!sapply(list_for_max_values, is.null)]
  filtered_indices = list_for_indices[!sapply(list_for_indices, is.null)]
  
  new_filtered_max_values = filtered_max_values[sapply(filtered_max_values, function(x) length(x) > 0)]
  new_filtered_indices = filtered_indices[sapply(filtered_indices, function(x) length(x) > 0)]
  
  
  # Creating number of signs covariate ----------------------------------------------------------
  
  cov_number_sign = rep(0, nrow(data_on_graph_red))
  
  for (i in 1:length(new_filtered_indices)) {
    if(length(dim(new_filtered_indices[[i]])) == 2){ # that is, if it is a matrix
      cov_number_sign[new_filtered_indices[[i]][,1]] = ncol(new_filtered_indices[[i]])
      new_filtered_indices[[i]] = as.vector(new_filtered_indices[[i]][,1]) # just getting one column
    }
  }
  
  # -------------------------------------------------------------------------------------------------
  
  covariate_alltogether = data.frame(all_indices = unlist(new_filtered_indices),
                                     all_max_values = unlist(new_filtered_max_values))
  
  
  
  final_cov = rep(0, nrow(data_on_graph_red))
  final_cov[covariate_alltogether$all_indices] = covariate_alltogether$all_max_values
  
  final1 = data_on_graph_red %>% mutate(final_cov = final_cov, cov_number_sign = cov_number_sign)
  
  
  # ------------------------------------------------------------------------------------------------------------------
  # For traffic_signals
  # ------------------------------------------------------------------------------------------------------------------
  
  
  # getting the sign_data (this could be bus_stop, traffic_signals, stop or crossing)
  
  sign_data = traffic_stops %>% 
    filter(highway == "traffic_signals") %>% 
    as.data.frame() %>% 
    dplyr::select(.edge_number, .distance_on_edge) %>%
    rename(edge_number = .edge_number, distance_on_edge = .distance_on_edge)
  
  # initializing a vector to store the edges numbers from sign_data (they are not necessarily unique)
  edges = c()
  
  borderline = 0.05
  # looping over the signs
  
  
  list_for_indices = list()
  list_for_indices[[(sf_graph$nE + 1)]] = "tmp1"
  list_for_max_values = list()
  list_for_max_values[[(sf_graph$nE + 1)]] = "tmp2"
  
  # ----------------------------------------------------------------------------------------------
  
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
    
    if(center_sign <= borderline){
      # going
      going_edges = which(vertex_matrix[,1] %in% vertices[1])
      if(length(going_edges) > 0){
        for (i in 1:length(going_edges)) {
          center_going = -center_sign
          idx = which(data_on_graph_red$edge_number == going_edges[i])
          list_for_indices[[going_edges[i]]] = idx
          aux = data_on_graph_red$distance_on_edge[idx]*as.numeric(edge_length[going_edges[i]])
          eval = matrix(fun(aux - center_going), ncol = 1)
          list_for_max_values[[going_edges[i]]] = apply(cbind(eval, list_for_max_values[[going_edges[i]]]) , 1, max)
        }
      }
      
      # coming
      coming_edges = which(vertex_matrix[,2] %in% vertices[1])
      if(length(coming_edges) > 0){
        for (i in 1:length(coming_edges)) {
          center_coming = as.numeric(edge_length[coming_edges[i]]) + center_sign
          idx = which(data_on_graph_red$edge_number == coming_edges[i])
          list_for_indices[[coming_edges[i]]] = idx
          aux = data_on_graph_red$distance_on_edge[idx]*as.numeric(edge_length[coming_edges[i]])
          eval = matrix(fun(aux - center_coming), ncol = 1)
          list_for_max_values[[coming_edges[i]]] = apply(cbind(eval, list_for_max_values[[coming_edges[i]]]) , 1, max)
        }
      }
      
    }
    if(center_sign >= (original_distance- borderline)){
      # going
      going_edges = which(vertex_matrix[,1] %in% vertices[2])
      if(length(going_edges) > 0){
        for (i in 1:length(going_edges)) {
          center_going = -(original_distance - center_sign)
          idx = which(data_on_graph_red$edge_number == going_edges[i])
          list_for_indices[[going_edges[i]]] = idx
          aux = data_on_graph_red$distance_on_edge[idx]*as.numeric(edge_length[going_edges[i]])
          eval = matrix(fun(aux - center_going), ncol = 1)
          list_for_max_values[[going_edges[i]]] = apply(cbind(eval, list_for_max_values[[going_edges[i]]]) , 1, max)
        }
      }
      # coming
      coming_edges = which(vertex_matrix[,2] %in% vertices[2])
      if(length(coming_edges) > 0){
        for (i in 1:length(coming_edges)) {
          center_coming = as.numeric(edge_length[coming_edges[i]]) + (original_distance - center_sign)
          idx = which(data_on_graph_red$edge_number == coming_edges[i])
          list_for_indices[[coming_edges[i]]] = idx
          aux = data_on_graph_red$distance_on_edge[idx]*as.numeric(edge_length[coming_edges[i]])
          eval = matrix(fun(aux - center_coming), ncol = 1)
          list_for_max_values[[coming_edges[i]]] = apply(cbind(eval, list_for_max_values[[coming_edges[i]]]) , 1, max)
        } # end for
      }# end if
    } # end else
  }
  
  
  #----------------------------------------------------------------------------------------------------
  # we compute covariate at the points on each edge just taking into account only signs in the same edge
  for (k in 1:nrow(sign_data)) {
    # getting the sign at iteration i
    sign = sign_data[k,]
    # getting the edge that contains the sign
    edge_number = sign[[1]]
    # storing the edge_number into edges vector
    edges[k] = edge_number
    # getting the distance on the edge of the sign
    distance_on_edge = sign[[2]]
    
    # indices of the data that has edge numbers equal to edge_number (in practice, getting the identity of the point the edge with number edge_number)
    idx = which(data_on_graph_red$edge_number == edge_number)
    list_for_indices[[edge_number]] = cbind(idx, list_for_indices[[edge_number]]) # to repeat the columns
    # getting the distance of the points in the edge of interest
    aux = data_on_graph_red$distance_on_edge[idx]
    # computing the value of the covariate (with normalized and no normalized distance)
    eval = matrix(fun(as.numeric(edge_length[edge_number])*(aux - distance_on_edge)), ncol = 1)
    list_for_max_values[[edge_number]] = apply(cbind(eval, list_for_max_values[[edge_number]]) , 1, max)
  }
  
  list_for_indices[[(sf_graph$nE + 1)]] = NULL
  list_for_max_values[[(sf_graph$nE + 1)]] = NULL
  
  # drop NULL members
  filtered_max_values = list_for_max_values[!sapply(list_for_max_values, is.null)]
  filtered_indices = list_for_indices[!sapply(list_for_indices, is.null)]
  
  new_filtered_max_values = filtered_max_values[sapply(filtered_max_values, function(x) length(x) > 0)]
  new_filtered_indices = filtered_indices[sapply(filtered_indices, function(x) length(x) > 0)]
  
  
  # Creating number of signs covariate ----------------------------------------------------------
  
  cov_number_sign = rep(0, nrow(data_on_graph_red))
  
  for (i in 1:length(new_filtered_indices)) {
    if(length(dim(new_filtered_indices[[i]])) == 2){ # that is, if it is a matrix
      cov_number_sign[new_filtered_indices[[i]][,1]] = ncol(new_filtered_indices[[i]])
      new_filtered_indices[[i]] = as.vector(new_filtered_indices[[i]][,1]) # just getting one column
    }
  }
  
  # -------------------------------------------------------------------------------------------------
  
  covariate_alltogether = data.frame(all_indices = unlist(new_filtered_indices),
                                     all_max_values = unlist(new_filtered_max_values))
  
  
  
  final_cov = rep(0, nrow(data_on_graph_red))
  final_cov[covariate_alltogether$all_indices] = covariate_alltogether$all_max_values
  
  final2 = data_on_graph_red %>% mutate(final_cov = final_cov, cov_number_sign = cov_number_sign)
  
  
  # ------------------------------------------------------------------------------------------------------------------
  # For stop
  # ------------------------------------------------------------------------------------------------------------------
  
  
  # getting the sign_data (this could be bus_stop, traffic_signals, stop or crossing)
  
  sign_data = traffic_stops %>% 
    filter(highway == "stop") %>% 
    as.data.frame() %>% 
    dplyr::select(.edge_number, .distance_on_edge) %>%
    rename(edge_number = .edge_number, distance_on_edge = .distance_on_edge)
  
  # initializing a vector to store the edges numbers from sign_data (they are not necessarily unique)
  edges = c()
  
  borderline = 0.05
  # looping over the signs
  
  
  list_for_indices = list()
  list_for_indices[[(sf_graph$nE + 1)]] = "tmp1"
  list_for_max_values = list()
  list_for_max_values[[(sf_graph$nE + 1)]] = "tmp2"
  
  # ----------------------------------------------------------------------------------------------
  
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
    
    if(center_sign <= borderline){
      # going
      going_edges = which(vertex_matrix[,1] %in% vertices[1])
      if(length(going_edges) > 0){
        for (i in 1:length(going_edges)) {
          center_going = -center_sign
          idx = which(data_on_graph_red$edge_number == going_edges[i])
          list_for_indices[[going_edges[i]]] = idx
          aux = data_on_graph_red$distance_on_edge[idx]*as.numeric(edge_length[going_edges[i]])
          eval = matrix(fun(aux - center_going), ncol = 1)
          list_for_max_values[[going_edges[i]]] = apply(cbind(eval, list_for_max_values[[going_edges[i]]]) , 1, max)
        }
      }
      
      # coming
      coming_edges = which(vertex_matrix[,2] %in% vertices[1])
      if(length(coming_edges) > 0){
        for (i in 1:length(coming_edges)) {
          center_coming = as.numeric(edge_length[coming_edges[i]]) + center_sign
          idx = which(data_on_graph_red$edge_number == coming_edges[i])
          list_for_indices[[coming_edges[i]]] = idx
          aux = data_on_graph_red$distance_on_edge[idx]*as.numeric(edge_length[coming_edges[i]])
          eval = matrix(fun(aux - center_coming), ncol = 1)
          list_for_max_values[[coming_edges[i]]] = apply(cbind(eval, list_for_max_values[[coming_edges[i]]]) , 1, max)
        }
      }
      
    }
    if(center_sign >= (original_distance- borderline)){
      # going
      going_edges = which(vertex_matrix[,1] %in% vertices[2])
      if(length(going_edges) > 0){
        for (i in 1:length(going_edges)) {
          center_going = -(original_distance - center_sign)
          idx = which(data_on_graph_red$edge_number == going_edges[i])
          list_for_indices[[going_edges[i]]] = idx
          aux = data_on_graph_red$distance_on_edge[idx]*as.numeric(edge_length[going_edges[i]])
          eval = matrix(fun(aux - center_going), ncol = 1)
          list_for_max_values[[going_edges[i]]] = apply(cbind(eval, list_for_max_values[[going_edges[i]]]) , 1, max)
        }
      }
      # coming
      coming_edges = which(vertex_matrix[,2] %in% vertices[2])
      if(length(coming_edges) > 0){
        for (i in 1:length(coming_edges)) {
          center_coming = as.numeric(edge_length[coming_edges[i]]) + (original_distance - center_sign)
          idx = which(data_on_graph_red$edge_number == coming_edges[i])
          list_for_indices[[coming_edges[i]]] = idx
          aux = data_on_graph_red$distance_on_edge[idx]*as.numeric(edge_length[coming_edges[i]])
          eval = matrix(fun(aux - center_coming), ncol = 1)
          list_for_max_values[[coming_edges[i]]] = apply(cbind(eval, list_for_max_values[[coming_edges[i]]]) , 1, max)
        } # end for
      }# end if
    } # end else
  }
  
  
  #----------------------------------------------------------------------------------------------------
  # we compute covariate at the points on each edge just taking into account only signs in the same edge
  for (k in 1:nrow(sign_data)) {
    # getting the sign at iteration i
    sign = sign_data[k,]
    # getting the edge that contains the sign
    edge_number = sign[[1]]
    # storing the edge_number into edges vector
    edges[k] = edge_number
    # getting the distance on the edge of the sign
    distance_on_edge = sign[[2]]
    
    # indices of the data that has edge numbers equal to edge_number (in practice, getting the identity of the point the edge with number edge_number)
    idx = which(data_on_graph_red$edge_number == edge_number)
    list_for_indices[[edge_number]] = cbind(idx, list_for_indices[[edge_number]]) # to repeat the columns
    # getting the distance of the points in the edge of interest
    aux = data_on_graph_red$distance_on_edge[idx]
    # computing the value of the covariate (with normalized and no normalized distance)
    eval = matrix(fun(as.numeric(edge_length[edge_number])*(aux - distance_on_edge)), ncol = 1)
    list_for_max_values[[edge_number]] = apply(cbind(eval, list_for_max_values[[edge_number]]) , 1, max)
  }
  
  list_for_indices[[(sf_graph$nE + 1)]] = NULL
  list_for_max_values[[(sf_graph$nE + 1)]] = NULL
  
  # drop NULL members
  filtered_max_values = list_for_max_values[!sapply(list_for_max_values, is.null)]
  filtered_indices = list_for_indices[!sapply(list_for_indices, is.null)]
  
  new_filtered_max_values = filtered_max_values[sapply(filtered_max_values, function(x) length(x) > 0)]
  new_filtered_indices = filtered_indices[sapply(filtered_indices, function(x) length(x) > 0)]
  
  
  # Creating number of signs covariate ----------------------------------------------------------
  
  cov_number_sign = rep(0, nrow(data_on_graph_red))
  
  for (i in 1:length(new_filtered_indices)) {
    if(length(dim(new_filtered_indices[[i]])) == 2){ # that is, if it is a matrix
      cov_number_sign[new_filtered_indices[[i]][,1]] = ncol(new_filtered_indices[[i]])
      new_filtered_indices[[i]] = as.vector(new_filtered_indices[[i]][,1]) # just getting one column
    }
  }
  
  # -------------------------------------------------------------------------------------------------
  
  covariate_alltogether = data.frame(all_indices = unlist(new_filtered_indices),
                                     all_max_values = unlist(new_filtered_max_values))
  
  
  
  final_cov = rep(0, nrow(data_on_graph_red))
  final_cov[covariate_alltogether$all_indices] = covariate_alltogether$all_max_values
  
  final3 = data_on_graph_red %>% mutate(final_cov = final_cov, cov_number_sign = cov_number_sign)
  
  
  
  # ------------------------------------------------------------------------------------------------------------------
  # For crossing
  # ------------------------------------------------------------------------------------------------------------------
  
  
  # getting the sign_data (this could be bus_stop, traffic_signals, stop or crossing)
  
  sign_data = traffic_stops %>% 
    filter(highway == "crossing") %>% 
    as.data.frame() %>% 
    dplyr::select(.edge_number, .distance_on_edge) %>%
    rename(edge_number = .edge_number, distance_on_edge = .distance_on_edge)
  
  # initializing a vector to store the edges numbers from sign_data (they are not necessarily unique)
  edges = c()
  
  borderline = 0.05
  # looping over the signs
  
  
  list_for_indices = list()
  list_for_indices[[(sf_graph$nE + 1)]] = "tmp1"
  list_for_max_values = list()
  list_for_max_values[[(sf_graph$nE + 1)]] = "tmp2"
  
  # ----------------------------------------------------------------------------------------------
  
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
    
    if(center_sign <= borderline){
      # going
      going_edges = which(vertex_matrix[,1] %in% vertices[1])
      if(length(going_edges) > 0){
        for (i in 1:length(going_edges)) {
          center_going = -center_sign
          idx = which(data_on_graph_red$edge_number == going_edges[i])
          list_for_indices[[going_edges[i]]] = idx
          aux = data_on_graph_red$distance_on_edge[idx]*as.numeric(edge_length[going_edges[i]])
          eval = matrix(fun(aux - center_going), ncol = 1)
          list_for_max_values[[going_edges[i]]] = apply(cbind(eval, list_for_max_values[[going_edges[i]]]) , 1, max)
        }
      }
      
      # coming
      coming_edges = which(vertex_matrix[,2] %in% vertices[1])
      if(length(coming_edges) > 0){
        for (i in 1:length(coming_edges)) {
          center_coming = as.numeric(edge_length[coming_edges[i]]) + center_sign
          idx = which(data_on_graph_red$edge_number == coming_edges[i])
          list_for_indices[[coming_edges[i]]] = idx
          aux = data_on_graph_red$distance_on_edge[idx]*as.numeric(edge_length[coming_edges[i]])
          eval = matrix(fun(aux - center_coming), ncol = 1)
          list_for_max_values[[coming_edges[i]]] = apply(cbind(eval, list_for_max_values[[coming_edges[i]]]) , 1, max)
        }
      }
      
    }
    if(center_sign >= (original_distance- borderline)){
      # going
      going_edges = which(vertex_matrix[,1] %in% vertices[2])
      if(length(going_edges) > 0){
        for (i in 1:length(going_edges)) {
          center_going = -(original_distance - center_sign)
          idx = which(data_on_graph_red$edge_number == going_edges[i])
          list_for_indices[[going_edges[i]]] = idx
          aux = data_on_graph_red$distance_on_edge[idx]*as.numeric(edge_length[going_edges[i]])
          eval = matrix(fun(aux - center_going), ncol = 1)
          list_for_max_values[[going_edges[i]]] = apply(cbind(eval, list_for_max_values[[going_edges[i]]]) , 1, max)
        }
      }
      # coming
      coming_edges = which(vertex_matrix[,2] %in% vertices[2])
      if(length(coming_edges) > 0){
        for (i in 1:length(coming_edges)) {
          center_coming = as.numeric(edge_length[coming_edges[i]]) + (original_distance - center_sign)
          idx = which(data_on_graph_red$edge_number == coming_edges[i])
          list_for_indices[[coming_edges[i]]] = idx
          aux = data_on_graph_red$distance_on_edge[idx]*as.numeric(edge_length[coming_edges[i]])
          eval = matrix(fun(aux - center_coming), ncol = 1)
          list_for_max_values[[coming_edges[i]]] = apply(cbind(eval, list_for_max_values[[coming_edges[i]]]) , 1, max)
        } # end for
      }# end if
    } # end else
  }
  
  
  #----------------------------------------------------------------------------------------------------
  # we compute covariate at the points on each edge just taking into account only signs in the same edge
  for (k in 1:nrow(sign_data)) {
    # getting the sign at iteration i
    sign = sign_data[k,]
    # getting the edge that contains the sign
    edge_number = sign[[1]]
    # storing the edge_number into edges vector
    edges[k] = edge_number
    # getting the distance on the edge of the sign
    distance_on_edge = sign[[2]]
    
    # indices of the data that has edge numbers equal to edge_number (in practice, getting the identity of the point the edge with number edge_number)
    idx = which(data_on_graph_red$edge_number == edge_number)
    list_for_indices[[edge_number]] = cbind(idx, list_for_indices[[edge_number]]) # to repeat the columns
    # getting the distance of the points in the edge of interest
    aux = data_on_graph_red$distance_on_edge[idx]
    # computing the value of the covariate (with normalized and no normalized distance)
    eval = matrix(fun(as.numeric(edge_length[edge_number])*(aux - distance_on_edge)), ncol = 1)
    list_for_max_values[[edge_number]] = apply(cbind(eval, list_for_max_values[[edge_number]]) , 1, max)
  }
  
  list_for_indices[[(sf_graph$nE + 1)]] = NULL
  list_for_max_values[[(sf_graph$nE + 1)]] = NULL
  
  # drop NULL members
  filtered_max_values = list_for_max_values[!sapply(list_for_max_values, is.null)]
  filtered_indices = list_for_indices[!sapply(list_for_indices, is.null)]
  
  new_filtered_max_values = filtered_max_values[sapply(filtered_max_values, function(x) length(x) > 0)]
  new_filtered_indices = filtered_indices[sapply(filtered_indices, function(x) length(x) > 0)]
  
  
  # Creating number of signs covariate ----------------------------------------------------------
  
  cov_number_sign = rep(0, nrow(data_on_graph_red))
  
  for (i in 1:length(new_filtered_indices)) {
    if(length(dim(new_filtered_indices[[i]])) == 2){ # that is, if it is a matrix
      cov_number_sign[new_filtered_indices[[i]][,1]] = ncol(new_filtered_indices[[i]])
      new_filtered_indices[[i]] = as.vector(new_filtered_indices[[i]][,1]) # just getting one column
    }
  }
  
  # -------------------------------------------------------------------------------------------------
  
  covariate_alltogether = data.frame(all_indices = unlist(new_filtered_indices),
                                     all_max_values = unlist(new_filtered_max_values))
  
  
  
  final_cov = rep(0, nrow(data_on_graph_red))
  final_cov[covariate_alltogether$all_indices] = covariate_alltogether$all_max_values
  
  final4 = data_on_graph_red %>% mutate(final_cov = final_cov, cov_number_sign = cov_number_sign)
  
  cov_obs <- data_on_graph[[".distance_on_edge"]]
  cov_obs <- 2 * ifelse(cov_obs > 0.5,  1 - cov_obs, cov_obs)
  
  data_on_mesh_with_covariates = data_on_graph %>% mutate(bus = final1$final_cov,
                                                          bus_number = final1$cov_number_sign,
                                                          signal = final2$final_cov,
                                                          signal_number = final2$cov_number_sign,
                                                          stop = final3$final_cov,
                                                          stop_number = final3$cov_number_sign,
                                                          crossing = final4$final_cov,
                                                          crossing_number = final4$cov_number_sign,
                                                          cov_obs = cov_obs)
  
  
  save(data_on_mesh_with_covariates, file = "Data_files/data_on_mesh_with_covariates_partialtomtom.RData")
  
  
}


