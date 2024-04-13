#set.seed(2)

# graph creation
edge1 <- rbind(c(0,0),c(3,0))
edge2 <- rbind(c(0,0),c(0,1))
edge3 <- rbind(c(0,1),c(-1,1))
theta <- seq(from=pi,to=3*pi/2,length.out = 20)
edge4 <- cbind(sin(theta),1+ cos(theta))
edges = list(edge1, edge2, edge3, edge4)
graph <- metric_graph$new(edges = edges)

range <- 0.2
sigma <- 1.3
sigma_e <- 0.1
alpha <- 1

n.obs.per.edge <- 300
PtE <- NULL
for(i in 1:graph$nE){
  #add locations sampled at random to each edge
  PtE <- rbind(PtE, cbind(rep(i, n.obs.per.edge), runif(n.obs.per.edge)))
}

u <- sample_spde(range = range, sigma = sigma, alpha = alpha,
                 graph = graph, PtE = PtE)

y <- u + sigma_e*rnorm(n.obs.per.edge * graph$nE)

df_data <- data.frame(y = y, edge_number = PtE[,1],
                      distance_on_edge = PtE[,2])

graph$add_observations(data = df_data, normalized = TRUE, clear_obs = TRUE)

data_on_graph = graph$get_data()

data_aux = data_on_graph[sample(2:(nrow(data_on_graph)-1), 15), ] %>% dplyr::select(y, .edge_number, .distance_on_edge)
sign_data = data_aux %>% 
  as.data.frame() %>% 
  dplyr::select(.edge_number, .distance_on_edge) %>% 
  rename(edge_number = .edge_number, distance_on_edge = .distance_on_edge)

#graph$plot(data = "y", newdata = data_aux, vertex_size = 0)


# length of each edge
edge_length = graph$get_edge_lengths()

# matrix of vertices, the vertices in the i-th are the vertices that define edge i
vertex_matrix = graph$E 


# getting the edge_number and distance_on_edge columns from data
data_on_graph_red = data.frame(edge_number = data_on_graph$.edge_number, 
                               distance_on_edge = data_on_graph$.distance_on_edge)


# defining function that decays from 1
fun = function(x){return(exp(-((x/0.2)^2)))}

# initializing a vector to store the edges numbers from sign_data (they are not necessarily unique)
edges = c()

borderline = 0.5
# looping over the signs


list_for_indices = list()
list_for_indices[[(graph$nE + 1)]] = "tmp1"
list_for_max_values = list()
list_for_max_values[[(graph$nE + 1)]] = "tmp2"

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
  if(center_sign <= borderline || center_sign >= (original_distance- borderline)){
    
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
      
    }else{
      # going
      going_edges = which(vertex_matrix[,1] %in% vertices[2])
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
      coming_edges = which(vertex_matrix[,2] %in% vertices[2])
      if(length(coming_edges) > 0){
        for (i in 1:length(coming_edges)) {
          center_coming = as.numeric(edge_length[coming_edges[i]]) + center_sign
          idx = which(data_on_graph_red$edge_number == coming_edges[i])
          list_for_indices[[coming_edges[i]]] = idx
          aux = data_on_graph_red$distance_on_edge[idx]*as.numeric(edge_length[coming_edges[i]])
          eval = matrix(fun(aux - center_coming), ncol = 1)
          list_for_max_values[[coming_edges[i]]] = apply(cbind(eval, list_for_max_values[[coming_edges[i]]]) , 1, max)
        } # end for
      }# end if
    } # end else
  }# end big if
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

list_for_indices[[(graph$nE + 1)]] = NULL
list_for_max_values[[(graph$nE + 1)]] = NULL

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

final1 = data_on_graph %>% mutate(final_cov = final_cov, cov_number_sign = cov_number_sign)

graph$plot_function(data = "final_cov", newdata = final1, plotly = TRUE, line_width = 2)