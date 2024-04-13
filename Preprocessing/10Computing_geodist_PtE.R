load("Graph_objects/graph_construction_01_03_2024.RData")
load("Data_files/Data_in_graph_coordinates_for_every7days/data_on_graph_hour_13.RData")
load("Data_files/stops_data_on_graph.RData")

# length of each edge
edge_length = sf_graph$get_edge_lengths()

# matrix of vertices, the vertices in the i-th are the vertices that define edge i
vertex_matrix = sf_graph$E 
  
bus_stop = traffic_stops %>% 
  filter(highway == "bus_stop") %>% 
  as.data.frame() %>% 
  dplyr::select(.edge_number, .distance_on_edge) %>%
  rename(edge_number = .edge_number, distance_on_edge = .distance_on_edge)

data_on_graph_column45 = data.frame(edge_number = data_on_graph$.edge_number, 
                                    distance_on_edge = data_on_graph$.distance_on_edge)



dist_to_bus_cov = matrix(NA, nrow = nrow(data_on_graph), ncol = 2)

fun = function(x){
  return(exp(-((x/0.02)^2)))
  
}


for (i in 1:nrow(data_on_graph)) {
  print(i)
  # slicing i-th row of data
  point = data_on_graph_column45[i,] # 4,5 is to get the 4 and 5 column (that is, .edge_number and .distance_on_edge)
  # getting the edge that contains the data point
  edge_number = point[[1]]
  # getting the distance on the edge of the data point 
  distance_on_edge = point[[2]]
  # getting the vertices corresponding to the edge of interest
  vertices = vertex_matrix[edge_number,]
  # getting all the neighboring edges of the edge of interest
  row1 = which(vertex_matrix[,1] %in% vertices)
  row2 = which(vertex_matrix[,2] %in% vertices)
  neighboring_edges = unique(c(row1, row2))
  # indices of neighboring edges in bus stop
  idx = which(bus_stop$edge_number %in% neighboring_edges)
  if(length(idx) == 0){
    dist_to_bus_cov[i] = 0
  }else {
  aux = bus_stop[idx,]
  PtE = rbind(point, aux)
  geodist = sf_graph$compute_geodist_PtE(
    PtE = PtE,
    normalized = TRUE,
    include_vertices = FALSE
  )
  dist_to_bus_cov[i,1] = min(geodist[1, 2:(length(idx)+1)])
  dist_to_bus_cov[i,2] = distance_on_edge*edge_length[edge_number]
  }
}

# to save
save(dist_to_bus_cov, file = "Data_files/geodist_bust_stop.RData")
