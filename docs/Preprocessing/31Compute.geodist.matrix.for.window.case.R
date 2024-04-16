library(dplyr)
library(here)

# loading the data
load(here("Data_files/data_on_graph_with_covariates_no_consecutive_zeros_partialtomtom.RData"))
load(here("Graph_objects/graph_construction_28_03_2024partialtomtomwhichlonglatsf.RData"))

aux = data_on_graph_with_covariates |>
  rename(distance_on_edge = .distance_on_edge, edge_number = .edge_number) |>
  as.data.frame() |>
  dplyr::select(edge_number, distance_on_edge)
# 
# distmatrix = sf_graph$compute_geodist_PtE(PtE = aux, 
#                                        normalized = TRUE, 
#                                        include_vertices = FALSE)
# 
# 
# save(distmatrix, file = here("Models_output/distmatrix.RData"))

# Warning message:
#   In system.time({ :
#       Duplicated locations were found when computing geodist. The returned values are given for unique locations.
#     
    
load(here("Models_output/distmatrix.RData"))   
nrow(unique(aux))
