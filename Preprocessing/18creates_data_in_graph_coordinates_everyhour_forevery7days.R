library(MetricGraph)
library(dplyr)

# loading the data
load("Data_files/january.RData")



# day goes from 1 to 31
# hour goes from 0 to 23

for (i in 0:23) {
  print(i)
  # loading the graph (it does not contain data)
  load("Graph_objects/graph_construction_01_03_2024.RData")
  
  tryCatch({
    
    # just filtering
    aux = january %>%
      filter(day %in% c(7,14,21,28), hour == i) %>% # every thursday of January 2021
      mutate(day = day/7) %>% # so that groups are 1,2,3,4
      dplyr::select(-hour) %>%
      distinct(geometry, .keep_all = TRUE) # to remove observations with the same location
    
    # adding observations to the graph
    sf_graph$add_observations(data = aux, group = "day", tolerance = 0.04, duplicated_strategy = "jitter")
    
    # getting the data from the graph (that is, in graph coordinates)
    data_on_graph = sf_graph$get_data()
    
    save(data_on_graph, file = paste("Data_files/Data_in_graph_coordinates_for_every7days/data_on_graph_hour_", i, ".RData", sep = ""))
    
    }, error = function(err){
      warning(paste("i=", i, "Error:", conditionMessage(err)))
     print(paste("i=", i, "Error:", conditionMessage(err)))
    })
    
    rm(sf_graph)
}


