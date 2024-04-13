library(MetricGraph)
library(dplyr)
library(tidyr)
library(sf)
library(plotly)
load("Graph_objects/graph_construction_01_03_2024.RData")
load("Data_files/onlybuses.RData")
load("Networks object/traffic_stop.RData")

only1PM = busdataset %>%
      mutate(hour = as.integer(format(date, "%I"))) %>% # to later use filter(hour == 1)
      filter(AM_PM == "PM") %>% # only PM observations # to remove AM observations
      filter(hour == 1) %>% # to select observations between 1pm to 2pm
      dplyr::select(-date, -AM_PM, -hour, -Vehicle.ID) # to remove useless variables
only1PMevery7 = only1PM %>%
      filter(day %in% c(7,14,21,28)) %>% # to select these 4 days
      filter(Average.Speed < 73) %>%
      mutate(Average.Speed=Average.Speed*1.60934)%>% # to remove some atypical speed observations
      distinct(geometry, .keep_all = TRUE) %>% # to remove observations with repeated location
      mutate(day = day/7) # so that the groups are 1,2,3,4


# Adding the traffic lights and bus stops
tot_obs<-sf_graph$add_observations(data = datapoints,
          tolerance=0.05, clear_obs=TRUE)

traffic_stops = sf_graph$get_data()

rem.obs<-sf_graph$add_observations(data=only1PMevery7, group="day",
          tolerance=0.2, clear_obs=TRUE, duplicated_strategy="jitter")




p = sf_graph$plot(vertex_size=0, data="Average.Speed", add_new_scale_weights = FALSE, data_size=2)
p + geom_point(traffic_stops %>% filter(highway == "traffic_signals"), mapping = aes(x=.coord_x, y=.coord_y), color = "red") + 
geom_point(traffic_stops %>% filter(highway == "bus_stop"), mapping = aes(x=.coord_x, y=.coord_y), color = "blue")





# p <- sf_graph$plot(vertex_size=0, edge_weight = "SpeedLimit", 
# edge_width_weight = "SpeedLimit", edge_width = 2, 
# data="Average.Speed", add_new_scale_weights = FALSE, data_size=2)
# p + geom_point(rem.obs, mapping = aes(x=.coord_x, y=.coord_y), color="red")


# XY = only1PMevery7 %>% 
#        mutate(coord_x = st_coordinates(geometry)[,1],
#         coord_y = st_coordinates(geometry)[,2])

# XY_df = as.matrix(data.frame(coord_x = XY$coord_x, coord_y = XY$coord_y))


# coord<-sf_graph$coordinates(XY = XY_df)



