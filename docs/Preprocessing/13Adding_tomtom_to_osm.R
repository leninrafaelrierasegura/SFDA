library(mapview)
library(ggplot2)
library(plotly)
library(sf)

# loading 
load("Data_files/tomtom.RData")
load("Networks object/speed_limit_with_close_dist.RData")


#
from_tomtom = tomtom %>%
  dplyr::select(-Id, -Segment.Id, -NewSegId, -timeSet, -dateRange, -standardDeviationSpeed, -travelTimeStandardDeviation) %>%
  filter(FRC != "7") %>% # to remove tomtom class 7 
  mutate(value = SpeedLimit, road_type = paste("class_", FRC, sep = ""), aux = paste("class_", FRC, sep = "")) %>%
  pivot_wider(names_from = aux, values_from = value, values_fill = list(value = 0)) %>%
  mutate(upto1 = class_0 + class_1) %>%
  mutate(upto3 = upto1 + class_3) %>%
  mutate(upto4 = upto3 + class_4) %>%
  mutate(upto5 = upto4 + class_5) %>%
  mutate(upto6 = upto5 + class_6) %>%
  mutate(Length  = Length/1000) %>%  # in km
  mutate(density = sampleSize/Length) %>% # density whole day
  mutate(density_per_hour = density/24) %>% # density per hour
  mutate(density_per_hour_normalized = density_per_hour/max(density_per_hour)) # normalized density



tomtom_network = st_transform(from_tomtom, crs = st_crs(final_data))

final_data_tomtom = st_join(final_data, tomtom_network, join = st_nearest_feature) %>% 
  dplyr::select(-maxspeed) %>%
  mutate(highway = as.factor(highway)) %>%
  mutate(value = SpeedLimit, aux = highway) %>%
  pivot_wider(names_from = aux, values_from = value, values_fill = list(value = 0))

save(final_data_tomtom, file = "Networks object/speed_limit_with_tomtom.RData")
