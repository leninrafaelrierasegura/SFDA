library(dplyr)
library(tidyr)

load("Data_files/weights_quantile.RData")
load("Networks object/speed_limit_with_close_dist.RData") # contains final_data


tmp = final_data[1:12855, ] %>% 
  mutate(speedlimit = weights_quantile$speedlimit, road_type = weights_quantile$road_type) %>%
  dplyr::select(speedlimit, road_type) %>%
  mutate(speedlimit = as.integer(speedlimit)) %>% 
  rename(highway = road_type) %>%
  mutate(value = speedlimit, road_type = highway) %>%
  pivot_wider(names_from = highway, values_from = value, values_fill = list(value = 0))

weights_quantile_corrected = data.frame(speedlimit = tmp$speedlimit,
                                  road_type = tmp$road_type,
                                  motorway = tmp$motorway,
                                  residential = tmp$residential,
                                  motorway_link = tmp$motorway_link,
                                  primary = tmp$primary,
                                  secondary = tmp$secondary,
                                  trunk = tmp$trunk,
                                  tertiary = tmp$tertiary,
                                  unclassified = tmp$unclassified)
save(weights_quantile_corrected, file = "Data_files/weights_quantile_corrected.RData")