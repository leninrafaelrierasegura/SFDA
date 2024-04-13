library(ggplot2)
library(plotly)

load("Data_files/onlybuses.RData")
load("Data_files/tomtom.RData")


from_tomtom = tomtom %>% 
  dplyr::select(Length, SpeedLimit, FRC, sampleSize) %>%
  filter(FRC != "6", FRC != "7") %>%
  mutate(value = SpeedLimit, road_type = FRC) %>%
  pivot_wider(names_from = FRC, values_from = value, values_fill = list(value = 0)) 

only1PM = busdataset %>% 
      mutate(hour = as.integer(format(date, "%I"))) %>% # to later use filter(hour == 1)
      filter(AM_PM == "PM") %>% # only PM observations # to remove AM observations
      filter(hour == 1) %>% # to select observations between 1pm to 2pm
      dplyr::select(-date, -AM_PM, -hour, -Vehicle.ID) # to remove useless variables
    
    
only1PMevery7 = only1PM %>% 
      filter(day %in% c(7,14,21,28)) %>% # to select these 4 days
      filter( Average.Speed < 75) %>% # to remove some atypical speed observations
      distinct(geometry, .keep_all = TRUE) %>% # to remove observations with repeated location
      mutate(day = day/7) # so that the groups are 1,2,3,4

(ggplot() +
  geom_sf(data = from_tomtom, color = "black", size = 8) +
  # Add Points
  geom_sf(data = only1PMevery7, color = "blue", size = 0.5)) %>% ggplotly()

    