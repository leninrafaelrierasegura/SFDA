# This R script reads the raw data and produces a data frame with observations corresponding to buses.

library(dplyr)
library(sf)

# bus numbers according to Wikipedia
busesnumber =  c(8501:8530, 8531:8560, 8601:8662, 8701:8750, 8751:8780, 
                 8800:8969, 6500:6554, 6560:6697, 6700:6730, 5701:5885, 7201:7293)


# read the raw data
raw = read.csv("Data_files/Speed_Observations_SF.csv")

# get just day, hour, and speed in kph
january =  raw %>% 
  filter(Vehicle.ID %in% busesnumber) %>% # to choose only buses
  filter(Latitude > 37.7, Latitude < 37.815, Longitude > -122.52, Longitude < -122.36) %>% # to remove some weird location in front of Gabon in Africa
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%  # to transform it as an sf object
  mutate(datetime = strptime(Position.Date.Time, format="%m/%d/%Y %I:%M:%S %p")) %>%  # to format so we can manipulate later
  mutate(day = as.integer(format(datetime, format = "%d")), hour = as.integer(format(datetime, format = "%H"))) %>% # to get day and hour
  filter(Average.Speed < 73) %>% # to remove some atypical speed observations. 73 because we are allowing 10kph above the limit
  mutate(Average.Speed = Average.Speed*1.60934) %>% # to transform from mph to kph
  select(-Position.Date.Time, -Heading, -datetime, -Vehicle.ID) %>% # to remove variable Position.Date.Time and Heading
  rename(speed = Average.Speed) # to rename Average.Speed to speed so it is easier to write
  

save(january, file = "Data_files/january.RData")

