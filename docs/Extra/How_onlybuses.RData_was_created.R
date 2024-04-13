
# This is how datainitial and SFdatainitial1.RData was created --------------------------------------------------------

datainitial = read.csv("Speed_Observations_SF.csv") %>% 
  filter(Latitude > 37) %>% # to remove some weird location in front of Gabon in Africa
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%  # to transform it as an sf object
  mutate(date = as.POSIXct(Position.Date.Time, format="%m/%d/%Y %I:%M:%S %p")) %>%  # to format so we can manipulate later
  mutate(day = format(date, format="%d"), AM_PM = format(date, format="%p")) %>% # to extract the day and AM or PM
  select(-Position.Date.Time) # %>% # to remove variable Position.Date.Time

save(datainitial, file = "SFdatainitial1.RData")

# This is how busdataset and onlybuses.RData was created ---------------------------------------------------------------


load("SFdatainitial1.RData")

busesnumber =  c(8501:8530, 8531:8560, 8601:8662, 8701:8750, 8751:8780, 8800:8969, 6500:6554, 6560:6697, 6700:6730, 5701:5885, 7201:7293)
busdataset = datainitial %>% 
  filter(Average.Speed > 0) %>% # removing zero speed observations
  mutate(day = as.numeric(day)) %>%
  select(-Heading) %>%  # removing some useless variables
  filter(Vehicle.ID %in% busesnumber)

save(busdataset, file = "onlybuses.RData")