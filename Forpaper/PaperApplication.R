################################################################################
######################Application of the paper##################################
################################################################################

# Load libraries -----------------------------------------------------------

# Uncomment to update

# inla.upgrade(testing = TRUE) # Run library(INLA) first
# remotes::install_github("inlabru-org/inlabru", ref = "devel")
# remotes::install_github("davidbolin/rspde", ref = "devel")
# remotes::install_github("davidbolin/metricgraph", ref = "devel")
# remotes::install_github("davidbolin/ngme2", ref = "devel")

library(INLA)
# inla.setOption(num.threads = 6) # Set the numbers of cores to be used by INLA
library(inlabru)
library(rSPDE)
library(MetricGraph)
library(ngme2)

library(plotly)
library(dplyr)

library(sf)

library(here) # Make use of here() function to make the code more portable

#-------------------------------------------------------------------------------

rm(list = ls()) # Clear the workspace
set.seed(1982) # Set seed for reproducibility

# Preprocess the data ----------------------------------------------------------

# Bus numbers according to Wikipedia
busesnumber =  c(8501:8530, 
                 8531:8560, 
                 8601:8662, 
                 8701:8750, 
                 8751:8780, 
                 8800:8969, 
                 6500:6554, 
                 6560:6697, 
                 6700:6730, 
                 5701:5885, 
                 7201:7293)


# Read the raw data (called "Speed_Observations_SF.csv" and downloaded from https://data.sfgov.org/Transportation/SFMTA-Transit-Vehicle-Location-History-Current-Yea/x344-v6h6/explore/query/SELECT%0A%20%20%60vehicle_position_date_time%60%2C%0A%20%20%60vehicle_id%60%2C%0A%20%20%60loc_x%60%2C%0A%20%20%60loc_y%60%2C%0A%20%20%60heading%60%2C%0A%20%20%60average_speed%60%0AWHERE%0A%20%20%60vehicle_position_date_time%60%0A%20%20%20%20BETWEEN%20%222021-01-01T15%3A18%3A52%22%20%3A%3A%20floating_timestamp%0A%20%20%20%20AND%20%222021-01-31T15%3A18%3A52%22%20%3A%3A%20floating_timestamp%0AORDER%20BY%20%60vehicle_position_date_time%60%20DESC%20NULL%20FIRST/page/filter)
raw = read.csv(here("Speed_Observations_SF.csv"))

# Filter and prepare data
# Get PDT, ID, speed, datetime, day, hour variables
january =  raw %>% 
  filter(Vehicle.ID %in% busesnumber) %>% # Filter by bus numbers
  filter(Latitude > 37.7, Latitude < 37.815, Longitude > -122.52, Longitude < -122.36) %>% # Filter by the area of interest
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%  # Transform to an sf object
  mutate(datetime = strptime(Position.Date.Time, format="%m/%d/%Y %I:%M:%S %p")) %>%  # Create datetime variable with format so we can manipulate later
  mutate(day = as.integer(format(datetime, format = "%d")), hour = as.integer(format(datetime, format = "%H"))) %>% # Create day and hour variables
  filter(Average.Speed < 73) %>% # Remove some atypical speed observations. 73 because we are allowing 10kph above the limit
  mutate(Average.Speed = Average.Speed*1.60934) %>% # Transform from mph to kph
  select(-Heading) %>% # Remove Heading variable
  rename(speed = Average.Speed, ID = Vehicle.ID, PDT = Position.Date.Time) # Rename Average.Speed to speed, Vehicle.ID to ID, Position.Date.Time to PDT, so it is easier to write

# Save the data
save(january, file = here("january_with_ID.RData"))

# Remove more than one consecutive zeros from the speed variable ---------------

# Load the data
load(here("january_with_ID.RData"))

# Function to remove consecutive zeros
remove_consecutive_zeros <- function(vec) {
  # Initialize a result vector
  result <- numeric(length(vec))
  # Index for the result vector
  index <- 1
  # Flag to track if the first zero has been encountered
  first_zero <- FALSE
  # Loop through the original vector
  for (i in 1:length(vec)) {
    # If current value is not zero or previous value is not zero, or it's the first zero, add it to result
    if (vec[i] != 0 || (i > 1 && vec[i - 1] != 0) || i == 1) {
      result[index] <- vec[i]
      index <- index + 1
      # Reset first_zero flag if it's the first zero
      if (vec[i] == 0 && !first_zero) {
        first_zero <- TRUE
      }
    } else {
      # Replace consecutive zeros with NA after the first zero
      result[index] <- NA
      index <- index + 1
    }
  }
  # Trim the result vector to remove unused entries
  result <- result[1:(index - 1)]
  return(result)
}

# Choose the days for the analysis
days = c(7,14,21,28) # every Thursday of January 2021

# Filter by days and hour of interest
aux = january %>%
  filter(day %in% days, hour %in% c(13)) %>% # Keep observations between 13:00 and 14:00
  dplyr::select(-PDT, -hour) # Remove PDT and hour variables

# Get the unique buses ID
buses_ID = unique(aux$ID)

# Remove more than one consecutive zeros from the speed variable
df = aux %>% 
  filter(ID == buses_ID[1], day == days[1]) %>% 
  arrange(datetime) %>%
  mutate(speed = remove_consecutive_zeros(speed)) %>%
  drop_na(speed)

for (i in 1:length(buses_ID)) {
  for (j in 1:length(days)) {
    if (i == 1 && j == 1) {
      next
    }
    check =  aux %>% filter(ID == buses_ID[i], day == days[j])
    if(nrow(check) > 0){
      tmp = check %>% 
        arrange(datetime) %>%
        mutate(speed = remove_consecutive_zeros(speed)) %>%
        drop_na(speed)
      df = rbind(tmp, df)
    }
  }
}

newdays = 1:4
df$day <- newdays[match(df$day, days)]


save(df, file = here("Data_files/data_day8152229_hour9_with_no_consecutive_zeros.RData"))