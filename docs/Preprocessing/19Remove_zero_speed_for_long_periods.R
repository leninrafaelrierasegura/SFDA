library(MetricGraph)
library(dplyr)
library(TSstudio)
library(ggplot2)
library(gganimate)
library(tsbox)


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

# loading the data
load("~/Desktop/Spring 2024/january_with_ID.RData")


# just filtering
aux = january %>%
  filter(day %in% c(7,14,21,28), hour %in% c(13,14)) %>% # every Thursday of January 2021
  mutate(day = day/7) %>%
  dplyr::select(-PDT, -hour) 
  #distinct(geometry, .keep_all = TRUE) # to remove observations with the same location

buses_ID = unique(aux$ID)
days = 1:4

df = aux %>% 
  filter(ID == buses_ID[1], day == 1) %>% 
  arrange(datetime) %>%
  mutate(speed = remove_consecutive_zeros(speed)) %>%
  drop_na(speed)


for (i in 2:length(buses_ID)) {
  for (j in 1:length(days)) {
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

save(df, file = "Data_files/data_day7142128_hour13and14_with_no_consecutive_zeros.RData")



