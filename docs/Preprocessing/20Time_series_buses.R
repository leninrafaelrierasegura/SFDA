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
  filter(day %in% c(7,14,21,28), hour == 13) %>% # every Thursday of January 2021
  mutate(day = day/7) %>%
  dplyr::select(-PDT, -hour) 
  #distinct(geometry, .keep_all = TRUE) # to remove observations with the same location



# let us get a specific bus
IDnumber = 6697 #sample(unique(aux$ID), 1, replace = FALSE)
df = aux %>% 
  filter(ID == IDnumber, day %in% c(1)) %>% 
  arrange(datetime) %>%
  mutate(speed = remove_consecutive_zeros(speed)) %>%
  drop_na(speed) %>%
  mutate(time_diff = as.numeric(c(diff(datetime),0)/60)) %>%
  mutate(time_since_zero = 0)






# Initialize variables to keep track of time since last zero speed observation
time_since_zero <- 0

# Iterate over rows in the data frame
for (i in 1:nrow(df)) {
  # If speed is zero, reset time_since_zero
  if (df$speed[i] == 0) {
    time_since_zero <- time_since_zero + df$time_diff[i]
  } else {
    # Increment time_since_zero by the time difference for non-zero speed observations
    time_since_zero <- 0
  }
  
  # Update the 'time_since_zero' column in the data frame
  df$time_since_zero[i] <- time_since_zero
}

df  = df %>% dplyr::select(-time_diff,-day,-ID)

TSstudio::ts_plot(df,
        line.mode = "lines+markers",
        title = paste("Speed records, day 7 January, bus ID: ", IDnumber, sep = ""),
        Xtitle = "Time",
        Ytitle = "Speed",
        type = "multiple")










