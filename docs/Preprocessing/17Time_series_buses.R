library(MetricGraph)
library(dplyr)
library(TSstudio)
library(ggplot2)
library(gganimate)
library(tsbox)
# loading the data
load("~/Desktop/Spring 2024/january_with_ID.RData")


# just filtering
aux = january %>%
  filter(day %in% c(7)) %>% # every Thursday of January 2021
  distinct(geometry, .keep_all = TRUE) # to remove observations with the same location

# let us get a specific bus
IDnumber = 6697 #sample(unique(aux$ID), 1, replace = FALSE)
df = aux %>% 
  filter(ID == IDnumber) %>% 
  dplyr::select(datetime, speed) %>% 
  #sf::st_drop_geometry() %>%
  arrange(datetime) %>%
  mutate(time_diff1 = as.numeric(c(0,diff(datetime))/60), time_diff = as.numeric(c(diff(datetime),0)/60)) %>%
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

df  = df %>% dplyr::select(-time_diff)

TSstudio::ts_plot(df,
        line.mode = "lines+markers",
        title = paste("Speed records, day 7 January, bus ID: ", IDnumber, sep = ""),
        Xtitle = "Time",
        Ytitle = "Speed",
        type = "multiple")

ggplot() + 
  geom_sf(data = df, aes(color = ifelse(time_since_zero == 0, "Zero", "Non-zero"))) +
  scale_color_manual(values = c("Zero" = "gray", "Non-zero" = "red")) +
  theme_minimal()

ggplot() +
  geom_sf(data = df, aes(color = speed)) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_sf(data = df[df$speed == 0, ], color = "red", size = 3) +
  theme_minimal()

ggplot() +
  geom_sf(data = df, aes(color = ifelse(speed == 0, "Zero", "Non-zero"))) +
  scale_color_manual(values = c("Zero" = "red", "Non-zero" = "gray")) +
  theme_minimal()

mapview::mapview(df %>% filter(time_since_zero > 9), zcol = "time_since_zero")

mapview::mapview(df %>% filter(time_since_zero > 0), zcol = "time_since_zero")

mapview::mapview(df %>% filter(time_since_zero > 0), zcol = "time_since_zero")

mapview::mapview(df[1:13,], zcol = "speed")

ppp = df %>% filter(time_since_zero > 9)

