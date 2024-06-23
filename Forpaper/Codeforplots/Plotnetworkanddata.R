library(plotly)
library(dplyr)
library(tidyr)
library(mapview)
library(ggplot2)
library(plotly)
library(sf)

library(here)
library(scales)

load(here("Data_files/tomtom.RData"))
load(here("Data_files/data_day7142128_hour13_with_no_consecutive_zeros.RData")) # just to get the crs

polygon = st_multipoint(c(st_point(c(-122.53000, 37.69702)),
                          st_point(c(-122.37000, 37.69702)),
                          st_point(c(-122.37000, 37.82600)),
                          st_point(c(-122.53000, 37.82600)))) %>%
  st_cast("POLYGON") %>%
  st_sfc(crs = st_crs(df))

from.tomtom = tomtom %>%
  dplyr::select(-Id, -Segment.Id, -NewSegId, -timeSet, -dateRange, -standardDeviationSpeed, -travelTimeStandardDeviation) %>%
  filter(FRC != "7", FRC != "6", FRC != "5") %>% # to remove tomtom class 5, 6 and 7 
  mutate(value = SpeedLimit, road_type = paste("class_", FRC, sep = ""), aux = paste("class_", FRC, sep = "")) %>%
  pivot_wider(names_from = aux, values_from = value, values_fill = list(value = 0)) %>%
  mutate(upto1 = class_0 + class_1) %>%
  mutate(upto3 = upto1 + class_3) %>%
  mutate(upto4 = upto3 + class_4) %>%
  #mutate(upto5 = upto4 + class_5) %>%
  #mutate(upto6 = upto5 + class_6) %>%
  mutate(Length  = Length/1000) %>%  # in km
  mutate(density = sampleSize/Length) %>% # density whole day
  mutate(density_per_hour = density/24) %>% # density per hour
  st_transform(crs = st_crs(df)) %>%
  st_filter(x = ., y = polygon, .predicate = st_within)


# Extracting coordinates from the SF object
coords <- st_coordinates(from.tomtom)
min_x <- min(coords[, "X"]) |> round(2)
max_x <- max(coords[, "X"]) |> round(2)
min_y <- min(coords[, "Y"]) |> round(2)
max_y <- max(coords[, "Y"]) |> round(2)
# Define labels for each group
group_labels <- c("January 7", "January 14", "January 21", "January 28")

# Create a base plot with road network
base_plot <- ggplot() +
  geom_sf(data = from.tomtom, aes(color = "Highways"), color = "black", size = 0.1) +
  scale_x_continuous(labels = function(x) paste0(x), name = "Longitude", breaks = seq(min_x, max_x, by = 0.04)) +  # Adjust x-axis label and breaks
  scale_y_continuous(labels = function(y) paste0(y), name = "Latitude", breaks = seq(min_y, max_y, by = 0.04)) +
  theme_minimal()

# Create individual plots for each group and overlay data points
group_plots <- lapply(unique(datarep$.group), function(gr) {
  geom_point(data = subset(datarep, .group == gr), aes(x = .coord_x, y = .coord_y, color = speed), size = 0.5)  # Adjust x_column and y_column
})

# Combine the base plot and the group plots
combined_plot <- base_plot + 
  facet_wrap(~ .group, ncol = 4, labeller = labeller(.group = function(variable, value) {
    return(group_labels[value])
  })) +  # Specify custom labels for facet_wrap
  group_plots +
  scale_color_gradient(name = "Speed") +
  labs(color = "Speed") +
  scale_color_viridis_c(option = "D")

# Print the combined plot
print(combined_plot)

ggsave(here("Forpaper/rep.png"), plot = combined_plot, dpi = 300)
