library(ggplot2)
library(viridis)

# Extracting coordinates from the SF object
coords <- st_coordinates(from.tomtom)
min_x <- min(coords[, "X"]) |> round(2)
max_x <- max(coords[, "X"]) |> round(2)
min_y <- min(coords[, "Y"]) |> round(2)
max_y <- max(coords[, "Y"]) |> round(2)
# Define labels for each group
group_labels <- c("Feb 12", "Feb 14", "Feb 23", "Feb 27")

# Create a base plot with road network
base_plot <- ggplot() +
  geom_sf(data = from.tomtom, aes(color = "Highways"), color = "black", size = 0.1) +
  scale_x_continuous(labels = function(x) paste0(x), name = "Longitude", breaks = seq(min_x, max_x, by = 0.04)) +  # Adjust x-axis label and breaks
  scale_y_continuous(labels = function(y) paste0(y), name = "Latitude", breaks = seq(min_y, max_y, by = 0.04))

# Create individual plots for each group and overlay data points
group_plots <- lapply(unique(datarep$.group), function(gr) {
  geom_point(data = subset(datarep, .group == gr), aes(x = .coord_x, y = .coord_y, color = speed), size = 0.5)  # Adjust x_column and y_column
})

# Combine the base plot and the group plots
combined_plot <- base_plot + 
  facet_wrap(~ .group, labeller = labeller(.group = function(variable, value) {
    return(group_labels[value])
  })) +  # Specify custom labels for facet_wrap
  group_plots +
  scale_color_gradient(name = "Speed") +
  labs(color = "Speed") +
  scale_color_viridis_c(option = "D")

# Print the combined plot
print(combined_plot)
