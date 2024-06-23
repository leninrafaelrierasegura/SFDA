load(here::here("Models_output/Highways23.RData"))
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
# Create data frames
mse_df <- data.frame(
  distance,
  Statnu0.5 = mse.stat,
  Nonstatnu0.5 = mse.nonstat
)

ls_df <- data.frame(
  distance,
  Statnu0.5 = -ls.stat,
  Nonstatnu0.5 = -ls.nonstat
)

# Convert to long format
mse_long <- mse_df %>%
  pivot_longer(cols = -distance, names_to = "nu", values_to = "MSE")

ls_long <- ls_df %>%
  pivot_longer(cols = -distance, names_to = "nu", values_to = "LogScore")


# Update the label mappings with the new legend title
label_mapping <- c(
  "Statnu0.5" = "0.5", 
  "Nonstatnu0.5" = "0.5*"
)

# Define color and linetype mapping
color_mapping <- c(
  "Statnu0.5" = "blue", 
  "Nonstatnu0.5" = "red"
)

linetype_mapping <- c(
  "Statnu0.5" = "dashed", 
  "Nonstatnu0.5" = "solid"
)

# Plot MSE
mse_plot <- ggplot(mse_long, aes(x = distance, y = MSE, color = nu, linetype = nu)) +
  geom_line(size = 1) +
  labs(y = "MSE", x = "Distance in km") +
  scale_color_manual(values = color_mapping, labels = label_mapping, name = expression(paste("Smoothness parameter ", nu))) +
  scale_linetype_manual(values = linetype_mapping, labels = label_mapping, name = expression(paste("Smoothness parameter ", nu))) +
  theme_minimal() +
  theme(text = element_text(family = "Palatino"))

# Plot negative log-score
ls_plot <- ggplot(ls_long, aes(x = distance, y = LogScore, color = nu, linetype = nu)) +
  geom_line(size = 1) +
  labs(y = "Negative Log-Score", x = "Distance in km") +
  scale_color_manual(values = color_mapping, labels = label_mapping, name = expression(paste("Smoothness parameter ", nu))) +
  scale_linetype_manual(values = linetype_mapping, labels = label_mapping, name = expression(paste("Smoothness parameter ", nu))) +
  theme_minimal() +
  theme(text = element_text(family = "Palatino"))

# Combine plots with a shared legend at the top in a single line
combined_plot <- mse_plot + ls_plot + 
  plot_layout(guides = 'collect') & 
  theme(legend.position = 'top') & 
  guides(color = guide_legend(nrow = 1), linetype = guide_legend(nrow = 1))

# Display combined plot
print(combined_plot)


# Save the plot in the best possible quality
ggsave(here("Forpaper/nonstat.png"), plot = combined_plot, dpi = 300)
