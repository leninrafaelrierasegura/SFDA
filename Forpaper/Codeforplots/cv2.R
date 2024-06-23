# Create data frames
mse_df <- data.frame(
  distance,
  Statnu0.5 = mse.statnu0.5,
  Nonstatnu0.5 = mse.nonstatnu0.5,
  Statnu1.5 = mse.statnu1.5,
  Nonstatnu1.5 = mse.nonstatnu1.5,
  Statnuest = mse.statnuest,
  Nonstatnuest = mse.nonstatnuest
)

ls_df <- data.frame(
  distance,
  Statnu0.5 = -ls.statnu0.5,
  Nonstatnu0.5 = -ls.nonstatnu0.5,
  Statnu1.5 = -ls.statnu1.5,
  Nonstatnu1.5 = -ls.nonstatnu1.5,
  Statnuest = -ls.statnuest,
  Nonstatnuest = -ls.nonstatnuest
)

# Convert to long format
mse_long <- mse_df %>%
  pivot_longer(cols = -distance, names_to = "nu", values_to = "MSE")

ls_long <- ls_df %>%
  pivot_longer(cols = -distance, names_to = "nu", values_to = "LogScore")


# Update the label mappings
label_mapping <- c(
  "Statnu0.5" = "0.5", 
  "Nonstatnu0.5" = "0.5*", 
  "Statnu1.5" = "1.5", 
  "Nonstatnu1.5" = "1.5*", 
  "Statnuest" = "est", 
  "Nonstatnuest" = "est*"
)

# Define color and linetype mapping
color_mapping <- c(
  "Statnu0.5" = "blue", 
  "Nonstatnu0.5" = "red", 
  "Statnu1.5" = "black", 
  "Nonstatnu1.5" = "green", 
  "Statnuest" = "darkorange", 
  "Nonstatnuest" = "skyblue"
)

linetype_mapping <- c(
  "Statnu0.5" = "dashed", 
  "Nonstatnu0.5" = "solid", 
  "Statnu1.5" = "dashed", 
  "Nonstatnu1.5" = "solid", 
  "Statnuest" = "dashed", 
  "Nonstatnuest" = "solid"
)

# Plot MSE
mse_plot <- ggplot(mse_long, aes(x = distance, y = MSE, color = nu, linetype = nu)) +
  geom_line(size = 1) +
  labs(y = "MSE", x = "Distance in km") +
  scale_color_manual(values = color_mapping, labels = label_mapping) +
  scale_linetype_manual(values = linetype_mapping, labels = label_mapping) +
  theme_minimal() +
  theme(text = element_text(family = "Palatino"))

# Plot negative log-score
ls_plot <- ggplot(ls_long, aes(x = distance, y = LogScore, color = nu, linetype = nu)) +
  geom_line(size = 1) +
  labs(y = "Negative Log-Score", x = "Distance in km") +
  scale_color_manual(values = color_mapping, labels = label_mapping) +
  scale_linetype_manual(values = linetype_mapping, labels = label_mapping) +
  theme_minimal() +
  theme(text = element_text(family = "Palatino"))

# Combine plots with a shared legend at the top in a single line
combined_plot <- mse_plot + ls_plot + 
  plot_layout(guides = 'collect') & 
  theme(legend.position = 'top') & 
  guides(color = guide_legend(nrow = 1), linetype = guide_legend(nrow = 1))

# Display combined plot
print(combined_plot)
