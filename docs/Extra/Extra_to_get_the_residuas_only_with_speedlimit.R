library(sf)
library(mapview)
library(dplyr)
library(plotly)
library(lubridate)
library(ggplot2)
library(gganimate)
library(MASS)
library(osmdata)
library(tidyr)

load("Graph_objects/graph_construction_03_03_2024.RData")
load("Data_files/Data_in_graph_coordinates_for_every7days/data_on_graph_hour_13.RData")
load("Data_files/data_on_graph_with_bus_signals_stop_density_and_numbers_covariates.RData")
# adding observations -----------------------------------------------------------------------

sf_graph$add_observations(data = data_on_graph, group = "day", tolerance = 0.2, duplicated_strategy = "jitter")



#sf_graph$edgeweight_to_data(data_loc=TRUE)


# getting data, observations and speed limits


data_new <- data_on_graph_with_bus_signals_stop_density_covariates %>% 
dplyr::select(speed, SpeedLimit) %>%
mutate(Average.Speed = speed, speedlimit = SpeedLimit) %>%
filter(Average.Speed > 0)

# running a linear model
res <- lm(Average.Speed ~ speedlimit, data = data_new)

i = 13
# to get qqplots
p = paste("Plots/no_zero_new_qqplot_on_residuals_with_speedlimits_", i, ".png", sep = "")
png(p, width = 8, height = 6, units = "in", res = 500)
plot(res, which = 2) # 2 is to plot the qqplot on the residuals
dev.off()


# to get residuals on graph
p2 =paste("Plots/no_zero_new_residuals_on_graph_", i, ".png", sep = "")
q2 = sf_graph$plot(newdata = data_new %>% mutate(residuals = res$residuals), "residuals", vertex_size = 0)
ggsave(p2, plot = q2, width = 8, height = 6, dpi = 500)



# to show the data
p3 = paste("Plots/no_zero_new_data_on_hour_", i, ".png", sep = "")
png(p3, width = 8, height = 6, units = "in", res = 500)
plot(data_new$speedlimit, data_new$Average.Speed)
dev.off()



for (k in c("motorway", "primary", "secondary")) {
  
p4 = paste("Plots/new_highest10_residuals_on_", k, "_at_hour_", i, ".png", sep = "")
q4 = sf_graph$plot(newdata = data_new %>% 
                mutate(residuals = res$residuals, absresiduals = abs(res$residuals)) %>% 
                filter(absresiduals >= quantile(absresiduals, probs = 0.9)),
              "residuals", 
              vertex_size = 0, edge_weight = k, edge_width_weight = k, edge_width = 6)
ggsave(p4, plot = q4, width = 8, height = 6, dpi = 500)


p5 = paste("Plots/new_highest10_neg_residuals_on_", k, "_at_hour_", i, ".png", sep = "")
q5 = sf_graph$plot(newdata = data_new %>%
                mutate(residuals = res$residuals, absresiduals = abs(res$residuals)) %>% 
                filter(residuals < 0) %>%
                filter(absresiduals >= quantile(absresiduals, probs = 0.9)),
              "residuals", 
              vertex_size = 0, edge_weight = k, edge_width_weight = k, edge_width = 6)
ggsave(p5, plot = q5, width = 8, height = 6, dpi = 500)


p6 = paste("Plots/new_highest10_pos_residuals_on_", k, "_at_hour_", i, ".png", sep = "")
q6 = sf_graph$plot(newdata = data_new %>%
                mutate(residuals = res$residuals, absresiduals = abs(res$residuals)) %>% 
                filter(residuals > 0) %>%
                filter(absresiduals >= quantile(absresiduals, probs = 0.9)),
              "residuals", 
              vertex_size = 0, edge_weight = k, edge_width_weight = k, edge_width = 6)
ggsave(p6, plot = q6, width = 8, height = 6, dpi = 500)
}









