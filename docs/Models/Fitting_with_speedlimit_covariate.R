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

# loading the graph
load("Graph_objects/final_graph_wholeday_speed75_setlimits_with_quantile.RData")

# weights_quantile =  sf_graph$get_edge_weights()
# save(weights_quantile, file = "Data_files/weights_quantile.RData")

# load the data
load("onlybuses.RData")

# filtering the data ---------------------------------------------------------------------------

only1PM = busdataset %>% 
  mutate(hour = as.integer(format(date, "%I"))) %>% # to later use filter(hour == 1)
  filter(AM_PM == "PM") %>% # only PM observations # to remove AM observations
  filter(hour == 1) %>% # to select observations between 1pm to 2pm
  dplyr::select(-date, -AM_PM, -hour, -Vehicle.ID) # to remove useless variables


only1PMevery7 = only1PM %>% 
  filter(day %in% c(7,14,21,28)) %>% # to select these 4 days
  filter( Average.Speed < 75) %>% # to remove some atypical speed observations
  distinct(geometry, .keep_all = TRUE) %>% # to remove observations with repeated location
  mutate(day = day/7) # so that the groups are 1,2,3,4

# adding observations -----------------------------------------------------------------------

sf_graph$add_observations(data = only1PMevery7, group = "day", clear_obs = TRUE, tolerance = 0.2, duplicated_strategy = "jitter")
sf_graph$edgeweight_to_data(data_loc=TRUE)


#save(sf_graph, file = "Graph_objects/graph_with_quantileweights_and_obs_1PM.RData")

# getting data, observations and speed limits

data <- sf_graph$get_data()
data_new <- data %>% dplyr::select(Average.Speed, speedlimit)%>%na.omit()

# running a linear model
res <- lm(Average.Speed ~ speedlimit, data = data_new)

# plot to check
graphics.off()
png("Plots/qqplot_on_residuals_with_speedlimits.png", width = 8, height = 6, units = "in", res = 500)
plot(res, which = 2) # 2 is to plot the qqplot on the residuals
dev.off()

png("Plots/residuals_on_graph.png", width = 8, height = 6, units = "in", res = 500)
sf_graph$plot(newdata = data_new %>% mutate(residuals = res$residuals), "residuals", vertex_size = 0)
dev.off()

png("Plots/difference_avg.speed_speedlimits.png", width = 8, height = 6, units = "in", res = 500)
sf_graph$plot(newdata = data_new %>% mutate(diff = Average.Speed-speedlimit), "diff", vertex_size = 0)
dev.off()

cov_obs <- data_new[[".distance_on_edge"]]
cov_obs <- 2 * ifelse(cov_obs > 0.5, 
                      1 - cov_obs,
                      cov_obs)

res2 <- lm(Average.Speed ~ speedlimit+cov_obs, data = data_new)

#sf_graph$plot(newdata = data_new %>% mutate(residuals = res2$residuals), "residuals", vertex_size = 0)

png("Plots/qqplot_on_residuals_with_speedlimits_and_cov.obs.png", width = 8, height = 6, units = "in", res = 500)
plot(res2, which = 2)
dev.off()


plot(data_new$speedlimit, data_new$Average.Speed, main = "blue is regression line")
abline(res2, col = "blue")
abline(a = 0, b = 1, col = "red")

min(res$residuals)



sf_graph$plot(newdata = data_new %>% 
                mutate(residuals = res$residuals, absresiduals = abs(res$residuals)) %>% 
                filter(absresiduals >= quantile(absresiduals, probs = 0.9)),
              "residuals", 
              vertex_size = 0, edge_weight = "secondary", edge_width_weight = "secondary", edge_width = 6)



sf_graph$plot(newdata = data_new %>%
                mutate(residuals = res$residuals, absresiduals = abs(res$residuals)) %>% 
                filter(residuals > 0) %>%
                filter(absresiduals >= quantile(absresiduals, probs = 0.9)),
              "residuals", 
              vertex_size = 0, edge_weight = "primary", edge_width_weight = "primary", edge_width = 6)