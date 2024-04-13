library(MetricGraph)
library(plotly)
library(dplyr)
library(MASS)

# loading the data
load("Data_files/data_on_graph_with_bus_signals_stop_density_and_numbers_covariates.RData")
load("Graph_objects/graph_construction_03_03_2024.RData")
load("Data_files/stops_data_on_graph.RData")

data = data_on_graph_with_bus_signals_stop_density_covariates

# adding observations
sf_graph$add_observations(data = data, group = "day", clear_obs = TRUE)


p = sf_graph$plot(newdata = data %>% filter(speed >= 0), group = 1:4, "speed", vertex_size = 0)

p

p + geom_point(traffic_stops %>% filter(highway == "traffic_signals"), mapping = aes(x=.coord_x, y=.coord_y), color = "red") 

p + geom_point(traffic_stops %>% filter(highway == "bus_stop"), mapping = aes(x=.coord_x, y=.coord_y), color = "blue")
 
p + geom_point(traffic_stops %>% filter(highway == "stop"), mapping = aes(x=.coord_x, y=.coord_y), color = "black")

p + geom_point(traffic_stops %>% filter(highway == "crossing"), mapping = aes(x=.coord_x, y=.coord_y), color = "violet")


p2 = p + geom_point(traffic_stops %>% filter(highway == "traffic_signals"), mapping = aes(x=.coord_x, y=.coord_y), color = "red") + 
  geom_point(traffic_stops %>% filter(highway == "bus_stop"), mapping = aes(x=.coord_x, y=.coord_y), color = "blue") +
 geom_point(traffic_stops %>% filter(highway == "stop"), mapping = aes(x=.coord_x, y=.coord_y), color = "black") 
  #geom_point(traffic_stops %>% filter(highway == "crossing"), mapping = aes(x=.coord_x, y=.coord_y), color = "darkgreen")

ggsave("Plots/create_covariates.png", plot = p2, width = 10, height = 8, dpi = 500)



res = lm(speed ~ SpeedLimit + density_per_hour + cov_bus_no_normalized + cov_stop_no_normalized + cov_signals_no_normalized + cov_crossing_no_normalized, data = data %>% filter(speed >= 0))

res = lm(speed ~ cov_bus_no_normalized + cov_number_bus + cov_signals_no_normalized + cov_number_signals + cov_stop_no_normalized + cov_number_stop +
                                                  cov_crossing_no_normalized +
                                                  cov_number_crossing +
                                                  SpeedLimit + highway +
                                                  density_per_hour, data = data %>% filter(speed > 0) %>% mutate(density_per_hour = density_per_hour/max(density_per_hour)))

plot(res, which = 2)

std.res <- broom::augment(res)

library(qqplotr)


ggplot(data = std.res, mapping = aes(sample = .std.resid)) + stat_qq_band() + stat_qq_line() + stat_qq_point() + labs(x = "Theoretical quantiles", y = "sample quantiles")

summary(res)



# Residuals on the graph

df_tmp <- data %>% mutate(residuals = res$residuals)

sf_graph$plot(newdata = df_tmp, data = "residuals", vertex_size = 0, scale_color_main = ggplot2::scale_color_viridis_c(option = "A"))


sf_graph$plot(newdata = data %>% filter(speed >= 0) %>%
                mutate(residuals = res$residuals, absresiduals = abs(res$residuals)) %>% 
                filter(absresiduals >= quantile(absresiduals, probs = 0.9)),
              "residuals", 
              vertex_size = 0)

sf_graph$plot(newdata = data %>%
                mutate(residuals = res$residuals, absresiduals = abs(res$residuals)) %>% 
                filter(residuals > 0) %>%
                filter(absresiduals >= quantile(absresiduals, probs = 0.9)),
              "residuals", 
              vertex_size = 0)


library(sandwich)
library(sjPlot)

tab_model(res, vcov.fun = "HC3", show.se=TRUE)

#rlm()

# lasso()