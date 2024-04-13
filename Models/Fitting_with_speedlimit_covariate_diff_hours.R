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


load("onlybuses.RData")

for (i in c(1, 3, 5, 7)) {
# filtering the data ---------------------------------------------------------------------------
  
  # loading the graph
  load("Graph_objects/graph_construction_28_02_2024.RData")
  
  # weights_quantile =  sf_graph$get_edge_weights()
  # save(weights_quantile, file = "Data_files/weights_quantile.RData")
  
  # load the data
  
  tryCatch({
    
only1PM = busdataset %>% 
  mutate(hour = as.integer(format(date, "%I"))) %>% # to later use filter(hour == 1)
  filter(AM_PM == "PM") %>% # only PM observations # to remove AM observations
  filter(hour == i) %>% # to select observations between 1pm to 2pm
  dplyr::select(-date, -AM_PM, -hour, -Vehicle.ID) # to remove useless variables


only1PMevery7 = only1PM %>% 
  filter(day %in% c(7,14,21,28)) %>% # to select these 4 days
  filter( Average.Speed < 75) %>% # to remove some atypical speed observations
  distinct(geometry, .keep_all = TRUE) %>% # to remove observations with repeated location
  mutate(day = day/7) # so that the groups are 1,2,3,4

# adding observations -----------------------------------------------------------------------

sf_graph$add_observations(data = only1PMevery7, group = "day", tolerance = 0.2, duplicated_strategy = "jitter")

save(sf_graph, file = paste("Graph_objects/graph_with_correct_edges_and_hour_", i, ".RData", sep = ""))


sf_graph$edgeweight_to_data(data_loc=TRUE)


# getting data, observations and speed limits

data <- sf_graph$get_data()
data_new <- data %>% dplyr::select(Average.Speed, speedlimit)%>%na.omit()

# running a linear model
res <- lm(Average.Speed ~ speedlimit, data = data_new)


# to get qqplots
p = paste("Plots/qqplot_on_residuals_with_speedlimits_", i, ".png", sep = "")
png(p, width = 8, height = 6, units = "in", res = 500)
plot(res, which = 2) # 2 is to plot the qqplot on the residuals
dev.off()


# to get residuals on graph
p2 =paste("Plots/residuals_on_graph_", i, ".png", sep = "")
q2 = sf_graph$plot(newdata = data_new %>% mutate(residuals = res$residuals), "residuals", vertex_size = 0)
ggsave(p2, plot = q2, width = 8, height = 6, dpi = 500)


#cov_obs <- data_new[[".distance_on_edge"]]
#cov_obs <- 2 * ifelse(cov_obs > 0.5,  1 - cov_obs, cov_obs)

# to show the data
p3 = paste("Plots/data_on_hour_", i, ".png", sep = "")
png(p3, width = 8, height = 6, units = "in", res = 500)
plot(data_new$speedlimit, data_new$Average.Speed)
dev.off()



for (k in c("motorway", "primary", "secondary")) {
  
p4 = paste("Plots/highest10_residuals_on_", k, "_at_hour_", i, ".png", sep = "")
q4 = sf_graph$plot(newdata = data_new %>% 
                mutate(residuals = res$residuals, absresiduals = abs(res$residuals)) %>% 
                filter(absresiduals >= quantile(absresiduals, probs = 0.9)),
              "residuals", 
              vertex_size = 0, edge_weight = k, edge_width_weight = k, edge_width = 6)
ggsave(p4, plot = q4, width = 8, height = 6, dpi = 500)


p5 = paste("Plots/highest10_neg_residuals_on_", k, "_at_hour_", i, ".png", sep = "")
q5 = sf_graph$plot(newdata = data_new %>%
                mutate(residuals = res$residuals, absresiduals = abs(res$residuals)) %>% 
                filter(residuals < 0) %>%
                filter(absresiduals >= quantile(absresiduals, probs = 0.9)),
              "residuals", 
              vertex_size = 0, edge_weight = k, edge_width_weight = k, edge_width = 6)
ggsave(p5, plot = q5, width = 8, height = 6, dpi = 500)


p6 = paste("Plots/highest10_pos_residuals_on_", k, "_at_hour_", i, ".png", sep = "")
q6 = sf_graph$plot(newdata = data_new %>%
                mutate(residuals = res$residuals, absresiduals = abs(res$residuals)) %>% 
                filter(residuals > 0) %>%
                filter(absresiduals >= quantile(absresiduals, probs = 0.9)),
              "residuals", 
              vertex_size = 0, edge_weight = k, edge_width_weight = k, edge_width = 6)
ggsave(p6, plot = q6, width = 8, height = 6, dpi = 500)
}

}, error = function(err){
  warning(paste("i=", i, ",k=", k, "Error:", conditionMessage(err)))
  print(paste("i=", i, ",k=", k, "Error:", conditionMessage(err)))
})

rm(sf_graph)
}







