library(MetricGraph)
library(plotly)
library(dplyr)
library(MASS)
library(glmnet)
library(car)


# loading the data
load("Data_files/data_on_graph_with_bus_signals_stop_density_and_numbers_covariates.RData")
load("Graph_objects/graph_construction_03_03_2024.RData")
load("Data_files/stops_data_on_graph.RData")

data = data_on_graph_with_bus_signals_stop_density_covariates %>% 
  mutate(density_per_hour = density_per_hour/max(density_per_hour)) %>%
  filter(speed >= 0)




res = lm(speed ~ cov_bus_no_normalized +
           #cov_number_bus + 
           cov_signals_no_normalized +
           #cov_number_signals + 
           cov_stop_no_normalized +
           #cov_number_stop +
           cov_crossing_no_normalized +
           #cov_number_crossing +
           SpeedLimit +
           highway +
           density_per_hour, data = data %>% mutate(highway = as.factor(highway)))

summary(res)
plot(res, which = 2)

car::vif(res)

# stepwise regression

step_model <- stepAIC(res)
summary(step_model)

# LASSO regression

y = as.vector(data$speed)
x = data.matrix(data[, c("cov_bus_no_normalized", 
                         "cov_number_bus", 
                         "cov_signals_no_normalized",
                         "cov_number_signals",
                         "cov_stop_no_normalized",
                         "cov_number_stop",
                         "cov_crossing_no_normalized",
                         "cov_number_crossing",
                         "SpeedLimit",
                         "highway",
                         "density_per_hour")])


cv_model = cv.glmnet(x, y, alpha = 1)
best_lambda = cv_model$lambda.min


best_model = glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)


cor(x)