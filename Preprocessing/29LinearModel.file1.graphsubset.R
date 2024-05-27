library(plotly)
library(dplyr)
library(here)
# helper functions
standardize <- function(vector) {return((vector - mean(vector)) / sd(vector))}
convert_to_binary <- function(input_vector) {return(ifelse(input_vector != 0, 1, 0))}

################################################################################
################################# PREPARE THE DATA #############################
################################################################################


# loading the data
load(here("Data_files/data_on_graph_with_covariates_no_consecutive_zerosgraphsubset.RData"))

data = data_on_graph_with_covariates %>% 
  mutate(across(starts_with(c("class_", "upto")), list(ind = convert_to_binary))) %>%
  mutate(across(c("bus", "signal", "stop", "crossing"), ~round(., 5))) %>%
  mutate(across(c("density_per_hour"), standardize)) 

res = lm(speed ~ SpeedLimit + 
           density_per_hour +
           bus +
           signal +
           stop +
           crossing +
           upto1_ind +
           bus_number + 
           signal_number +
           stop_number +
           crossing_number,
         data = data)

summary(res)

yhat = res$fitted.values
plot(yhat)
