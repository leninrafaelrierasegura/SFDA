# `Preprocessing` folder description
 
Here is a short description of the files that accompany this Readme file. We describe what they do and why.

## Miscellaneous

### `1Process_raw_data.R`

- This R script reads the raw data (a csv. file) and process it into a data frame (saved as January) with columns `day`, `hour`, and `speed`. It saves the data frame in a .RData file with path `"Data_files/january.RData"`


### `3speedlimits_using_quantile.R`

- This file builds a metric graph from `speed_limit_with_close_dist.RData` network, adds observations from a whole day (7 January), and uses the data to set the speed limits as the 95% quantile. 
**Pruning is carried out during the graph creation**. **Build a metric graph**. **Not important because later is not used**.


### `4fixed_weights_from_quantile.R`

- This file fixes something that was wrong (the column of speed limits was ok but the one-hot encoding was not ok) with `weights_quantile.RData` (Only God knows where it was created. It was created using the weights from the graph built in `3speedlimits_using_quantile.R`). **Not important because later is not used**.


### `7Graph_builder_osmdata.R`

- This file builds a graph from `speed_limit_with_close_dist.RData` network and using `weights_quantile_corrected.RData` as weights. **Pruning is carried out during the graph creation**. **Build a metric graph**. **Not important because later is not used**.


### `8Visualizing_tomtom_data_and_sings.R`

- This file does not do much. **Not important because later is not used**.


### `10Computing_geodist_PtE.R`

- This file was an attempt to create a covariate that takes into account the minimum distance to a sign. It was not efficient. **Not important because later is not used**.
 
 
### `11Check_if_tomtom_graph_contains_most_of_the_data.R`
 
- This file just plots the network stored in `tomtom.RData` and some data points. **Not important because later is not used**.
 
### `12Graph_builder_tomtom_data.R`

- This file uses `tomtom.RData` network to build a graph. It is not useful because we cannot build a graph with type 6 and 7 roads. **Pruning is carried out during the graph creation**. **Build a metric graph**. **Not important because later is not used**.


### `16Process_raw_data_with_ID.R`

- This R script does the same as Process_raw_data.R, except that it stores three additional columns: `ID` with the bus ID, `datetime`, and `PDT` (same as `datetime`, just another format). It saves a data frame in an .RData file with path `"Data_files/january_with_ID.RData"`.



### `19Remove_zero_speed_for_long_periods.R`

- This file uses data corresponding to January 7,14,21,28 from 1 to 2 pm and remove consecutive zero speed observations.



### `22check.covarariates.are.computed.correctly.R`

- This file uses a simple graph and the code used to create covariates (`5Creates_covariates_on_graph.R`) to check we are creating the covariates correctly.


## From OSM - weights and the network

### `2Creates_network_with_speedlimits_with_close_dist.R`

- This file gets data from OSM and creates a network (saved as `speed_limit_with_close_dist.RData`), where the NA values were replaced by the closest available value according to the road type. **The network in this file is the base for all futures networks and metric graphs**.



## From TomTom

### `9Creates_tomtom_dataset_from_shapefile_and_json.R`

- This file deals with data ONLY from TomTom. No network or metric_graph is used in here. It produces `tomtom.RData`, which contains all (absolutely all) weights that can be obtained from TomTom.




## From OSM - only signs locations

### `5Traffic_stop_inclussion.R`

- This file gets the signs locations from OSM and save them into a network object called `traffic_stop.RData`.

### `6stops_obs_on_graph.R`

- This file reads `traffic_stop.RData` (a network object) and processes into graph data called `stops_data_on_graph.RData` using a graph. **This file requires a graph**.

Something else