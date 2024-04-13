# `Data_files` folder description

## `data_on_graph_with_covariates.RData`

This file was created in `5Creates_covariates.R` file, using  

- `load("Graph_objects/graph_construction_03_03_2024.RData")`
- `load("Data_files/Data_in_graph_coordinates_for_every7days/data_on_graph_hour_13.RData")`
- `load("Data_files/stops_data_on_graph.RData")`

## `tomtom.RData`

This file contains ONLY data from TomTom. No network nor metric_graph was used in its creation. It was created in `Creates_tomtom_dataset_from_shapefile_and_json.R` using 

- `tomtom_sph = st_read("Data_files/San_Francisco_data_from_TomTom/network.shp")`
- `tomtom_json = fromJSON("Data_files/San_Francisco_data_from_TomTom.json")$network$segmentResults$segmentTimeResults`


## `data_day7142128_hour13_with_no_consecutive_zeros.RData`

This file contains data corresponding to days 7, 14, 21 ,28 from 1 to 2 pm. It was created on March 14, 2024 using `Preprocessing/19Remove_zero_speed_for_long_periods.R` and 

- `load("~/Desktop/Spring 2024/january_with_ID.RData")`


## `data_day7142128_hour13_with_no_consecutive_zeros_graph_processed.RData`

This file contains the processed version of `data_day7142128_hour13_with_no_consecutive_zeros.RData`. It was created on March 14, 2024 using `Preprocessing/21process_data_with_no_consecutive_zeros.R` and 

- `load("Data_files/data_day7142128_hour13_with_no_consecutive_zeros.RData")`
- `load("Graph_objects/graph_construction_11_03_2024.RData")`



## `Data_files/data_on_graph_with_covariates_no_consecutive_zeros.RData`

This file contains the data on graph with covariates without considering consecutive zero speed observations. It was created on March 12, 2024 using `Covariates/7Create_covariates_on_graph_without_consec_zeros.R`

## `data_day7142128_hour13_with_no_consecutive_zeros_graphsubset_graph_processed.RData`

This file contains the processed version of `data_day7142128_hour13_with_no_consecutive_zeros.RData`. It was created on March 22, 2024 using `Preprocessing/23Subsetting.by.road.type.file2.R` and 

- `load("Graph_objects/graph_construction_21_03_2024alltomtomwhichlonglatsf.RData")`
- `load("Data_files/data_day7142128_hour13_with_no_consecutive_zeros.RData")`

## `Data_files/data_on_graph_with_covariates_no_consecutive_zerosgraphsubset.RData`

This file contains the data on graph with covariates without considering consecutive zero speed observations. It was created on March 22, 2024 using `Covariates/7Create_covariates_on_graph_without_consec_zeros.R`. Most importantly, it was created using 

- `load("Graph_objects/graph_construction_21_03_2024alltomtomwhichlonglatsf.RData")`
- `load("Data_files/data_day7142128_hour13_with_no_consecutive_zeros_graphsubset_graph_processed.RData")`
- `load("Data_files/stops_data_on_graphsubset.RData")`

## `Data_files/data_on_mesh_with_covariates_graphsubset.RData`

This file was created on March 22, 2024 using `Covariates/6Creates_covariates_on_mesh.R` and 

- `load("Graph_objects/graph_construction_21_03_2024alltomtomwhichlonglatsf.RData")`
- `load("Data_files/data_day7142128_hour13_with_no_consecutive_zeros_graphsubset_graph_processed.RData")`
- `load("Data_files/stops_data_on_graphsubset.RData")`

## `Data_files/stops_data_on_graphsubset.RData`

This file was created on March 22, 2024 using `6stops_obs_on_graph.R` and 

- `load("Graph_objects/graph_construction_21_03_2024alltomtomwhichlonglatsf.RData")`
- `load("Networks object/traffic_stop.RData")`
- `stops_obs <- sf_graph$add_observations(data = datapoints, tolerance = 0.007, clear_obs = TRUE) # tolerance 7m`


## `Data_files/data_day7142128_hour13and14_with_no_consecutive_zeros.RData`

This file contains data corresponding to days 7, 14, 21 ,28 from 1 to 3 pm. It is essentially the same as `Data_files/data_day7142128_hour13_with_no_consecutive_zeros.RData` with the exception that now we consider observations from 1 to 3 pm (before from 1 to 2 pm only). That is, we increase the time window to 2 hours now. It was created on March 22, 2024 using `Preprocessing/19Remove_zero_speed_for_long_periods.R` and 

- `load("~/Desktop/Spring 2024/january_with_ID.RData")`
