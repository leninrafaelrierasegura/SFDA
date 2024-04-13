## Workflow to create data

 1. Run `13Adding_tomtom_to_osm.R` 

- This will only add TomTom data to the OSM network. It will create `speed_limit_with_tomtom.RData`, which contains a network. No graph will be created.

 2. Run `14Graph_builder_tomtom_to_osmdata.R`
 
 - This file uses the network from `speed_limit_with_tomtom.RData`, and creates a graph with weights (the ones obtained from tomtom).
 
 3. 
 
 
 
## Workflow to remove zero speed observations where the bus has stopped for a long period

1. Run `Preprocessing/19Remove_zero_speed_for_long_periods.R`

- This will create `"Data_files/data_day7142128_hour13_with_no_consecutive_zeros.RData"`, which contains data corresponding to January 7,14,21,28 from 1 to 2 pm where consecutive zero speed observations have been removed.

2. Run `Preprocessing/21process_data_with_no_consecutive_zeros.R` (similar to `Preprocessing/18creates_data_in_graph_coordinates_everyhour_forevery7days.R`)

- This will use `"Data_files/data_day7142128_hour13_with_no_consecutive_zeros.RData"` and `"Graph_objects/graph_construction_11_03_2024.RData"` to process the data into graph data. This will create `"Data_files/data_day7142128_hour13_with_no_consecutive_zeros_graph_processed.RData"`

3. Run `Covariates/5Creates_covariance_on_graph.R`

- This will use `"Data_files/data_day7142128_hour13_with_no_consecutive_zeros_graph_processed.RData"`, a graph object (presumably `"Graph_objects/graph_construction_11_03_2024.RData"`), a network object (presumably `Data_files/stops_data_on_graph.RData`) with signs information, and create a file with all available covariate for all the data.

4. Run any R script that fits models.
 
## New workflow for working with graph built entirely from tomtom network

1. Run `Preprocessing/23Subsetting.by.road.type.file1.R` to built the graph from tomtom network.
   
- This will use `Data_files/tomtom.RData` and  `Data_files/data_day7142128_hour13_with_no_consecutive_zeros.RData` (the latter just to get the crs) to create `Graph_objects/graph_construction_21_03_2024alltomtomwhichlonglatsf.RData`.

2. Run `Preprocessing/23Subsetting.by.road.type.file2.R` to process the data 
   
- This will use `Data_files/data_day7142128_hour13_with_no_consecutive_zeros.RData` and `Graph_objects/graph_construction_21_03_2024alltomtomwhichlonglatsf.RData` to create `Data_files/data_day7142128_hour13_with_no_consecutive_zeros_graphsubset_graph_processed.RData`.

3. Run `6stops_obs_on_graph.R` to process the signs into graph coordinates
   
- This will use `Graph_objects/graph_construction_21_03_2024alltomtomwhichlonglatsf.RData` and `Networks object/traffic_stop.RData` to create `Data_files/stops_data_on_graphsubset.RData`.

4. Run `Covariates/7Creates_covariates_on_graph_without_consec_zeros.R` to create all possible covariates on graph

- This will use `Graph_objects/graph_construction_21_03_2024alltomtomwhichlonglatsf.RData`, `Data_files/data_day7142128_hour13_with_no_consecutive_zeros_graphsubset_graph_processed.RData`, and `Data_files/stops_data_on_graphsubset.RData` to create `Data_files/data_on_graph_with_covariates_no_consecutive_zerosgraphsubset.RData`

5. Make sure to adapt `Covariates/Create_covariates_on_mesh.R`

6. Model
