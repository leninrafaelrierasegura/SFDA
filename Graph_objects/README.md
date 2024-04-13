# File description
This file gives some details about of the files in this folder

## File content

- **final_graph_with_1day_data_filter_less_75.RData**:
  This .RData file contains a graph object (called sf_graph) that contains weights (speedlimits) obtained from OMS. It also contains the observations from onlybuses dataset (onlybuses.RData) corresponding to 7 JAN 2021. This file was created in speedlimits_using_quantiles.R. 

- **final_graph_with_1day_data_filter_less_75_weights_fixed.RData**:
  This .RData file contains the same information as in final_graph_with_1day_data_filter_less_75.RData (also called sf_graph), except that the weights were corrected using the speed observations corresponding to 7 JAN 2021. Here are some details. The quantile was 0.95. This file was created in speedlimits_using_quantiles.R.

- **graph_construction_21_03_2024.RData**:
  This graph was created using `23Subsetting.by.road.type.file1.R`. To create it, we use the same code as usual plus the following that cuts the initial graph within a window given by

```
p1 = st_point(c(-122.53000, 37.69702))
p2 = st_point(c(-122.37000, 37.69702))
p3 = st_point(c(-122.37000, 37.82600))
p4 = st_point(c(-122.53000, 37.82600))

poly = st_multipoint(c(p1, p2, p3, p4)) %>%
   st_cast("POLYGON") %>%
   st_sfc(crs = st_crs(from.tomtom))
 
tomtom.subset = from.tomtom %>% st_intersection(poly) %>% st_cast("LINESTRING")
```

- **graph_construction_21_03_2024alltomtom.RData**:
  This graph was created using `23Subsetting.by.road.type.file1.R`. In its creation, all the network from tomtom was used. This is how we created it: `graph = graph_components$new(edges = edges, longlat = TRUE, edge_weights = weights)`

- **graph_construction_21_03_2024alltomtomwhichlonglatsf.RData**:
  This graph was created using `23Subsetting.by.road.type.file1.R`. In its creation, all the network from tomtom was used. This is how we created it: `graph = graph_components$new(edges = edges, which_longlat = "sf", longlat = TRUE, edge_weights = weights)`