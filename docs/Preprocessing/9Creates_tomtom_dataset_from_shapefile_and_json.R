library(sf)
library(mapview)
library(jsonlite)
library(dplyr)

#tomtom_geojson = st_read("Data_files/San_Francisco_data_from_TomTom.geojson")

# read shape file
tomtom_sph = st_read("Data_files/San_Francisco_data_from_TomTom/network.shp")

# read json file and select the information of interest
tomtom_json = fromJSON("Data_files/San_Francisco_data_from_TomTom.json")$network$segmentResults$segmentTimeResults


# Initialize empty data frames for 10 and 12 columns
df_10_columns <- data.frame()
df_12_columns <- data.frame()

# Iterate through the list of data frames
for (i in seq_along(tomtom_json)) {
  # Check the number of columns in the current data frame
  num_cols <- ncol(tomtom_json[[i]])
  
  # Add list number column
  tomtom_json[[i]]$List_Number <- i
  
  # Append to the appropriate data frame based on the number of columns
  if (num_cols == 10) {
    df_10_columns <- rbind(df_10_columns, tomtom_json[[i]])
  } else if (num_cols == 12) {
    df_12_columns <- rbind(df_12_columns, tomtom_json[[i]])
  }
}

# add two columns with NA values so that we can rbin later
from_10_to_12 = df_10_columns %>% mutate(standardDeviationSpeed = NA, travelTimeStandardDeviation = NA)
  
# rbind and  order by List_Number
almost_tomtom = rbind(from_10_to_12, df_12_columns) %>% arrange(List_Number)

# join shape file and the dataset we get from the above process
casi_tomtom = bind_cols(tomtom_sph, almost_tomtom) %>% 
  mutate(FRC = as.character(FRC))

PERCENTILES = do.call(rbind, casi_tomtom$speedPercentiles) %>% as.data.frame()
names(PERCENTILES) =  paste(seq(5, 95, by = 5), "percentile", sep = "")

tomtom = bind_cols(casi_tomtom, PERCENTILES) %>% dplyr::select(-speedPercentiles)


# save the obtained data set
save(tomtom, file = "Data_files/tomtom.RData")

  