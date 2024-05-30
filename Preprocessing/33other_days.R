library(sf)
library(dplyr)
library(tidyr)
library(mapview)
library(MetricGraph)
library(here)


load(here("Data_files/data_day8152229_hour9_with_no_consecutive_zeros.RData")) # to get the crs


################################################################################
#              build polygon to cut the network and the data                   #
################################################################################

polygon = st_multipoint(c(st_point(c(-122.53000, 37.69702)),
                          st_point(c(-122.37000, 37.69702)),
                          st_point(c(-122.37000, 37.82600)),
                          st_point(c(-122.53000, 37.82600)))) %>%
  st_cast("POLYGON") %>%
  st_sfc(crs = st_crs(df))


################################################################################
#                                   save the graph                             #
################################################################################

load(here("Graph_objects/graph_construction_19MAY24_FRC0134.RData"))


################################################################################
#                                 process the data                             #
################################################################################

data.reduced = st_filter(x = df, y = polygon, .predicate = st_within)

sf_graph$add_observations(data = data.reduced, group = "day", tolerance = 0.02, duplicated_strategy = "jitter") # tolerance = 20m

# getting the data from the graph (that is, in graph coordinates)
data_on_graph = sf_graph$get_data()

################################################################################
#                                   save the data                              #
################################################################################

save(data_on_graph, file = here("Data_files/data_day8152229_hour9_with_no_consecutive_zeros_19MAY24_FRC0134_graph_processed.RData"))

# Files produced by this script

# "Data_files/data_day7142128_hour8_with_no_consecutive_zeros_19MAY24_FRC0134_graph_processed.RData"
# Message: The total number of points removed due do being far is 66376

# "Data_files/data_day7142128_hour16_with_no_consecutive_zeros_19MAY24_FRC0134_graph_processed.RData"
# Message: The total number of points removed due do being far is 66764

# "Data_files/data_day6132027_hour8_with_no_consecutive_zeros_19MAY24_FRC0134_graph_processed.RData"
# The total number of points removed due do being far is 62073

# "Data_files/data_day6132027_hour16_with_no_consecutive_zeros_19MAY24_FRC0134_graph_processed.RData"
# The total number of points removed due do being far is 60580

# "Data_files/data_day8152229_hour9_with_no_consecutive_zeros_19MAY24_FRC0134_graph_processed.RData"
# The total number of points removed due do being far is 65914
