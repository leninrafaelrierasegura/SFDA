# Some useful functions or strategies that are worth writing down

- st_drop_geometry(sf_object): it transforms sf_object into a regular data frame 
  without any attribute and removes the geometry column. This is useful when adding the weights 
  when building a metric_graph object.

## To get geometries that are close to other geometries (can be points as well)

closed_to_network = st_is_within_distance(from_tomtom, only1PMevery7, sparse = TRUE, dist = 30)
only1PMevery7.clean = only1PMevery7[unique(unlist(closed_to_network)),]

## To plot two geometries in the same picture

(ggplot() +
  geom_sf(data = from_tomtom, color = "black", size = 8) +
  geom_sf(data = only1PMevery7.clean, color = "blue", size = 0.5)) %>% ggplotly()
 
## To plot the removed observation (projected on same points) on the graph.

rem.obs<-sf_graph$add_observations(data=only1PMevery7, group="day",
          tolerance=0.2)

p <- sf_graph$plot(vertex_size=0, edge_weight = "SpeedLimit", 
edge_width_weight = "SpeedLimit", edge_width = 2, 
data="Average.Speed", add_new_scale_weights = FALSE, data_size=2)
p + geom_point(rem.obs, mapping = aes(x=.coord_x, y=.coord_y), color="red")


## To plot data and traffic lights and bus stops

p = sf_graph$plot(vertex_size=0, data="Average.Speed", add_new_scale_weights = FALSE, data_size=2)
p + geom_point(traffic_stops %>% filter(highway == "traffic_signals"), mapping = aes(x=.coord_x, y=.coord_y), color = "red") + 
geom_point(traffic_stops %>% filter(highway == "bus_stop"), mapping = aes(x=.coord_x, y=.coord_y), color = "blue")

## To remove points with the same location

sf_object %>% distinct(geometry, .keep_all = TRUE)

## To remove members of a list with NULL type

your_list[!sapply(your_list, is.null)]

## To get the most frequent number in a vector and its frequency

freq_table <- table(vector)
names(freq_table)[which.max(freq_table)]

max(freq_table)

## To remove consecutive zeros c(0,1,0,0,0,1,0) -> c(0,1,0,1,0)

remove_consecutive_zeros <- function(vec) {
  # Initialize a result vector
  result <- numeric(length(vec))
  # Index for the result vector
  index <- 1
  # Loop through the original vector
  for (i in 1:length(vec)) {
    # If current value is not zero or previous value is not zero, or it's the first zero, add it to result
    if (vec[i] != 0 || (i > 1 && vec[i - 1] != 0) || i == 1) {
      result[index] <- vec[i]
      index <- index + 1
    }
  }
  # Trim the result vector to remove unused entries
  result <- result[1:(index - 1)]
  return(result)
}

## Conditional independence (x and z are conditional independence)

This can be seen from the precision matrix but no from the covariance matrix

n = 1000
x <- rnorm(n)
y <- x + rnorm(n)
z <- y + rnorm(n)
var(data.frame(x = x, y = y, z = z))
solve(var(data.frame(x = x, y = y, z = z))) 

## To define a buffer zone around an sf object

This can be done with st_buffer()

## To define a two level function 

power <- function(exponent) {
  function(x) x ^ exponent
}

square <- power(2)
square(2)

