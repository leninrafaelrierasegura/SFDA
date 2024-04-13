library(sf)
library(lwgeom)
library(mapview)
library(dplyr)
library(ggplot2)
library(terra)
library(tmap)

load("Data_files/data_on_graph_with_covariates_no_consecutive_zerosgraphsubset.RData")

points = data_on_graph_with_covariates %>% 
  as.data.frame() %>%
  st_as_sf(coords = c(".coord_x", ".coord_y"), crs = 4326) %>%
  mutate(., index = 1:nrow(.)) %>%
dplyr::select(index)


# to define a polygon
p01 = st_point(c(-122.47607, 37.80832))
p02 = st_point(c(-122.40655, 37.81065))
p03 = st_point(c(-122.38649, 37.79022))
p04 = st_point(c(-122.38224, 37.74355))
p05 = st_point(c(-122.40355, 37.70275))
p06 = st_point(c(-122.43990, 37.71142))
p07 = st_point(c(-122.47231, 37.70100))
p08 = st_point(c(-122.50785, 37.73528))
p09 = st_point(c(-122.51519, 37.78053))
p10 = st_point(c(-122.48840, 37.78684))
polygon = st_multipoint(c(p01, p02, p03, p04, p05, p06, p07, p08, p09, p10)) %>%
  st_cast("POLYGON") %>%
  st_sfc(crs = st_crs(points))
  

grid100 = st_make_grid(polygon, n = c(100, 100)) %>% 
  as.data.frame() %>% 
  st_as_sf(crs = st_crs(points)) %>% 
  st_centroid() %>%
  mutate(dummy = 1)

grid10 = st_make_grid(polygon, n = c(10, 10)) %>% 
  as.data.frame() %>% 
  st_as_sf(crs = st_crs(points)) %>% 
  st_centroid() %>%
  mutate(dummy = 1)

get.index = points %>% 
  st_join(x = grid100, y = ., join = st_nearest_feature) %>% 
  dplyr::select(index) %>%
  st_join(x = grid10, y = ., join = st_nearest_feature) %>%
  dplyr::select(index) %>%
  st_filter(polygon)

chosen.points = points[get.index$index, ]

distance = seq(from = 0, to = 2000, by = 50)
mse_int <- ls_int <- mse_fra <- ls_fra <- rep(0,length(distance))
# cross-validation for-loop
for(j in 1:length(distance)){
  print(j)
  groups <- list()
  for(i in 1:nrow(get.index)) {
    # get index of the points that should be removed
    groups[[i]] <- (chosen.points[i,2] %>% 
                      st_buffer(dist = distance[j], nQuadSegs = 1000000) %>%
                      st_filter(x = points, y = .) %>%
                      as.data.frame())$index
  }
  # cross-validation of the fractional model 
  cv <- inla.group.cv(res, groups = groups)
  # cross-validation of the integer model
  cv_int <- inla.group.cv(res_int, groups = groups)
  # obtain MSE and LS
  mse_int[j] <- mean((cv_int$mean-data$z)^2)
  mse_fra[j] <- mean((cv$mean-data$z)^2)
  ls_int[j] <- mean(log(cv_int$cv))
  ls_fra[j] <- mean(log(cv$cv))
}





