library(MetricGraph)
library(sf)
library(mapview)
library(dplyr)
library(plotly)
library(ggplot2)
library(osmdata)
library(broom)
library(EnvStats)
library(INLA)
library(inlabru)

bb = c(-122.51362, 37.69702, -122.35779 ,  37.81130)
data = opq(bbox = bb) %>%
  add_osm_feature(key="highway", value=c("motorway", "trunk", "tertiary", "primary", "secondary", "residential", "motorway_link", "unclassified")) %>%
  osmdata_sf()


data_s_limit_2<-data$osm_lines%>%dplyr::select(maxspeed, geometry, highway)%>%
  mutate(maxspeed = as.numeric(gsub("[^0-9.]", "", maxspeed)))

data_motorway = data_s_limit_2 %>% filter(highway %in% c("motorway_link", "motorway"))
tmp = data_motorway # all the data
tmp_na = tmp[which(is.na(tmp$maxspeed)),] # subset of data with na values
tmp_no_na = tmp[which(!is.na(tmp$maxspeed)),] # subset of data with no na values
iter = dim(tmp_na)[1]
for (j in 1:iter) {
matrix_aux = matrix(NA, ncol = 3, nrow = dim(tmp_na)[1])
for (i in 1:dim(tmp_na)[1]) {
  aux = tmp_na[i,] %>% rename(dummy  = maxspeed) 
  close = st_distance(aux, tmp_no_na)
  dist_aux = min(close)
  closestindex = which.min(close)
  matrix_aux[i, 1] = closestindex
  matrix_aux[i, 2] = i
  matrix_aux[i, 3] = dist_aux
}
colnames(matrix_aux) = c("closestindex", "i", "dist_aux")
df_order = matrix_aux %>% as.data.frame() %>% arrange(dist_aux)
newtmp = tmp_na[df_order$i[1], ]
newtmp[1,1] = tmp_no_na$maxspeed[df_order$closestindex[1]]
tmp_no_na = rbind(tmp_no_na, newtmp)
tmp_na = tmp_na[-df_order$i[1], ]
}

final_data = tmp_no_na
roads = c("trunk", "tertiary", "primary", "secondary", "unclassified")
for (k in 1:length(roads)) {
data_motorway = data_s_limit_2 %>% filter(highway == roads[k]) 
tmp = data_motorway # all the data
tmp_na = tmp[which(is.na(tmp$maxspeed)),] # subset of data with na values
tmp_no_na = tmp[which(!is.na(tmp$maxspeed)),] # subset of data with no na values
iter = dim(tmp_na)[1]
for (j in 1:iter) {
matrix_aux = matrix(NA, ncol = 3, nrow = dim(tmp_na)[1])
for (i in 1:dim(tmp_na)[1]) {
  aux = tmp_na[i,] %>% rename(dummy  = maxspeed)
  close = st_distance(aux, tmp_no_na)
  dist_aux = min(close)
  closestindex = which.min(close)
  matrix_aux[i, 1] = closestindex
  matrix_aux[i, 2] = i
  matrix_aux[i, 3] = dist_aux
}
colnames(matrix_aux) = c("closestindex", "i", "dist_aux")
df_order = matrix_aux %>% as.data.frame() %>% arrange(dist_aux)
newtmp = tmp_na[df_order$i[1], ]
newtmp[1,1] = tmp_no_na$maxspeed[df_order$closestindex[1]]
tmp_no_na = rbind(tmp_no_na, newtmp)
tmp_na = tmp_na[-df_order$i[1], ]
}
tmp_new = tmp_no_na
final_data  = rbind(final_data, tmp_new)
}
data_residential = data_s_limit_2 %>% filter(highway == "residential") %>% mutate(maxspeed = if_else(is.na(maxspeed), 25, maxspeed))
final_data = rbind(final_data, data_residential)

save(final_data, file="speed_limit_with_close_distance.RData")








