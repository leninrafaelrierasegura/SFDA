# balls  = points[get.index$index, ] %>% 
#   mutate(dummy = 1) %>% 
#   dplyr::select(dummy) %>% 
#   mutate(geometry = st_buffer(geometry, dist = 2000, nQuadSegs = 1000000))
# 
# to.check = st_filter(points, balls[80,]) %>% setdiff(x = points, y =.)