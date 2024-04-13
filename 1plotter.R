library(ggplot2)
library(plotly)
library(mapview)
sf_graph$plot(data = "SpeedLimit", 
              vertex_size = 0, 
              group = 1:4, 
              edge_width = 4, 
              edge_weight = "SpeedLimit", 
              edge_width_weight = "SpeedLimit", 
              edge_color = "SpeedLimit", 
              add_new_scale_weights = FALSE)

sf_graph$plot(vertex_size = 0, 
              edge_width = 1, 
              edge_weight = "10percentile", 
              edge_width_weight = "upto1", 
              edge_color = "upto1", 
              add_new_scale_weights = FALSE)

sf_graph$plot(data = "SpeedLimit",
              newdata = data_on_graph_with_covariates %>% filter(StreetName == "Mission St"),
              vertex_size = 0, 
              edge_width = 1)

plot(predicted.values.nonstat)

sf_graph$plot(data = "speed", edge_width = 0, vertex_size = 0, group = 1:4)


ggplot() + geom_sf(data = to.check, aes(color = "blue"), color = "blue") + 
  geom_sf(data = points[get.index$index,], aes(color = "red"), color = "red") +
  geom_sf(data = balls[80,], aes(color = "black"), color = "black") 

ggplot() + geom_sf(data = to.check, aes(color = "blue"), color = "blue") + 
  geom_sf(data = points[get.index$index,], aes(color = "red"), color = "red")

ggplot() + geom_sf(data = a, aes(color = "blue"), color = "blue") + 
  geom_sf(data = b, aes(color = "red"), color = "red")

ggplot() + geom_sf(data = df, aes(color = "blue"), color = "blue")


ggplot() + geom_sf(data = polygon, aes(color = "blue"), color = "blue") + 
  geom_sf(data = df, aes(color = "red"), color = "red")

(ggplot() + geom_sf(data = polygon, aes(color = "blue"), color = "blue") + 
  geom_sf(data = res, aes(color = "red"), color = "red")) %>% ggplotly()

(ggplot() + geom_sf(data = polygon, aes(color = "blue"), color = "blue") + 
    geom_sf(data = res, aes(color = "red"), color = "red")) %>% ggplotly()


ggplot() + geom_sf(data = points, aes(color = "blue"), color = "blue") + 
  geom_sf(data = chosen.points, aes(color = "red"), color = "red")

ggplot() + geom_sf(data = rf, aes(color = "blue"), color = "blue")




ball = chosen.points[i,2] %>% 
  st_buffer(dist = 100, nQuadSegs = 30)

mapview(ball)



ggplot() + geom_sf(data = ball, aes(color = "blue"), color = "blue")


circle <- st_buffer(st_sfc(st_point(c(x = 0, y = 0)), crs = st_crs(points)), dist = 5, nQuadSegs = 5)

# Plot the circle
plot(circle, main = "Circle in R with SF Package")

circle <- st_buffer(st_sfc(st_point(c(x = 0, y = 0))), dist = 5, nQuadSegs = 500)

# Plot the circle
plot(circle, main = "Circle in R with SF Package")

par(mfrow=c(1,2))

# Plot MSE
plot(distance, mse.stat, main = "MSE", ylim = c(min(mse.nonstat, mse.stat), max(mse.nonstat, mse.stat)),
     type = "l", ylab = "MSE", xlab = "distance in km", col = "black")
lines(distance, mse.nonstat, col = "blue")
legend("topright", legend = c("Stationary", "Non-stationary"), col = c("black", "blue"), lty = 1)

# Plot log-score
plot(distance, -ls.stat, main = "log-score", ylim = c(min(-ls.nonstat, -ls.stat), max(-ls.nonstat, -ls.stat)),
     type = "l", ylab = "log-score", xlab = "distance in km", col = "black")
lines(distance, -ls.nonstat, col = "blue")
legend("topright", legend = c("Stationary", "Non-stationary"), col = c("black", "blue"), lty = 1)



library(ggplot2)
library(gridExtra)
# Create data frames for MSE and log-score
mse_data <- data.frame(distance = distance, stationary = mse.stat, nonstationary = mse.nonstat)
ls_data <- data.frame(distance = distance, stationary = -ls.stat, nonstationary = -ls.nonstat)

# Plot MSE using ggplot2
p1 <- ggplot(mse_data, aes(x = distance)) +
  geom_line(aes(y = stationary, color = "Stationary")) +
  geom_line(aes(y = nonstationary, color = "Non-stationary")) +
  labs(title = "MSE", x = "distance in km", y = "MSE") +
  scale_color_manual(values = c("Stationary" = "black", "Non-stationary" = "blue")) +
  theme_minimal()

# Plot log-score using ggplot2
p2 <- ggplot(ls_data, aes(x = distance)) +
  geom_line(aes(y = stationary, color = "Stationary")) +
  geom_line(aes(y = nonstationary, color = "Non-stationary")) +
  labs(title = "log-score", x = "distance in km", y = "log-score") +
  scale_color_manual(values = c("Stationary" = "black", "Non-stationary" = "blue")) +
  theme_minimal()

# Plot both plots side by side
grid.arrange(p1, p2, ncol = 2)

#


