#load(here::here("Models_output/Highways22.RData"))


## plot results
par(mfrow = c(1,2), family = "Palatino")

# Plot MSE
plot(distance, mse.statnu0.5, ylim = c(min(mse.nonstatnu0.5, 
                                                         mse.statnu0.5, 
                                                         mse.nonstatnu1.5, 
                                                         mse.statnu1.5, 
                                                         mse.nonstatnuest, 
                                                         mse.statnuest), 
                                                     max(mse.nonstatnu0.5, 
                                                         mse.statnu0.5, 
                                                         mse.nonstatnu1.5, 
                                                         mse.statnu1.5, 
                                                         mse.nonstatnuest, 
                                                         mse.statnuest)),
     type = "l", ylab = "MSE", xlab = "distance in km", col = "blue", lty = 1, lwd = 2)

lines(distance, mse.nonstatnu0.5, col = "red", lty = 2, lwd = 2)
lines(distance, mse.statnu1.5, col = "black", lty = 1, lwd = 2)
lines(distance, mse.nonstatnu1.5, col = "green", lty = 2, lwd = 2)
lines(distance, mse.statnuest, col = "darkorange", lty = 1, lwd = 2)
lines(distance, mse.nonstatnuest, col = "skyblue", lty = 2, lwd = 2)

legend("bottomright", legend = c("Statnu0.5", 
                                 "Nonstatnu0.5", 
                                 "Statnu1.5", 
                                 "Nonstatnu1.5", 
                                 "Statnuest", 
                                 "Nonstatnuest"), col = c("blue", 
                                                          "red", 
                                                          "black", 
                                                          "green", 
                                                          "darkorange", 
                                                          "skyblue"), lty = c(1,2,1,2,1,2), lwd = 2)


# Plot log-score
plot(distance, -ls.statnu0.5, ylim = c(min(-ls.nonstatnu0.5, 
                                                               -ls.statnu0.5, 
                                                               -ls.nonstatnu1.5, 
                                                               -ls.statnu1.5, 
                                                               -ls.nonstatnuest, 
                                                               -ls.statnuest), 
                                                           max(-ls.nonstatnu0.5, 
                                                               -ls.statnu0.5, 
                                                               -ls.nonstatnu1.5, 
                                                               -ls.statnu1.5, 
                                                               -ls.nonstatnuest, 
                                                               -ls.statnuest)),
     type = "l", ylab = " negative log-score", xlab = "distance in km", col = "blue", lty = 1, lwd = 2)

lines(distance, -ls.nonstatnu0.5, col = "red", lty = 2, lwd = 2)
lines(distance, -ls.statnu1.5, col = "black", lty = 1, lwd = 2)
lines(distance, -ls.nonstatnu1.5, col = "green", lty = 2, lwd = 2)
lines(distance, -ls.statnuest, col = "darkorange", lty = 1, lwd = 2)
lines(distance, -ls.nonstatnuest, col = "skyblue", lty = 2, lwd = 2)

legend("bottomright", legend = c("Statnu0.5", 
                                 "Nonstatnu0.5", 
                                 "Statnu1.5", 
                                 "Nonstatnu1.5", 
                                 "Statnuest", 
                                 "Nonstatnuest"), col = c("blue", 
                                                          "red", 
                                                          "black", 
                                                          "green", 
                                                          "darkorange", 
                                                          "skyblue"), lty = c(1,2,1,2,1,2), lwd = 2)
