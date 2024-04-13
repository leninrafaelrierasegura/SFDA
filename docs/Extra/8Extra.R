toplot = final1 %>% filter(.edge_number %in% c(10843))
aa = sign_data %>% filter(edge_number == 10843)

plot(toplot$.distance_on_edge*as.numeric(edge_length[10843]), toplot$cov_bus, ylim = c(0,1), xlim = c(0, as.numeric(edge_length[10843])))
points(aa$distance_on_edge*as.numeric(edge_length[10843]), rep(0,5), col = "red")


v = vertex_matrix[10843,]

which(vertex_matrix[,1] %in% v[1])
which(vertex_matrix[,1] %in% v[2])
which(vertex_matrix[,2] %in% v[1])
which(vertex_matrix[,2] %in% v[2])

# 10843, 3317, 3312, 10844, 3310, 8892, 3616

toplot = final1 %>% filter(.edge_number %in% c(10844))
aa = sign_data %>% filter(edge_number == 10844)
plot(toplot$.distance_on_edge*as.numeric(edge_length[10844]), toplot$cov_bus, ylim = c(0,1), xlim = c(0, as.numeric(edge_length[10844])))
points(aa$distance_on_edge*as.numeric(edge_length[10844]), rep(0,2), col = "red")


toplot = final1 %>% filter(.edge_number %in% c(3616))
aa = sign_data %>% filter(edge_number == 3616)
plot(toplot$.distance_on_edge*as.numeric(edge_length[3616]), toplot$cov_bus, ylim = c(0,1), xlim = c(0, as.numeric(edge_length[3616])))
points(aa$distance_on_edge*as.numeric(edge_length[3616]), rep(0,1), col = "red")




toplot2 = final1 %>% filter(.edge_number %in% c(10843, 10844, 3616))

library(ggplot2)
ggplot(toplot2, aes(x = .coord_x, y = .coord_y, colour = cov_bus))+ geom_point()
library(plotly)
plot_ly(data = toplot2, x = ~.coord_x, y = ~.coord_y, z = ~cov_bus, type = "scatter3d", mode = "markers")


freq_table <- table(sign_data$edge_number)
names(freq_table)[which.max(freq_table)]

# 9790 has 16 signs


# ---------------------------------------------------------------------------------------------------------------------------------


toplot_d = final1 %>% filter(.edge_number %in% c(536))
aa = sign_data %>% filter(edge_number == 536)

plot(toplot_d$.distance_on_edge*as.numeric(edge_length[536]), toplot_d$cov_bus, ylim = c(0,1), xlim = c(0, as.numeric(edge_length[536])))


v = vertex_matrix[536,]

which(vertex_matrix[,1] %in% v[1])
which(vertex_matrix[,1] %in% v[2])
which(vertex_matrix[,2] %in% v[1])
which(vertex_matrix[,2] %in% v[2])

# 341, 21, 387

toplot_i = final1 %>% filter(.edge_number %in% c(341))
aa = sign_data %>% filter(edge_number == 341)
plot(toplot_i$.distance_on_edge*as.numeric(edge_length[341]), toplot_i$cov_bus, ylim = c(0,1), xlim = c(0, as.numeric(edge_length[341])))
points(aa$distance_on_edge*as.numeric(edge_length[341]), rep(0,1), col = "red")


dd = c(toplot_i$.distance_on_edge*as.numeric(edge_length[341]), as.numeric(edge_length[341]) + toplot_d$.distance_on_edge*as.numeric(edge_length[536]))
fdd = c(toplot_i$cov_bus, toplot_d$cov_bus)

plot(dd,fdd)


toplot2 = final1 %>% filter(.edge_number %in% c(536, 341))

library(ggplot2)
ggplot(toplot2, aes(x = .coord_x, y = .coord_y, colour = cov_bus))+ geom_point()
library(plotly)
plot_ly(data = toplot2, x = ~.coord_x, y = ~.coord_y, z = ~cov_bus, type = "scatter3d", mode = "markers")


freq_table <- table(sign_data$edge_number)
names(freq_table)[which.max(freq_table)]


