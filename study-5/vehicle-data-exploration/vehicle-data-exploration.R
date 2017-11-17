dat <- 
  read.csv2(choose.files(), 
            skip = 13, 
            sep = ",",
            dec = ".",
            header = T)

library(ggplot2)


plot1 <- 
  ggplot() + 
  geom_path(data = dat,
            aes(x = Time,
                y = can.angez_Geschw,
                color = "v_can")) +
  geom_path(data = dat,
            aes(x = Time,
                y = abs(itrace.speed.v_y) * 3.6,
                color = "v_itrace_y")) + 
  scale_colour_manual(name = "Speed",
                      values = c("black", "red")) + 
  coord_cartesian(xlim = c(0, 1000)) + 
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1))
plot1  

plot2 <- 
  ggplot() + 
  geom_path(data = dat,
            aes(x = Time,
                y = can.Laengsbeschleunigung,
                color = "acc_lon")) +
  geom_path(data = dat,
            aes(x = Time,
                y = itrace.acc.acc_z_komp,
                color = "acc_lon_itrace_z")) + 
  scale_colour_manual(name = "Längsbeschleunigung",
                      values = c("black", "red")) +
  coord_cartesian(xlim = c(0, 1000),
                  ylim = c(-5, 5)) + 
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1))
plot2

plot3 <- 
  ggplot() + 
  geom_path(data = dat,
            aes(x = Time,
                y = can.Querbeschleunigung,
                color = "acc_lat")) + 
  geom_path(data = dat,
            aes(x = Time,
                y = itrace.acc.acc_y_komp,
                color = "acc_lat_itrace_y"),
            alpha = 0.5) + 
  scale_colour_manual(name = "Querbeschleunigung",
                      values = c("black", "red")) + 
  coord_cartesian(xlim = c(0, 1000),
                  ylim = c(-5, 5)) + 
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1))
plot3

library(gridExtra)
grid.arrange(plot1, plot2, plot3)
