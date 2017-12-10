
# V1: Translate coordinates using Harvestine ------------------------------

## Load data for testing
test <- dat_adtf_raw

## Translate coordinates
test_temp <- conv.GPS2XY(dat_adtf_raw[, c("gps_lon", "gps_lat")],
                    "gps_lon",
                    "gps_lat",
                    11.63825455,
                    48.07737816)

## Complete data
test <- cbind(test, test_temp[, c("x", "y")])



# V2: Translate coordinates using WHOI ------------------------------------

## Load data for testing
test2 <- dat_adtf_raw

## Settings for rotation angle
param_angle <- 0 # no rotation
param_angle <- -1.85 # taxiway will be parallel to x
param_angle <- -1.85

## Create new variables
test2$x <- 0
test2$y <- 0

## Translate coordinates
test2[, c("x2", "y2")] <- 
  apply(test2[, c("gps_lon", "gps_lat")], 1, function(x) {
    result <- 
      conv.gps2xy_byWHOI(x["gps_lon"], x["gps_lat"],
                         11.63825455, 48.07737816, # reference coordinates
                         rotation_angle_deg = param_angle)
  }) %>% bind_rows()



# Visualization -----------------------------------------------------------

set.seed(42)

## Plot drivers paths
plot_paths <- 
  ggplot() + 
  ggtitle(param_angle) + 
  
  geom_path(data = test %>% 
              filter(file_name %in% sample(dat_adtf_raw2_summary$file_name, 10)),
            aes(x = x,
                y = y,
                group = file_name),
            color = "grey85",
            size = 1) +
  
  geom_path(data = test2 %>% 
              filter(file_name %in% sample(dat_adtf_raw2_summary$file_name, 10)),
            aes(x = pos_x,
                y = pos_y,
                group = file_name),
            size = 1)
  
plot(plot_paths)

## Add additional points

plot_paths_points <- 
  plot_paths + 
  ## Parking vehicle
  geom_point(data = data.frame(X=1.4132504272460938e+02, Y=-2.5997845458984375e+02),
             aes(x = X,
                 y = Y),
             color= "red") + 
  geom_point(data = data.frame(X=1.0045167541503906e+02, Y=-2.7543286132812500e+02),
             aes(x = X,
                 y = Y),
             color= "blue") +
  
  ## Vehicle at intersection
  geom_point(data = data.frame(X=-4.3962503393970820e+02, Y=-4.2817026386490869e+02),
             aes(x = X,
                 y = Y),
             color= "red") + 
    geom_point(data = data.frame(X=-3.8804388427734375e+02, Y=-4.3699484252929688e+02),
               aes(x = X,
                   y = Y),
               color= "blue") + 
    
  ## Preceding vehicle
  geom_point(data = data.frame(X=-6.1787453802710695e+02, Y=-5.2793164934619699e+02),
             aes(x = X,
                 y = Y),
             color= "red") +

  ## Crossing cyclist
  geom_point(data = data.frame(X=8.6991783142089844e+01, Y=-2.7988125610351562e+02),
             aes(x = X,
                 y = Y),
             color= "red") + 

  ## Evasing vehicle
  geom_point(data = data.frame(X=-5.0457767916741614e+02, Y=-4.8502175737738213e+02),
             aes(x = X,
                 y = Y),
             color= "red")


plot(plot_paths_points)


events <- dbGetSrc(dbFindConnObj("Study-5"), "t_coordinates_xy_events")
events <- events %>% mutate(row_nr = row_number())


plot_paths + 
  geom_point(data = events,
             aes(x = pos_x_conflict,
                 y = pos_y_conflict,
                 color = event_type)) + 
  scale_color_manual(values = c("red1", "green3", "yellow3"))
