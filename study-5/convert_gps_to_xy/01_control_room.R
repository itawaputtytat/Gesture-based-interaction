
test <- conv.GPS2XY(dat_adtf_raw[, c("gps_lon", "gps_lat")],
                    "gps_lon",
                    "gps_lat",
                    11.63825455,
                    48.07737816)

test <- cbind(dat_adtf_raw, test[, c("x", "y")])



# V2 ----------------------------------------------------------------------

test <- dat_adtf_raw
coordinates(test) <- c("gps_lat", "gps_lon")
proj4string(test) <- CRS("+proj=longlat +datum=WGS84")  ## for example

res <- spTransform(test, CRS("+proj=utm +zone=51 ellps=WGS84"))

test <- res %>% data.frame()



# 
# r = 6371000 + alt
# x = r*cos(lat)*cos(lon)
# y = r*cos(lat)*sin(lon)
# z = r*sin(lat)
# 6378137
# 
# 
# dat_adtf_raw$x2 <- 6378137 * cos(dat_adtf_raw$gps_lat*pi/180) * cos(dat_adtf_raw$gps_lon*pi/180)
# dat_adtf_raw$y2 <- 6378137 * cos(dat_adtf_raw$gps_lat*pi/180) * sin(dat_adtf_raw$gps_lon*pi/180)
# 
# 
# 
# long1 = -71.02; lat1 = 42.33;
# long2 = -73.94; lat2 = 40.66;
# 
# lat1 *=pi/180;
# lat2 *=pi/180;
# long1*=pi/180;
# long2*=pi/180;
# 
# dlong = (long2 - long1);
# dlat  = (lat2 - lat1);
# 
# // Haversine formula:
#   R = 6371;
# a = sin(dlat/2)*sin(dlat/2) + cos(lat1)*cos(lat2)*sin(dlong/2)*sin(dlong/2)
# c = 2 * atan2( sqrt(a), sqrt(1-a) );
# d = R * c;
# 

test_uniques <- dat_adtf_raw2_summary$
  



# V3 ----------------------------------------------------------------------

param_angle <- -1.85

test <- dat_adtf_raw
test$x <- 0
test$y <- 0
test[, c("x", "y")] <- 
  apply(test[, c("gps_lon", "gps_lat")], 1, function(x) {
  result <- translate(x["gps_lon"], x["gps_lat"],
                      11.63825455, 48.07737816,
                      #rotation_angle_degs = 0)
                      rotation_angle_degs = param_angle)
                      #rotation_angle_degs = 17) ## Taxiway will be parallel to X
}) %>% bind_rows()


set.seed(42)
ggplot() + 
  ## Driver path
  ggtitle(param_angle) + 
  geom_path(data = test %>% 
              filter(file_name %in% sample(dat_adtf_raw2_summary$file_name, 10)),
            aes(x = x,
            y = y,
            group = file_name),
            size = 1) + 
  
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
