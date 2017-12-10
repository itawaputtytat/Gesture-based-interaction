dat_gps <- 
  dat_adtf_raw %>% 
  filter(file_name == "Y_853313_20170630_160746_export.csv")

map <- get_map(location = c(dat_gps$gps_lon[45], dat_gps$gps_lat[45]), zoom = 16, maptype = "satellite")
ggmap(map) + 
  geom_path(data = dat_gps,
            aes(x = gps_lon,
                y = gps_lat),
            color = "yellow")
