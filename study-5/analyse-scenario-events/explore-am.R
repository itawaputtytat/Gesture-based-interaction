
# Preparatory settings ----------------------------------------------------

sett_proc <- c()
sett_proc$scenario_id <- as.numeric(substr(sett_query$sxx_exx, 3, 3))
sett_proc$event_id <- as.numeric(substr(sett_query$sxx_exx, 7, 7))



# Load reference position -------------------------------------------------

dat_gps_reference <- 
  dbGetSrc(sett_query$db_conn_name,
           "v_vtd_coordinates_gps_events_start") %>% 
  filter(scenario_id == sett_proc$scenario_id & 
           event_id == sett_proc$event_id)



# Load map data for reference position ------------------------------------

map <- 
  get_map(c(dat_gps_reference$gps_lon, dat_gps_reference$gps_lat),
          maptype = "satellite", 
          zoom = 17)

ggmap(map) + 
  geom_point(data = dat_gps_reference,
             aes(x = gps_lon,
                 y = gps_lat),
             color = "white") + 
  geom_point(data = dat_study5_t_adtf_sxx_exx_exx_full_intrpld %>% 
               filter(sxx_exx_dti_m == 0) %>% 
               group_by(case) %>% 
               summarize_all("max"),
             aes(x = gps_lon,
                 y = gps_lat,
                 group = case),
             color = "yellow",
             alpha = 0.5) + 
  geom_point(data = dat_study5_t_adtf_sxx_exx_exx_full_intrpld %>% 
               filter(!usable) %>% 
               filter(sxx_exx_dti_m == 0) %>% 
               group_by(case) %>% 
               summarize_all("max"),
             aes(x = gps_lon,
                 y = gps_lat,
                 group = case),
             color = "red",
             alpha = 0.5)




# Load exemplary gps path -------------------------------------------------

dat_gps <-
  dbGetQuery(get(sett_query$db_conn_name),
             #"SELECT * FROM t_adtf_formatted WHERE file_name = 'Y_853313_20170619_083022_export.csv'")
             "SELECT * FROM t_adtf_s00_e01_full WHERE subject_id = 526")
             #"SELECT * FROM t_adtf_s00_e02_full WHERE subject_id = 512")
             #"SELECT * FROM t_adtf_s00_e02_gps_dist WHERE subject_id = 512")
             

# dat_gps <- 
#   dat_gps %>% 
#   mutate(going_to = ifelse(gps_lon > lag(gps_lon, 50), 'east', 'west'))

plot_gps <-
  ggplot() +
  geom_point(data = dat_gps,
            aes(x = gps_lon,
                y = gps_lat,
                color = going_east)) 


plot(plot_gps)
# dat_test <- 
#   dat_gps %>% 
#   filter(scenario_id == 1) %>% 
#   group_by(subject_id) %>% 
#   mutate(gps_lon_diff = gps_lon - lag(gps_lon, 25)) %>% 
#   mutate(gps_lat_diff = gps_lat - lag(gps_lat, 25)) %>% 
#   mutate(gps_diff_ratio = gps_lon_diff / gps_lat_diff) %>% 
#   mutate(going_to2 = ifelse(gps_diff_ratio <= 2.5, "north", "derp"))
#   #filter(time_s <= 76)
# 
# ggplot() + 
#   geom_line(data = dat_test,
#             aes(x = time_s,
#                 y = gps_diff_ratio)) + 
#   geom_line(data = dat_test,
#             aes(x = time_s,
#                 y = speed_kmh)) + 
#   coord_cartesian(xlim = c(50, 75), ylim = c(-5, 5))
#   
# plot_test <-
#   ggplot() +
#   geom_path(data = dat_test,
#             aes(x = gps_lon,
#                 y = gps_lat,
#                 group = subject_id,
#                 color = going_to2))
# 
# plot(plot_test)


# Scenario detection ------------------------------------------------------

# dat_gps_reference <- 
#   dbGetSrc(sett_query$db_conn_name, "t_coordinates_gps_reference")
# 
# 
# 
# plot_test <-
#   ggplot() +
#   geom_path(data = dat_gps,
#             aes(x = gps_lon,
#                 y = gps_lat,
#                 group = subject_id,
#                 color = going_to)) + 
#   geom_point(data = dat_gps %>% filter(is.na(going_to)),
#              aes(x = gps_lon,
#                  y = gps_lat),
#              color = "blue")

plot_test <- 
  plot_gps + 
  #ggplot() +
  geom_path(data = dat_study5_t_adtf_sxx_exx_exx_full_intrpld,
            #%>% filter(subject_id != 509),
            #data = dat_study5_t_adtf_sxx_exx_exx_full_intrpld %>% filter(subject_id %in% finder),
            aes(x = gps_lon,
                y = gps_lat,
                group = subject_id),
            color = "green4",
            size = 1,
            alpha = 0.35) #+
  #coord_cartesian(xlim = c(11.63575, 11.641), ylim = c(48.0745, 48.07525))

#windows(); 
plot(plot_test)

finder <- dat_study5_t_adtf_sxx_exx_exx_full_intrpld %>% filter(gps_lon >= 11.642)
finder <- unique(finder$subject_id)
