dat_case <- dbGetQuery(get(sett_dat$db_conn_name), "SELECT * FROM t_adtf_formatted WHERE subject_id = 525")

dat_gps_ref <- dbGetSrc(sett_dat$db_conn_name, "t_coordinates_gps_reference")
dat_gps_crit <- dbGetSrc(sett_dat$db_conn_name, "v_vtd_coordinates_gps_events_start")
dat_gps_crit_events <- dbGetSrc(sett_dat$db_conn_name, "t_coordinates_xy_events")

map <- get_map(location = c(dat_gps_ref$gps_lon - 0.004, dat_gps_ref$gps_lat), zoom = 15, maptype = "satellite")



# Compute event positions -------------------------------------------------

dat_gps_crit_events_start <- 
  dat_gps_crit_events %>% 
  filter(event_reason == "situation start")

dat_gps_crit_action <- dat_gps_crit_events_start
dat_gps_crit_conflict <- dat_gps_crit_events_start

dat_gps_crit_action$gps_lon_action <- 
  conv.xy2gps_byWHOI(dat_gps_crit_action$position_x_action, dat_gps_crit_action$position_y_action,
                     11.63825455, 48.07737816,
                     2.05,
                     0,
                     2,
                     return_only_lon = T)

dat_gps_crit_action$gps_lat_action <- 
  conv.xy2gps_byWHOI(dat_gps_crit_action$position_x_action, dat_gps_crit_action$position_y_action,
                     11.63825455, 48.07737816,
                     2.05,
                     0,
                     2,
                     return_only_lat = T)


dat_gps_crit_conflict$gps_lon_conflict <- 
  conv.xy2gps_byWHOI(dat_gps_crit_conflict$pos_x_conflict, dat_gps_crit_conflict$pos_y_conflict,
                     11.63825455, 48.07737816,
                     2.05,
                     0,
                     2,
                     return_only_lon = T)

dat_gps_crit_conflict$gps_lat_conflict <- 
  conv.xy2gps_byWHOI(dat_gps_crit_conflict$pos_x_conflict, dat_gps_crit_conflict$pos_y_conflict,
                     11.63825455, 48.07737816,
                     2.05,
                     0,
                     2,
                     return_only_lat = T)



# ggmap(map) + 
#   geom_point(data = dat_gps_crit,
#              aes(x = gps_lon_action,
#                  y = gps_lat_action,
#                  color = as.factor(scenario_id),
#                  shape = as.factor(event_id)))


# ggplot() +
#   geom_path(data = dat_case,
#             aes(x = gps_lon,
#                 y = gps_lat)) + 
#   geom_point(data = dat_gps_crit_action %>% 
#                filter(scenario_id == 4),
#              aes(x = gps_lon_action,
#                  y = gps_lat_action,
#                  color = as.factor(scenario_id),
#                  shape = as.factor(event_id))) 



# Test --------------------------------------------------------------------
# s01 = 1170
# s02 = 1480
# s03 = 588
# s04 = 2056

test <- 
  dbGetQuery(get(sett_dat$db_conn_name), 
             "SELECT * FROM t_adtf_s04_e02_gps_dist WHERE subject_id = 525")

test %>% filter(gps_lat == max(gps_lat)) %>% select(dist_m)

plot(test$gps_lon, test$gps_lat)


# AM Viz ------------------------------------------------------------------

sett_dat$scenario_id <- 2

dat_case_am1 <- dbGetQuery(get(sett_dat$db_conn_name), 
                           paste0("SELECT * FROM t_adtf_s0", sett_dat$scenario_id, "_e01_am WHERE subject_id = 525"))
dat_case_am2 <- dbGetQuery(get(sett_dat$db_conn_name), 
                           paste0("SELECT * FROM t_adtf_s0", sett_dat$scenario_id, "_e02_am WHERE subject_id = 525"))
dat_case_am3 <- dbGetQuery(get(sett_dat$db_conn_name), 
                           paste0("SELECT * FROM t_adtf_s0", sett_dat$scenario_id, "_e03_am WHERE subject_id = 525"))

dat_case_am_merged1 <- 
  left_join(dat_case %>% filter(scenario_id == sett_dat$scenario_id),
            dat_case_am1)

dat_case_am_merged2 <- 
  left_join(dat_case %>% filter(scenario_id == sett_dat$scenario_id),
            dat_case_am2)

dat_case_am_merged3 <- 
  left_join(dat_case %>% filter(scenario_id == sett_dat$scenario_id),
            dat_case_am3)

filter_string1 <- paste0("s0", sett_dat$scenario_id, "_e01_dti_m >= -25 & s0", sett_dat$scenario_id, "_e01_dti_m <= 0")
filter_string2 <- paste0("s0", sett_dat$scenario_id, "_e02_dti_m >= -25 & s0", sett_dat$scenario_id, "_e02_dti_m <= 0")
filter_string3 <- paste0("s0", sett_dat$scenario_id, "_e03_dti_m >= -25 & s0", sett_dat$scenario_id, "_e03_dti_m <= 0")

ggplot() +
  geom_path(data = dat_case,
            aes(x = gps_lon,
                y = gps_lat)) + 
  geom_point(data = dat_case_am_merged1 %>% 
               filter_(filter_string1),
             aes(x = gps_lon,
                 y = gps_lat),
             color = "orange") + 
  geom_point(data = dat_case_am_merged2 %>% 
               filter_(filter_string2),
             aes(x = gps_lon,
                 y = gps_lat),
             color = "orange") +
  geom_point(data = dat_case_am_merged3 %>% 
               filter_(filter_string3),
             aes(x = gps_lon,
                 y = gps_lat),
             color = "orange") +
  geom_vline(data = dat_gps_crit_action %>% 
               filter_(paste0("scenario_id == ", sett_dat$scenario_id)),
             aes(xintercept = gps_lon_action),
             col = "green2",
             size = 1) + 
  geom_vline(data = dat_gps_crit_conflict %>% 
               filter_(paste0("scenario_id == ", sett_dat$scenario_id)),
             aes(xintercept = gps_lon_conflict),
             col = "red3") 
  # geom_point(data = dat_gps_crit_action %>% 
  #              filter_(paste0("scenario_id == ", sett_dat$scenario_id)),
  #            aes(x = gps_lon_action,
  #                y = gps_lat_action,
  #                #color = as.factor(scenario_id),
  #                shape = as.factor(event_id)),
  #            color = "green2") +
  # geom_point(data = dat_gps_crit_conflict %>% 
  #              filter_(paste0("scenario_id == ", sett_dat$scenario_id)),
  #            aes(x = gps_lon_conflict,
  #                y = gps_lat_conflict,
  #                #color = as.factor(scenario_id),
  #                shape = as.factor(event_id)),
  #            color = "red")

