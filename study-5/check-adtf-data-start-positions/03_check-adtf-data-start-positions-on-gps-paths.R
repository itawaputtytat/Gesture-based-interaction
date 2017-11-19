
# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$db_conn_name <- dbFindConnObj("Study-5")
sett_dat$db_src_name <- "v_adtf_raw_every_100th_row"
sett_dat$col_names$case <- "file_name"
sett_dat$col_names$gps_lon <- "gps_lon"
sett_dat$col_names$gps_lat <- "gps_lat"
# sett_proc$threshold_gps_lon_1 <- 11.640036
# sett_proc$threshold_gps_lat_1 <- 48.075038
sett_proc$threshold_gps_lon_2 <- 11.632194
sett_proc$threshold_gps_lat_2 <- 48.073399
sett_dat$col_names$gps_lon_first <- "gps_lon_first"
sett_dat$col_names$gps_lat_first <- "gps_lat_first"
sett_dat$col_names$gps_lon_last <- "gps_lon_last"
sett_dat$col_names$gps_lat_last <- "gps_lat_last"



# Load data ---------------------------------------------------------------

dat_adtf_raw <- dbGetSrc(sett_dat$db_conn_name, sett_dat$db_src_name)




# Vizualize GPS paths -----------------------------------------------------

plot_gps_paths <- 
  ggplot() + 
  geom_path(data = dat_adtf_raw,
            aes(x = gps_lon,
                y = gps_lat,
                group = file_name),
            alpha = 0.25)

windows(); plot(plot_gps_paths)




# Join trip information ---------------------------------------------------

dat_adtf_raw2 <- 
  left_join(dat_adtf_raw,
            dat)

dat_adtf_raw2$progress <- 100 / dat_adtf_raw2$row_nr_last * dat_adtf_raw2$row_nr
dat_adtf_raw2$progress_alpha = dat_adtf_raw2$progress / 100




# Visualize GPS paths -----------------------------------------------------

plot_gps_paths_trip_nr <- 
  ggplot() + 
  geom_path(data = dat_adtf_raw2,
            aes(x = gps_lon,
                y = gps_lat,
                group = file_name,
                colour = progress,
                alpha = progress_alpha),
            size = 1) + 
            #alpha = 0.25) +
  facet_grid(.~trip_nr_clarified) + 
  geom_point(data = dat,
             aes_string(x = sett_dat$col_names$gps_lon_first,
                        y = sett_dat$col_names$gps_lat_first,
                        group = sett_dat$col_names$case),
             colour = "green4",
             alpha = 0.25) + 
  geom_point(data = dat,
             aes_string(x = sett_dat$col_names$gps_lon_last,
                        y = sett_dat$col_names$gps_lat_last,
                        group = sett_dat$col_names$case),
             colour = "red",
             alpha = 0.25) + 
  scale_colour_continuous(trans = 'reverse')

windows(); plot(plot_gps_paths_trip_nr)





# Check min / max ---------------------------------------------------------

dat_adtf_raw2 <- 
  dat_adtf_raw2 %>% 
  group_by(file_name) %>% 
  mutate(trip_nr_clarified_v2 = trip_nr_clarified) %>% 
  mutate(trip_nr_clarified_v2 = ifelse(trip_nr_clarified_v2 == "2_crash", "2_crash_v1", trip_nr_clarified_v2)) %>% 
  mutate(trip_nr_clarified_v2 = ifelse(trip_nr_clarified_v2 == "2_crash_v1" & min(gps_lon) <= sett_proc$threshold_gps_lon_2, "2_crash_v2", trip_nr_clarified_v2)) %>% 
  mutate(trip_nr_clarified_v2 = ifelse(trip_nr_clarified_v2 == "4_crash", "4_crash_v1", trip_nr_clarified_v2)) %>% 
  mutate(trip_nr_clarified_v2 = ifelse(trip_nr_clarified_v2 == "4_crash_v1" & max(gps_lon) >= sett_proc$threshold_gps_lon_2, "4_crash_v2", trip_nr_clarified_v2)) %>% 
  mutate(trip_nr_clarified_v2 = ifelse(trip_nr_clarified_v2 == "start_N_end_NA", "start_N_end_NA_v1", trip_nr_clarified_v2)) %>% 
  mutate(trip_nr_clarified_v2 = ifelse(trip_nr_clarified_v2 == "start_N_end_NA_v1" & max(gps_lon) >= sett_proc$threshold_gps_lon_2, "start_N_end_NA_v2", trip_nr_clarified_v2))

dat_adtf_raw2_summary <- 
  dat_adtf_raw2 %>% 
  group_by(file_name) %>% 
  summarise_all(.funs = min)



# Visualize GPS paths -----------------------------------------------------

plot_gps_paths_trip_nr <- 
  ggplot() + 
  geom_path(data = dat_adtf_raw2,
            aes(x = gps_lon,
                y = gps_lat,
                group = file_name,
                colour = progress,
                alpha = progress_alpha),
            size = 1) + 
  #alpha = 0.25) +
  geom_point(data = dat_adtf_raw2_summary,
             aes_string(x = sett_dat$col_names$gps_lon_first,
                        y = sett_dat$col_names$gps_lat_first,
                        group = sett_dat$col_names$case),
             colour = "green4",
             alpha = 0.25) + 
  geom_point(data = dat_adtf_raw2_summary,
             aes_string(x = sett_dat$col_names$gps_lon_last,
                        y = sett_dat$col_names$gps_lat_last,
                        group = sett_dat$col_names$case),
             colour = "red",
             alpha = 0.25) + 
  facet_grid(.~trip_nr_clarified_v2) + 
  scale_colour_continuous(trans = 'reverse')

windows(); plot(plot_gps_paths_trip_nr)



# write to dat ------------------------------------------------------------

dbWriteTable(get(sett_dat$db_conn_name), 
             "dat_adtf_raw2_summary", 
             dat_adtf_raw2_summary,
             row.number = F,
             overwrite = T)
