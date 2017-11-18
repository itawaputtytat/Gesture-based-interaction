dat <- dbGetSrc(dbFindConnObj("Study-5"), db_src_name = "v_adtf_raw_at_100_s")


plot_gps <- ggplot() + 
  geom_point(data = dat,
             aes(x = itrace.gps.Longitude,
                 y = itrace.gps.Latitude,
                 group = subject_id)) + 
  facet_grid(.~trip_nr)

plot(plot_gps)