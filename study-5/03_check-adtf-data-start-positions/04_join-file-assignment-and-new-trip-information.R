
# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$db_conn_name <- dbFindConnObj("Study-5")
sett_dat$db_src_name <- "t_adtf_file_assignment"



# Load file assignments ---------------------------------------------------

dat_file_assignment <- dbGetSrc(sett_dat$db_conn_name, sett_dat$db_src_name)




# Join data from anomaly detection ----------------------------------------

dat_file_assignment_v2 <- 
  left_join(dat_file_assignment,
            dat_adtf_raw2_summary %>% 
              select(file_name, trip_nr_clarified_v2))



# Write to database -------------------------------------------------------

dbWriteTable(get(sett_dat$db_conn_name), 
             paste_(sett_dat$db_src_name, "v2_anomalies_clarified"),
             dat_file_assignment_v2,
             overwrite = T,
             row.names = F)
