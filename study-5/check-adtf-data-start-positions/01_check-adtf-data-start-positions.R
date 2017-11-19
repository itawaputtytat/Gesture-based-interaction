
# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$db_conn_name <- dbFindConnObj("Study-5")
sett_dat$db_src_name <- "t_adtf_raw_first_and_last_row"
sett_dat$col_names$case <- "file_name"
sett_dat$col_names$gps_lon_first <- "gps_lon_first"
sett_dat$col_names$gps_lat_first <- "gps_lat_first"
sett_dat$col_names$gps_lon_last <- "gps_lon_last"
sett_dat$col_names$gps_lat_last <- "gps_lat_last"

sett_proc <- c()
sett_proc$threshold_gps_lon_1 <- 11.640036
sett_proc$threshold_gps_lat_1 <- 48.075038
sett_proc$threshold_gps_lon_2 <- 11.632194
sett_proc$threshold_gps_lat_2 <- 48.073399



# Load data ---------------------------------------------------------------

dat <- dbGetSrc(sett_dat$db_conn_name, db_src_name = sett_dat$db_src_name)
dat$trip_nr[dat$trip_nr == "NA"] <- NA

# ## Copy trip nr
# dat$trip_nr_clean <- dat$trip_nr
# dat$trip_nr_clean[dat$trip_nr_clean == "0"] <- "0-N"
# dat$trip_nr_clean[dat$trip_nr_clean == "1"] <- "1-E"
# dat$trip_nr_clean[dat$trip_nr_clean == "2"] <- "2-N"
# dat$trip_nr_clean[dat$trip_nr_clean == "3"] <- "3-W"
# dat$trip_nr_clean[dat$trip_nr_clean == "4"] <- "4-N"



# Identify start ----------------------------------------------------------

dat$pos_start <- "start_NA"

## Start N
dat <- 
  dat %>% 
  mutate(pos_start = 
           ifelse(gps_lon_first < sett_proc$threshold_gps_lon_1 &
                     gps_lat_first > sett_proc$threshold_gps_lat_1,
                  "start_N",
                  pos_start))

## Start east
dat <-
  dat %>%
  mutate(pos_start =
           ifelse(gps_lon_first > sett_proc$threshold_gps_lon_1 &
                     gps_lat_first > sett_proc$threshold_gps_lat_1,
                  "start_E",
                  pos_start))

## Start west
dat <-
  dat %>%
  mutate(pos_start =
           ifelse(gps_lon_first < sett_proc$threshold_gps_lon_2 &
                     gps_lat_first < sett_proc$threshold_gps_lat_2,
                  "start_W",
                  pos_start))



# Identify end ------------------------------------------------------------

dat$pos_end <- "end_NA"

## End N
dat <- 
  dat %>% 
  mutate(pos_end = 
           ifelse(gps_lon_last < sett_proc$threshold_gps_lon_1 &
                    gps_lat_last > sett_proc$threshold_gps_lat_1,
                  "end_N",
                  pos_end))

## End east
dat <-
  dat %>%
  mutate(pos_end =
           ifelse(gps_lon_last > sett_proc$threshold_gps_lon_1 &
                    gps_lat_last > sett_proc$threshold_gps_lat_1,
                  "end_E",
                  pos_end))

## End west
dat <-
  dat %>%
  mutate(pos_end =
           ifelse(gps_lon_last < sett_proc$threshold_gps_lon_2 &
                    gps_lat_last < sett_proc$threshold_gps_lat_2,
                  "end_W",
                  pos_end))



# Visualize start / end categories ----------------------------------------

plot_gps <- 
  ggplot() + 
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
  facet_grid(pos_start~pos_end) + 
  geom_vline(xintercept = sett_proc$threshold_gps_lon_1, color = "red4") +
  geom_hline(yintercept = sett_proc$threshold_gps_lat_1, color = "red4") +
  geom_vline(xintercept = sett_proc$threshold_gps_lon_2, color = "green4") +
  geom_hline(yintercept = sett_proc$threshold_gps_lat_2, color = "green4")


windows(); plot(plot_gps)



# Combine info to clean trips ---------------------------------------------

dat <- 
  dat %>% 
  mutate(trip_nr_complete = paste_(pos_start, pos_end)) %>% 
  mutate(trip_nr_complete = ifelse(pos_start == "start_N" & pos_end == "end_N", 0, trip_nr_complete)) %>% 
  mutate(trip_nr_complete = ifelse(pos_start == "start_N" & pos_end == "end_E", 1, trip_nr_complete)) %>% 
  mutate(trip_nr_complete = ifelse(pos_start == "start_E" & pos_end == "end_N", 2, trip_nr_complete)) %>% 
  mutate(trip_nr_complete = ifelse(pos_start == "start_N" & pos_end == "end_W", 3, trip_nr_complete)) %>% 
  mutate(trip_nr_complete = ifelse(pos_start == "start_W" & pos_end == "end_N", 4, trip_nr_complete))



# Visualize clean trips ---------------------------------------------------

plot_gps <- 
  ggplot() + 
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
  facet_grid(.~trip_nr_complete) + 
  geom_vline(xintercept = sett_proc$threshold_gps_lon_1, color = "red4") +
  geom_hline(yintercept = sett_proc$threshold_gps_lat_1, color = "red4") +
  geom_vline(xintercept = sett_proc$threshold_gps_lon_2, color = "green4") +
  geom_hline(yintercept = sett_proc$threshold_gps_lat_2, color = "green4")


windows(); plot(plot_gps)


# Clarify crashes ---------------------------------------------------------

dat <- 
  dat %>% 
  mutate(trip_nr_clarified = trip_nr_complete) %>% 
  mutate(trip_nr_clarified = ifelse(trip_nr_complete == "start_E_end_E", "start_stop_anomaly", trip_nr_clarified)) %>% 
  mutate(trip_nr_clarified = ifelse(trip_nr_complete == "start_W_end_W", "start_stop_anomaly", trip_nr_clarified)) %>% 
  mutate(trip_nr_clarified = ifelse(trip_nr_complete == "start_E_end_NA", "2_crash", trip_nr_clarified)) %>% 
  mutate(trip_nr_clarified = ifelse(trip_nr_complete == "start_W_end_E",  "4_crash", trip_nr_clarified)) %>% 
  mutate(trip_nr_clarified = ifelse(trip_nr_complete == "start_W_end_NA", "4_crash", trip_nr_clarified))



# Visualize clean trips and crashes ---------------------------------------

plot_gps <- 
  ggplot() + 
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
  facet_grid(.~trip_nr_clarified) + 
  geom_vline(xintercept = sett_proc$threshold_gps_lon_1, color = "red4") +
  geom_hline(yintercept = sett_proc$threshold_gps_lat_1, color = "red4") +
  geom_vline(xintercept = sett_proc$threshold_gps_lon_2, color = "green4") +
  geom_hline(yintercept = sett_proc$threshold_gps_lat_2, color = "green4")


windows(); plot(plot_gps)








# 
# # fdsfdsf -----------------------------------------------------------------
# 
# 
# 
# dat <-
#   dat %>%
#   mutate(trip_nr_v2_by_area =
#            ifelse(is.na(trip_nr_v2_by_area) &
#                     gps_lon_first > sett_proc$threshold_gps_lon_1 &
#                     gps_lat_first > sett_proc$threshold_gps_lat_1 &
#                     gps_lon_last > sett_proc$threshold_gps_lon_1 &
#                     gps_lat_last > sett_proc$threshold_gps_lat_1,
#                   "2-2",
#                   trip_nr_v2_by_area))
# 
# 
# dat <-
#   dat %>%
#   mutate(trip_nr_v2_by_area =
#            ifelse(is.na(trip_nr_v2_by_area) &
#                     gps_lon_first < sett_proc$threshold_gps_lon_2 &
#                     gps_lat_first < sett_proc$threshold_gps_lat_2 &
#                     gps_lon_last < sett_proc$threshold_gps_lon_2 &
#                     gps_lat_last < sett_proc$threshold_gps_lat_2,
#                   "4-4",
#                   trip_nr_v2_by_area))
# 
# 
# dat <- 
#   dat %>% 
#   mutate(trip_nr_v2_by_area = 
#            ifelse(is.na(trip_nr_v2_by_area) &
#                     gps_lon_last > sett_proc$threshold_gps_lon_1 &
#                     gps_lat_last > sett_proc$threshold_gps_lat_1,
#                   "X-E",
#                   trip_nr_v2_by_area))
# 
# dat <-
#   dat %>%
#   mutate(trip_nr_v2_by_area =
#            ifelse(is.na(trip_nr_v2_by_area) &
#                     gps_lon_last < sett_proc$threshold_gps_lon_1 &
#                     gps_lat_last > sett_proc$threshold_gps_lat_1,
#                   "X-N",
#                   trip_nr_v2_by_area))
# 
# 
# dat <-
#   dat %>%
#   mutate(trip_nr_v2_by_area =
#            ifelse(is.na(trip_nr_v2_by_area) &
#                     gps_lon_last < sett_proc$threshold_gps_lon_2 &
#                     gps_lat_last < sett_proc$threshold_gps_lat_2,
#                   "X-W",
#                   trip_nr_v2_by_area))
# 
# 
# 
# ## Group cases into -1, if starting positions are located in top left area
# # dat <- 
# #   dat %>% 
# #   mutate(trip_nr_v2_by_area = trip_nr) %>% 
# #   mutate(trip_nr_v2_by_area = 
# #            ifelse(is.na(trip_nr_v2_by_area) &
# #                     gps_lon_first < sett_proc$threshold_gps_lon_1 &
# #                     gps_lat_first > sett_proc$threshold_gps_lat_1,
# #                   -1,
# #                   trip_nr_v2_by_area))
# 
# 
# 
# # Code identification strategy --------------------------------------------
# 
# dat <- 
#   dat %>% 
#   mutate(was_identified = 
#            ifelse(is.na(trip_nr) & !is.na(trip_nr_v2_by_area),
#                   "1) identified by starting area",
#                   "already correct"))
# 
# 
# 
# # Visualize results: Identification by area -------------------------------
# 
# plot_gps_area <- 
#   ggplot() + 
#   geom_point(data = dat,
#              aes_string(x = sett_dat$col_names$gps_lon_first,
#                         y = sett_dat$col_names$gps_lat_first,
#                         group = "file_name",
#                         color = "was_identified"),
#              alpha = 0.35) + 
#   # geom_point(data = dat,
#   #            aes_string(x = sett_dat$col_names$gps_lon_first,
#   #                       y = sett_dat$col_names$gps_lat_first,
#   #                       group = "file_name",
#   #                       shape = "was_identified"),
#   #            colour = "green4",
#   #            alpha = 0.25) + 
#   # geom_point(data = dat,
#   #            aes_string(x = sett_dat$col_names$gps_lon_last,
#   #                       y = sett_dat$col_names$gps_lat_last,
#   #                       group = "file_name",
#   #                       shape = "was_identified"),
#   #            colour = "red",
#   #            alpha = 0.25) +
#   facet_grid(trip_nr_v2_by_area~.) + 
#   geom_vline(xintercept = sett_proc$threshold_gps_lon_1, color = "red") +
#   geom_hline(yintercept = sett_proc$threshold_gps_lat_1, color = "red") +
#   geom_vline(xintercept = sett_proc$threshold_gps_lon_2, color = "green4") +
#   geom_hline(yintercept = sett_proc$threshold_gps_lat_2, color = "green4") 
# 
# plot(plot_gps_area)
# 
# 
# 
# # GPS start stop ----------------------------------------------------------
# 
# 
# 
# plot_gps <- 
#   ggplot() + 
#   geom_point(data = dat,
#              aes_string(x = sett_dat$col_names$gps_lon_first,
#                         y = sett_dat$col_names$gps_lat_first,
#                         group = sett_dat$col_names$case),
#              colour = "green4",
#              alpha = 0.25) + 
#   geom_point(data = dat,
#              aes_string(x = sett_dat$col_names$gps_lon_last,
#                         y = sett_dat$col_names$gps_lat_last,
#                         group = sett_dat$col_names$case),
#              colour = "red",
#              alpha = 0.25) + 
#   facet_grid(.~trip_nr_v2_by_area) + 
#   geom_vline(xintercept = sett_proc$threshold_gps_lon_1, color = "red4") +
#   geom_hline(yintercept = sett_proc$threshold_gps_lat_1, color = "red4") +
#   geom_vline(xintercept = sett_proc$threshold_gps_lon_2, color = "green4") +
#   geom_hline(yintercept = sett_proc$threshold_gps_lat_2, color = "green4")
# 
# 
# windows(); plot(plot_gps)
# 
# 
# 
# 
# 
# # Identify -1 using previous value = 2 ------------------------------------
# 
# ## If previous value == 2, then trip should be probably 3
# dat <- 
#   dat %>% 
#   group_by(subject_id) %>% 
#   mutate(trip_nr_v3_by_prev_2 = trip_nr_v2_by_area) %>% 
#   mutate(trip_nr_v3_by_prev_2 = 
#            ifelse(trip_nr_v3_by_prev_2 == -1 & 
#                     lag(trip_nr_v2_by_area == 2, default = NA),
#                   3, 
#                   trip_nr_v3_by_prev_2)) %>% 
#   mutate(trip_nr_v3_by_prev_2 = 
#            ifelse(is.na(trip_nr_v3_by_prev_2), 
#                   trip_nr_v2_by_area, 
#                   trip_nr_v3_by_prev_2)) %>% 
#   mutate(was_identified = was_identified) %>% 
#   mutate(was_identified = 
#            ifelse(trip_nr_v2_by_area == -1 &
#                     trip_nr_v2_by_area != trip_nr_v3_by_prev_2,
#                   "2) identifitied 3 by previous value = 2",
#                   was_identified))
# 
# 
# 
# # Visualize results: Identify -1 using previous = 2 -----------------------
# 
# plot_gps_prev_2 <- 
#   ggplot() + 
#   geom_point(data = dat,
#              aes_string(x = sett_dat$col_names$gps_lon_first,
#                         y = sett_dat$col_names$gps_lat_first,
#                         group = "file_name",
#                         color = "was_identified"),
#              alpha = 0.35) + 
#   facet_grid(trip_nr_v3_by_prev_2~.) + 
#   geom_vline(xintercept = sett_proc$threshold_gps_lon_1, color = "red") +
#   geom_hline(yintercept = sett_proc$threshold_gps_lat_1, color = "red") +
#   geom_vline(xintercept = sett_proc$threshold_gps_lon_2, color = "green4") +
#   geom_hline(yintercept = sett_proc$threshold_gps_lat_2, color = "green4")
# 
# 
# plot(plot_gps_prev_2)
# 
# 
# 
# 
# 
# # Identify 1 using next value = 2 -----------------------------------------
# 
# dat <- 
#   dat %>% 
#   group_by(subject_id) %>% 
#   mutate(trip_nr_v4_by_next_2 = trip_nr_v3_by_prev_2) %>% 
#   mutate(trip_nr_v4_by_next_2 = 
#            ifelse(trip_nr_v4_by_next_2 == -1 & 
#                     lead(trip_nr_v3_by_prev_2 == 2, default = NA),
#                   1, 
#                   trip_nr_v4_by_next_2)) %>% 
#   mutate(trip_nr_v4_by_next_2 = 
#            ifelse(is.na(trip_nr_v4_by_next_2), 
#                   trip_nr_v3_by_prev_2, 
#                   trip_nr_v4_by_next_2)) %>% 
#   mutate(was_identified = 
#            ifelse(trip_nr_v3_by_prev_2 == -1 &
#                     trip_nr_v3_by_prev_2 != trip_nr_v4_by_next_2,
#                   "3) identifitied 1 by next value = 2",
#                   was_identified))
# 
# 
# 
# # Visualize results: Identify 1 using next = 2 ----------------------------
# 
# plot_gps_next_2 <- 
#   ggplot() + 
#   geom_point(data = dat,
#              aes_string(x = sett_dat$col_names$gps_lon_first,
#                         y = sett_dat$col_names$gps_lat_first,
#                         group = "file_name",
#                         color = "was_identified"),
#              alpha = 0.35) + 
#   facet_grid(trip_nr_v4_by_next_2~.) + 
#   geom_vline(xintercept = sett_proc$threshold_gps_lon_1, color = "red") +
#   geom_hline(yintercept = sett_proc$threshold_gps_lat_1, color = "red") +
#   geom_vline(xintercept = sett_proc$threshold_gps_lon_2, color = "green4") +
#   geom_hline(yintercept = sett_proc$threshold_gps_lat_2, color = "green4")
# 
# plot(plot_gps_next_2)
# 
# 
# 
# # Visualize results: Identify 0 using next = 1 ----------------------------
# 
# dat <- 
#   dat %>% 
#   group_by(subject_id) %>% 
#   mutate(trip_nr_v5_by_next_1 = trip_nr_v4_by_next_2) %>% 
#   mutate(trip_nr_v5_by_next_1 = 
#            ifelse(trip_nr_v5_by_next_1 == -1 & 
#                     lead(trip_nr_v4_by_next_2 == 1, default = NA),
#                   0, 
#                   trip_nr_v5_by_next_1)) %>% 
#   mutate(trip_nr_v5_by_next_1 = 
#            ifelse(is.na(trip_nr_v5_by_next_1), 
#                   trip_nr_v4_by_next_2, 
#                   trip_nr_v5_by_next_1)) %>% 
#   mutate(was_identified = 
#            ifelse(trip_nr_v4_by_next_2 == -1 &
#                     trip_nr_v4_by_next_2 != trip_nr_v5_by_next_1,
#                   "3) identifitied 0 by next value = 1",
#                   was_identified))
# 
# 
# 
# # Visualize results: Identify 0 using next = 1 ----------------------------
# 
# plot_gps_v5 <- 
#   ggplot() + 
#   geom_point(data = dat,
#              aes_string(x = sett_dat$col_names$gps_lon_first,
#                         y = sett_dat$col_names$gps_lat_first,
#                         group = "file_name",
#                         color = "was_identified"),
#              alpha = 0.35) + 
#   facet_grid(trip_nr_v5_by_next_1~.) + 
#   geom_vline(xintercept = sett_proc$threshold_gps_lon_1, color = "red") +
#   geom_hline(yintercept = sett_proc$threshold_gps_lat_1, color = "red") +
#   geom_vline(xintercept = sett_proc$threshold_gps_lon_2, color = "green4") +
#   geom_hline(yintercept = sett_proc$threshold_gps_lat_2, color = "green4")
# 
# plot(plot_gps_v5)
# 
# 
# 
# 
# # Visualise start and end positions after V5 ------------------------------
# 
# plot_gps <- 
#   ggplot() + 
#   geom_point(data = dat,
#              aes_string(x = sett_dat$col_names$gps_lon_first,
#                         y = sett_dat$col_names$gps_lat_first,
#                         group = sett_dat$col_names$case),
#              colour = "green4",
#              alpha = 0.25) + 
#   geom_point(data = dat,
#              aes_string(x = sett_dat$col_names$gps_lon_last,
#                         y = sett_dat$col_names$gps_lat_last,
#                         group = sett_dat$col_names$case),
#              colour = "red",
#              alpha = 0.25) + 
#   facet_grid(.~trip_nr_v5_by_next_1) + 
#   geom_vline(xintercept = sett_proc$threshold_gps_lon_1, color = "red4") +
#   geom_hline(yintercept = sett_proc$threshold_gps_lat_1, color = "red4") +
#   geom_vline(xintercept = sett_proc$threshold_gps_lon_2, color = "green4") +
#   geom_hline(yintercept = sett_proc$threshold_gps_lat_2, color = "green4")
# 
# 
# windows(); plot(plot_gps)