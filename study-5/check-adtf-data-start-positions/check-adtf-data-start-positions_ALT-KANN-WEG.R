
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



# Visualize GPS positions -------------------------------------------------

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
  facet_grid(trip_nr~.) + 
  geom_vline(xintercept = sett_proc$threshold_gps_lon_1, color = "red4") +
  geom_hline(yintercept = sett_proc$threshold_gps_lat_1, color = "red4") +
  geom_vline(xintercept = sett_proc$threshold_gps_lon_2, color = "green4") +
  geom_hline(yintercept = sett_proc$threshold_gps_lat_2, color = "green4")

plot(plot_gps)



# Identify NA using starting GPS positions and area -----------------------

## Copy trip nr
dat$trip_nr_v2_by_area <- dat$trip_nr
dat$trip_nr_v2_by_area[dat$trip_nr_v2_by_area == "0"] <- "0-N"
dat$trip_nr_v2_by_area[dat$trip_nr_v2_by_area == "1"] <- "1-E"
dat$trip_nr_v2_by_area[dat$trip_nr_v2_by_area == "2"] <- "2-N"
dat$trip_nr_v2_by_area[dat$trip_nr_v2_by_area == "3"] <- "3-W"
dat$trip_nr_v2_by_area[dat$trip_nr_v2_by_area == "4"] <- "4-N"


## Identify clean trips

## Group cases into 1
dat <- 
  dat %>% 
  mutate(trip_nr_clean = 
           ifelse(is.na(trip_nr_v2_by_area) &
                    gps_lon_first < sett_proc$threshold_gps_lon_1 &
                    gps_lat_first > sett_proc$threshold_gps_lat_1 &
                    gps_lon_last > sett_proc$threshold_gps_lon_1 &
                    gps_lat_last > sett_proc$threshold_gps_lat_1,
                  "1-E",
                  trip_nr_v2_by_area))



# Group cases into 2, if starting positions are located in top right area
dat <-
  dat %>%
  mutate(trip_nr_v2_by_area =
           ifelse(is.na(trip_nr_v2_by_area) &
                    gps_lon_first > sett_proc$threshold_gps_lon_1 &
                    gps_lat_first > sett_proc$threshold_gps_lat_1 &
                    gps_lon_last < sett_proc$threshold_gps_lon_1 &
                    gps_lat_last > sett_proc$threshold_gps_lat_1,
                  "2-N",
                  trip_nr_v2_by_area))


## Group cases into 3
dat <- 
  dat %>% 
  mutate(trip_nr_v2_by_area = 
           ifelse(is.na(trip_nr_v2_by_area) &
                    gps_lon_first < sett_proc$threshold_gps_lon_1 &
                    gps_lat_first > sett_proc$threshold_gps_lat_1 &
                    gps_lon_last < sett_proc$threshold_gps_lon_2 &
                    gps_lat_last < sett_proc$threshold_gps_lat_2,
                  "3-W",
                  trip_nr_v2_by_area))



## Group cases into 4, if starting positions are located in bottom left area
dat <-
  dat %>%
  mutate(trip_nr_v2_by_area =
           ifelse(is.na(trip_nr_v2_by_area) &
                    gps_lon_first < sett_proc$threshold_gps_lon_2 &
                    gps_lat_first < sett_proc$threshold_gps_lat_2 &
                    gps_lon_last < sett_proc$threshold_gps_lon_1 &
                    gps_lat_last > sett_proc$threshold_gps_lat_1,
                  "4-N",
                  trip_nr_v2_by_area))


dat <-
  dat %>%
  mutate(trip_nr_v2_by_area =
           ifelse(is.na(trip_nr_v2_by_area) &
                    gps_lon_first > sett_proc$threshold_gps_lon_1 &
                    gps_lat_first > sett_proc$threshold_gps_lat_1 &
                    gps_lon_last > sett_proc$threshold_gps_lon_1 &
                    gps_lat_last > sett_proc$threshold_gps_lat_1,
                  "2-2",
                  trip_nr_v2_by_area))


dat <-
  dat %>%
  mutate(trip_nr_v2_by_area =
           ifelse(is.na(trip_nr_v2_by_area) &
                    gps_lon_first < sett_proc$threshold_gps_lon_2 &
                    gps_lat_first < sett_proc$threshold_gps_lat_2 &
                    gps_lon_last < sett_proc$threshold_gps_lon_2 &
                    gps_lat_last < sett_proc$threshold_gps_lat_2,
                  "4-4",
                  trip_nr_v2_by_area))


dat <- 
  dat %>% 
  mutate(trip_nr_v2_by_area = 
           ifelse(is.na(trip_nr_v2_by_area) &
                    gps_lon_last > sett_proc$threshold_gps_lon_1 &
                    gps_lat_last > sett_proc$threshold_gps_lat_1,
                  "X-E",
                  trip_nr_v2_by_area))

dat <-
  dat %>%
  mutate(trip_nr_v2_by_area =
           ifelse(is.na(trip_nr_v2_by_area) &
                    gps_lon_last < sett_proc$threshold_gps_lon_1 &
                    gps_lat_last > sett_proc$threshold_gps_lat_1,
                  "X-N",
                  trip_nr_v2_by_area))


dat <-
  dat %>%
  mutate(trip_nr_v2_by_area =
           ifelse(is.na(trip_nr_v2_by_area) &
                    gps_lon_last < sett_proc$threshold_gps_lon_2 &
                    gps_lat_last < sett_proc$threshold_gps_lat_2,
                  "X-W",
                  trip_nr_v2_by_area))



## Group cases into -1, if starting positions are located in top left area
# dat <- 
#   dat %>% 
#   mutate(trip_nr_v2_by_area = trip_nr) %>% 
#   mutate(trip_nr_v2_by_area = 
#            ifelse(is.na(trip_nr_v2_by_area) &
#                     gps_lon_first < sett_proc$threshold_gps_lon_1 &
#                     gps_lat_first > sett_proc$threshold_gps_lat_1,
#                   -1,
#                   trip_nr_v2_by_area))



# Code identification strategy --------------------------------------------

dat <- 
  dat %>% 
  mutate(was_identified = 
           ifelse(is.na(trip_nr) & !is.na(trip_nr_v2_by_area),
                  "1) identified by starting area",
                  "already correct"))



# Visualize results: Identification by area -------------------------------

plot_gps_area <- 
  ggplot() + 
  geom_point(data = dat,
             aes_string(x = sett_dat$col_names$gps_lon_first,
                        y = sett_dat$col_names$gps_lat_first,
                        group = "file_name",
                        color = "was_identified"),
             alpha = 0.35) + 
  # geom_point(data = dat,
  #            aes_string(x = sett_dat$col_names$gps_lon_first,
  #                       y = sett_dat$col_names$gps_lat_first,
  #                       group = "file_name",
  #                       shape = "was_identified"),
  #            colour = "green4",
  #            alpha = 0.25) + 
  # geom_point(data = dat,
  #            aes_string(x = sett_dat$col_names$gps_lon_last,
  #                       y = sett_dat$col_names$gps_lat_last,
  #                       group = "file_name",
  #                       shape = "was_identified"),
  #            colour = "red",
  #            alpha = 0.25) +
  facet_grid(trip_nr_v2_by_area~.) + 
  geom_vline(xintercept = sett_proc$threshold_gps_lon_1, color = "red") +
  geom_hline(yintercept = sett_proc$threshold_gps_lat_1, color = "red") +
  geom_vline(xintercept = sett_proc$threshold_gps_lon_2, color = "green4") +
  geom_hline(yintercept = sett_proc$threshold_gps_lat_2, color = "green4") 

plot(plot_gps_area)



# GPS start stop ----------------------------------------------------------



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
  facet_grid(.~trip_nr_v2_by_area) + 
  geom_vline(xintercept = sett_proc$threshold_gps_lon_1, color = "red4") +
  geom_hline(yintercept = sett_proc$threshold_gps_lat_1, color = "red4") +
  geom_vline(xintercept = sett_proc$threshold_gps_lon_2, color = "green4") +
  geom_hline(yintercept = sett_proc$threshold_gps_lat_2, color = "green4")


windows(); plot(plot_gps)





# Identify -1 using previous value = 2 ------------------------------------

## If previous value == 2, then trip should be probably 3
dat <- 
  dat %>% 
  group_by(subject_id) %>% 
  mutate(trip_nr_v3_by_prev_2 = trip_nr_v2_by_area) %>% 
  mutate(trip_nr_v3_by_prev_2 = 
           ifelse(trip_nr_v3_by_prev_2 == -1 & 
                    lag(trip_nr_v2_by_area == 2, default = NA),
                  3, 
                  trip_nr_v3_by_prev_2)) %>% 
  mutate(trip_nr_v3_by_prev_2 = 
           ifelse(is.na(trip_nr_v3_by_prev_2), 
                  trip_nr_v2_by_area, 
                  trip_nr_v3_by_prev_2)) %>% 
  mutate(was_identified = was_identified) %>% 
  mutate(was_identified = 
           ifelse(trip_nr_v2_by_area == -1 &
                    trip_nr_v2_by_area != trip_nr_v3_by_prev_2,
                  "2) identifitied 3 by previous value = 2",
                  was_identified))



# Visualize results: Identify -1 using previous = 2 -----------------------

plot_gps_prev_2 <- 
  ggplot() + 
  geom_point(data = dat,
             aes_string(x = sett_dat$col_names$gps_lon_first,
                        y = sett_dat$col_names$gps_lat_first,
                        group = "file_name",
                        color = "was_identified"),
             alpha = 0.35) + 
  facet_grid(trip_nr_v3_by_prev_2~.) + 
  geom_vline(xintercept = sett_proc$threshold_gps_lon_1, color = "red") +
  geom_hline(yintercept = sett_proc$threshold_gps_lat_1, color = "red") +
  geom_vline(xintercept = sett_proc$threshold_gps_lon_2, color = "green4") +
  geom_hline(yintercept = sett_proc$threshold_gps_lat_2, color = "green4")


plot(plot_gps_prev_2)





# Identify 1 using next value = 2 -----------------------------------------

dat <- 
  dat %>% 
  group_by(subject_id) %>% 
  mutate(trip_nr_v4_by_next_2 = trip_nr_v3_by_prev_2) %>% 
  mutate(trip_nr_v4_by_next_2 = 
           ifelse(trip_nr_v4_by_next_2 == -1 & 
                    lead(trip_nr_v3_by_prev_2 == 2, default = NA),
                  1, 
                  trip_nr_v4_by_next_2)) %>% 
  mutate(trip_nr_v4_by_next_2 = 
           ifelse(is.na(trip_nr_v4_by_next_2), 
                  trip_nr_v3_by_prev_2, 
                  trip_nr_v4_by_next_2)) %>% 
  mutate(was_identified = 
           ifelse(trip_nr_v3_by_prev_2 == -1 &
                    trip_nr_v3_by_prev_2 != trip_nr_v4_by_next_2,
                  "3) identifitied 1 by next value = 2",
                  was_identified))



# Visualize results: Identify 1 using next = 2 ----------------------------

plot_gps_next_2 <- 
  ggplot() + 
  geom_point(data = dat,
             aes_string(x = sett_dat$col_names$gps_lon_first,
                        y = sett_dat$col_names$gps_lat_first,
                        group = "file_name",
                        color = "was_identified"),
             alpha = 0.35) + 
  facet_grid(trip_nr_v4_by_next_2~.) + 
  geom_vline(xintercept = sett_proc$threshold_gps_lon_1, color = "red") +
  geom_hline(yintercept = sett_proc$threshold_gps_lat_1, color = "red") +
  geom_vline(xintercept = sett_proc$threshold_gps_lon_2, color = "green4") +
  geom_hline(yintercept = sett_proc$threshold_gps_lat_2, color = "green4")

plot(plot_gps_next_2)



# Visualize results: Identify 0 using next = 1 ----------------------------

dat <- 
  dat %>% 
  group_by(subject_id) %>% 
  mutate(trip_nr_v5_by_next_1 = trip_nr_v4_by_next_2) %>% 
  mutate(trip_nr_v5_by_next_1 = 
           ifelse(trip_nr_v5_by_next_1 == -1 & 
                    lead(trip_nr_v4_by_next_2 == 1, default = NA),
                  0, 
                  trip_nr_v5_by_next_1)) %>% 
  mutate(trip_nr_v5_by_next_1 = 
           ifelse(is.na(trip_nr_v5_by_next_1), 
                  trip_nr_v4_by_next_2, 
                  trip_nr_v5_by_next_1)) %>% 
  mutate(was_identified = 
           ifelse(trip_nr_v4_by_next_2 == -1 &
                    trip_nr_v4_by_next_2 != trip_nr_v5_by_next_1,
                  "3) identifitied 0 by next value = 1",
                  was_identified))



# Visualize results: Identify 0 using next = 1 ----------------------------

plot_gps_v5 <- 
  ggplot() + 
  geom_point(data = dat,
             aes_string(x = sett_dat$col_names$gps_lon_first,
                        y = sett_dat$col_names$gps_lat_first,
                        group = "file_name",
                        color = "was_identified"),
             alpha = 0.35) + 
  facet_grid(trip_nr_v5_by_next_1~.) + 
  geom_vline(xintercept = sett_proc$threshold_gps_lon_1, color = "red") +
  geom_hline(yintercept = sett_proc$threshold_gps_lat_1, color = "red") +
  geom_vline(xintercept = sett_proc$threshold_gps_lon_2, color = "green4") +
  geom_hline(yintercept = sett_proc$threshold_gps_lat_2, color = "green4")

plot(plot_gps_v5)




# Visualise start and end positions after V5 ------------------------------

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
  facet_grid(.~trip_nr_v5_by_next_1) + 
  geom_vline(xintercept = sett_proc$threshold_gps_lon_1, color = "red4") +
  geom_hline(yintercept = sett_proc$threshold_gps_lat_1, color = "red4") +
  geom_vline(xintercept = sett_proc$threshold_gps_lon_2, color = "green4") +
  geom_hline(yintercept = sett_proc$threshold_gps_lat_2, color = "green4")


windows(); plot(plot_gps)