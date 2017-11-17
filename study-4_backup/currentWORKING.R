
## Get list of driving data tables
tablelist <- dbListTables(dbconn_study4)
library(stringr)
tablelist <- tablelist[str_detect(tablelist, "t_stisim")]

dat2import <- c()
dat2import$scenario <- "city"
dat2import$type <- "gesture"
dat2import$level <- ""

test <- apply( sapply(dat2import, grepl, tablelist), 1, all)

datcoll <- c()

dat_df <- 
  plyr::ldply(tablelist[test], function(x) {
    dat <- dbGetQuery(dbconn_study4, paste("SELECT eltime01, lodist06, lonacc02, \"t.lacc12\", spedmh23, latpos07 FROM", x))
    dat$table <- x
    #dat$type <- ifelse(grepl("touch", x), "touch", "gesture")
    dat$type <- ifelse(grepl("touch", x), "touch", "gesture")
    dat$level <- ifelse(grepl("simple", x), "simple", "complex")
    #cat(x, "\n")
    #cat(type, "\n")
    datcoll <- rbind(datcoll, dat)
    } )

# ggplot() + 
#   geom_line(data = dat_df,
#             aes(x = lodist06, y = spedmh23 / 1.6, group = table)) + 
#   facet_grid(.~type) +
#   theme_bw()

#dat_df_touch <- dat_df
dat_df_gesture <- dat_df

dat_df <- rbind(dat_df_touch, dat_df_gesture)

# First interaction task --------------------------------------------------

dat_df_task1 <- 
  dat_df %>% 
  filter(lodist06 >= 550 & lodist06 <= 650) %>% 
  filter()

ggplot() + 
  geom_line(data = dat_df_task1,
            aes(x = lodist06, y = spedmh23 / 1.61, group = table)) + 
  facet_grid(level~type) +
  theme_bw()

ggplot() + 
  geom_line(data = dat_df_task1,
            aes(x = lodist06, y = latpos07, group = table)) + 
  facet_grid(level~type) +
  #scale_y_continuous(limits = c(1, 3.5)) + 
  theme_bw()



# Resterampe --------------------------------------------------------------



# ## Re-compute speed
# dat_df <-
#   dat_df %>%
#   group_by(table) %>%
#   mutate(acc = (2 * (lodist06 - lag(lodist06)) ) / (eltime01 - lag(eltime01))^2 ) %>%
#   mutate(v = 0) %>% 
#   mutate(v = lag(v) + acc * (eltime01 - lag(eltime01)) )
# 
# ## 1 mp/h = 1.61 km/h
# ## 1 km/h = 0.62 mp/h
# 
# dat_df_avg <- 
#   dat_df %>% 
#   mutate(dist = round(lodist06, 1)) %>% 
#   group_by(type, dist) %>% 
#   summarise(spedmh23_avg = mean(spedmh23)) %>% 
#   data.frame()
# 
# ggplot() +
#   geom_line(data = dat_df_avg,
#             aes(x = dist, y = spedmh23_avg, color = type)) + 
#   facet_grid(.~type) +
#   theme_bw() +
#   coord_cartesian(ylim = c(0, 100))
# 
# ggplot() +
#   geom_line(data = dat_df %>%
#               filter(table %in%
#                        c("t_stisim_402_city_touch_simple",
#                          "t_stisim_402_city_gesture_simple")),
#             aes(x = lodist06, y = spedmh23 / 1.61, color = table)) +
#   facet_grid(.~type) +
#   theme_bw() +
#   coord_cartesian(ylim = c(0, 100))