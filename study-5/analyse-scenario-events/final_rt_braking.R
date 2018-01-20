
# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$db_conn_name <- dbFindConnObj(sett_query$db_name, output = F)



# Get data ----------------------------------------------------------------

dat_turning <- dbGetSrc(sett_dat$db_conn_name, "t_results_time_s_tta_to_brake_1st_s01_e01")
dat_turning$scenario <- "turning"

dat_evasion <- dbGetSrc(sett_dat$db_conn_name, "t_results_time_s_tta_to_brake_1st_s02_e02")
dat_evasion$scenario <- "evasion"

dat_pedestrian <- dbGetSrc(sett_dat$db_conn_name, "t_results_time_s_tta_to_brake_1st_s03_e02")
dat_pedestrian$scenario <- "pedestrian"

dat_reverse <- dbGetSrc(sett_dat$db_conn_name, "t_results_time_s_tta_to_brake_1st_s04_e03")
dat_reverse$scenario <- "reverse"



# Data preparation --------------------------------------------------------

dat <- 
  rbind(
    dat_turning,
    dat_evasion,
    dat_pedestrian,
    dat_reverse
  ) %>% 
  filter(!is_outlier)

dat <- 
  dat %>% 
  mutate(condition_scenario = 
           factor(condition_scenario, 
                  levels = c("gesture", "touch"), 
                  labels = c("GBI", "TBI"))) %>% 
  mutate(scenario = 
           factor(scenario,
                  levels = c("turning", "evasion", "pedestrian", "reverse"),
                  labels = c("Turning vehicle",
                             "Evasing vehicle",
                             "Crossing pedestrian",
                             "Parking vehicle")))

dat_summary <- 
  dat %>% 
  group_by(condition_scenario, 
           scenario) %>% 
  summarize(time_s_diff_avg = mean(time_s_diff),
            time_s_diff_sd = sd(time_s_diff),
            time_s_diff_se = sd(time_s_diff) / sqrt(n()))


# Visualize data ----------------------------------------------------------

plot_rt_break <- 
  ggplot() +
  geom_boxplot(data = dat,
               aes(x = condition_scenario,
                   y = time_s_diff,
                   fill = condition_scenario),
               alpha = 0.5,
               size = 0.2,
               fatten = 2.5,
               outlier.colour = NA) +
  geom_hline(data = dat_summary,
             aes_string(yintercept = "time_s_diff_avg",
                        group = "scenario",
                        color = "condition_scenario",
                        linetype = "condition_scenario"),
             size = 0.35) +
  # stat_boxplot(data = dat_ssq_t1_t2_scores_long,
  #              aes_string(x = "tx",
  #                         y = "score",
  #                         olor = "subscale"),
  #              geom ='errorbar',
  #              lwd = 0.35) + 
  geom_point(data = dat,
             aes_string(x = "condition_scenario",
                        y = "time_s_diff",
                        #shape = "is_outlier_z",
                        color = "condition_scenario"),
             shape = 16,
             alpha = 0.5,
             size = 0.35,
             position = position_jitter(w = 0.1, h = 0)) +
  facet_grid(.~scenario) + 
  coord_cartesian(ylim = c(0, 2.5)) +
  scale_color_manual(values = c("green3", "orange2", "steelblue3", "red3")) +
  scale_fill_manual(values = c("green3", "orange2", "steelblue3", "red3")) +
  scale_linetype_manual(name = "Score means: ", values = c("dashed", "solid")) + 
  guides(fill = F, color = F) +
  theme_bw() + 
  theme(legend.justification = c(0, 1), 
        legend.position = c(0.001, 0.999),
        legend.background = element_rect(color = "black",
                                         size = 0.2)) +
  theme(title = element_text(size = 7, face = "bold")) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7)) + 
  theme(axis.text.x = element_text(size = 6, color = "black"),
        axis.text.y = element_text(size = 6, color = "black")) + 
  theme(legend.position = "top",
        legend.background = element_rect(color = NA),
        legend.key.height = unit(-0.2, "cm"),
        legend.key.width = unit(0.39, "cm"),
        #legend.margin = margin(c(0.1, 0.1, 0.1, 0.1), unit='cm'),
        legend.margin = margin(c(0.07, 0, -0.15, 0), unit='cm'),
        legend.title = element_text(size = 6, color = "black"),
        legend.text = element_text(size = 6, color = "black", face = "bold")) + 
  theme(strip.text.x = element_text(size = 6, face = "bold")) 
  
plot(plot_rt_break)

  
  # geom_bar(dat = dat_summary,
  #          aes(x = condition_scenario,
  #              y = time_s_diff_avg),
  #          stat = "identity") + 
  # geom_errorbar(data = dat_summary,
  #               aes(x = condition_scenario,
  #                   ymin = time_s_diff_avg - time_s_diff_se, 
  #                   ymax = time_s_diff_avg + time_s_diff_se), 
  #               width = 0.2) +
  coord_cartesian(ylim = c(0, 2.5)) +
  facet_grid(.~scenario) +
  expand_limits(y = 0)

plot(plot_rt_break)


# Post-processing ---------------------------------------------------------

plot_rt_break <- 
  plot_rt_break +
  ggtitle("Reaction time until braking") + 
  labs(x = "Interaction system",
       y = "Reaction time in s")

plot(plot_rt_break)

ggsave(filename = "brake-rt.png", 
       plot = plot_rt_break,
       path = "plots",
       width = 16,
       height = 5,
       units = "cm",
       dpi = 600)
