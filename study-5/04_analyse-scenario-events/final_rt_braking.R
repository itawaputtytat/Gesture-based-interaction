
# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$db_conn_name <- dbFindConnObj("GBI_Study-5", output = F)



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
                  labels = c("Gesture", "Touch"))) %>% 
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
               alpha = 0.35,
               size = 0.2,
               fatten = 2.5,
               outlier.colour = NA) +
  geom_hline(data = dat_summary,
             aes_string(yintercept = "time_s_diff_avg",
                        color = "condition_scenario",
                        alpha = "condition_scenario"),
             alpha = 0.5,
             size = 0.5) +
  geom_hline(data = dat_summary,
             aes_string(yintercept = "time_s_diff_avg",
                        color = "condition_scenario",
                        linetype = "condition_scenario"),
             size = 0.5) +
  # stat_boxplot(data = dat_ssq_t1_t2_scores_long,
  #              aes_string(x = "tx",
  #                         y = "score",
  #                         olor = "subscale"),
  #              geom ='errorbar',
  #              lwd = 0.35) + 
  geom_point(data = dat,
             aes_string(x = "condition_scenario",
                        y = "time_s_diff",
                        color = "condition_scenario"),
             shape = 16,
             alpha = 0.5,
             size = 1,
             position = position_jitter(w = 0.1, h = 0)) +
  facet_grid(.~scenario) + 
  coord_cartesian(ylim = c(0, 3)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_shape_manual("line1", values = c(16)) +
  scale_alpha_manual(name = "Average reaction time: ", 
                     values = c(1, 0.5)) +
  scale_color_manual(name = "Average reaction time: ", 
                     values = c("firebrick3", "dodgerblue4", "steelblue3", "red3")) +
  scale_fill_manual(values = c("firebrick3", "dodgerblue4", "steelblue3", "red3")) +
  scale_linetype_manual(name = "Average reaction time: ", 
                        values = c("solid", "dashed")) + 
  guides(fill = F, shape = F,
         color = guide_legend(override.aes=list(shape=NA)))
  
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
  # coord_cartesian(ylim = c(0, 2.5)) +
  # facet_grid(.~scenario) +
  # expand_limits(y = 0)

plot(plot_rt_break)


# Post-processing ---------------------------------------------------------

sett_plot <- c()
sett_plot$legend_text_size <- 6
sett_plot$legend_text_color <- "black"

plot_rt_break <- 
  plot_rt_break +
  ggtitle("Reaction time until braking") + 
  labs(x = "Interaction system",
       y = "Reaction time (s)") + 
  theme_bw() + 
  theme(legend.justification = c(0, 1), 
        legend.position = "top",
        legend.direction = "horizontal",
        legend.margin = margin(c(-0.1, 0, -0.25, 0.01), unit = 'cm')) +
  # theme(legend.justification = c(0, 1), 
  #       legend.position = c(0.001, 0.999),
  #       legend.background = element_rect(color = "black",
  #                                        size = 0.2)) +
  theme(legend.key.width = unit(0.33, "cm"),
        legend.key.height = unit(0.25, "cm"),
        legend.key = element_blank(),
        legend.title = element_text(size = sett_plot$legend_text_size, 
                                    color = sett_plot$legend_text_color),
        #legend.title = element_blank(),
        legend.text = element_text(size = sett_plot$legend_text_size, 
                                   color = sett_plot$legend_text_color),
        legend.background = element_blank()) +
  theme(title = element_text(size = 7, face = "bold")) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7)) + 
  theme(axis.text.x = element_text(size = 6, color = "black"),
        axis.text.y = element_text(size = 6, color = "black")) + 
  # theme(legend.position = "top",
  #       legend.background = element_rect(color = NA),
  #       legend.key.height = unit(-0.2, "cm"),
  #       legend.key.width = unit(0.39, "cm"),
  #       #legend.margin = margin(c(0.1, 0.1, 0.1, 0.1), unit='cm'),
  #       legend.margin = margin(c(0.07, 0, -0.15, 0), unit='cm'),
  #       legend.title = element_text(size = 6, color = "black"),
  #       legend.text = element_text(size = 6, color = "black", face = "bold")) + 
  theme(strip.text.x = element_text(size = 6, face = "bold"))

plot(plot_rt_break)

ggsave(filename = "brake-rt.png", 
       plot = plot_rt_break,
       path = "plots",
       width = 16,
       height = 6,
       units = "cm",
       dpi = 600)



# t-test ------------------------------------------------------------------

dat_test <- 
  dat_test1 <- 
  dat %>% 
  filter(grepl("s03_e02", case)) %>% 
  filter(!is_outlier) 

dat_test1 <- 
  dat_test %>% 
  filter(condition_scenario == "Gesture") %>% 
  pull(time_s_diff)

dat_test2 <- 
  dat_test %>% 
  filter(condition_scenario == "Touch") %>% 
  pull(time_s_diff)
  
print(t.test(dat_test1, dat_test2, var.equal = F, paired = F))
print(cohen.d(dat_test1, dat_test2, paired = F))


# F-test ------------------------------------------------------------------

## Nicht zu ende gedacht

dat_f <- 
  dat %>% 
  mutate(tx = 4) %>% 
  mutate(tx = ifelse(grepl("Turning", scenario), 1, tx)) %>% 
  mutate(tx = ifelse(grepl("Evasing", scenario), 2, tx)) %>% 
  mutate(tx = ifelse(grepl("Crossing", scenario), 3, tx))
  # mutate(group_btw = 
  #          ifelse(scenario %in% "Turning vehicle" |
  #                   scenario %in% "Crossing pedestrian",
  #                 "group_btw1",
  #                 "group_btw2")) %>% 
  # mutate(tx = 
  #          ifelse(scenario %in% "Turning vehicle" |
  #                   scenario %in% "Evasing vehicle",
  #                 "t1",
  #                 "t2"))

library(lme4)
lmeModel = lmer(time_s_diff ~ condition_scenario*tx + (1|subject_id), data = dat_f)
anova(lmeModel)
summary(lmeModel)


library(ez)

ezANOVA(data = dat_f, 
        dv = time_s_diff, 
        wid = subject_id, 
        within = tx,
        between = condition_scenario, 
        type = 3, 
        detailed = F,
        return_aov = T)
