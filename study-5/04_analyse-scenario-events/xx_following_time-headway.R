
# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$df_name <- "dat_study5_t_adtf_sxx_exx_exx_full_intrpld"
sett_dat$db_conn_name <- dbFindConnObj(sett_query$db_name, output = F)

sett_proc <- c()
sett_proc$speed_ms_preceding_vehicle <- 50/3.6
sett_proc$cases_to_exclude <- c("s01_e02_s504", "s01_e02_s552")
sett_proc$thresholds$am_limit1 <- 0
sett_proc$thresholds$am_limit2 <- 90
sett_proc$thresholds$steer_angle_deg <- 0
sett_proc$select_columns <- c(
  "case", 
  "subject_id",
  "sxx_exx",
  "condition_code",
  "interaction_type",
  "interaction_complexity",
  "speed_ms",
  "itrace_speed_ms", 
  "dist_m", 
  "sxx_exx_dti_m", 
  "dist_m_at_tta", 
  "sxx_exx_dti_m_at_tta", 
  "sxx_exx_tti_s", 
  "time_s", 
  "tta_s", 
  "time_s_at_tta", 
  "tta_s_min"
)
sett_proc$col_name_group <- "interaction_type"
sett_proc$col_name_am <- "sxx_exx_dti_m"
sett_proc$col_name_indicator <- "time_headway_s"



# Prepare data ------------------------------------------------------------

dat_gap <- 
  get(sett_dat$df_name) %>% 
  filter(is_usable) %>% 
  mutate(speed_ms = speed_kmh / 3.6)

## Exclude third scenario
# dat_gap <- 
#   dat_gap %>% 
#   filter(!grepl("s03", sxx_exx)) %>% 
#   filter(!grepl("s01", sxx_exx)) 

## Join data and information on TTA falling below threshold 
dat_gap <- 
  left_join(dat_gap,
            dat_tta_1st %>% 
              select(case, 
                     time_s_at_tta = time_s, 
                     tta_s_min = tta_s,
                     sxx_exx_dti_m_at_tta = sxx_exx_dti_m,
                     dist_m_at_tta = dist_m)) %>% 
  select(sett_proc$select_columns) 



# Simulate preceding vehicle and compute gap ------------------------------

dat_gap <- 
  dat_gap %>% 
  group_by(case) %>% 
  
  ## Compute driven distance since TTA
  mutate(dist_m_since_tta = dist_m - dist_m_at_tta) %>% 
  mutate(dist_m_since_tta = 
           ifelse(dist_m_since_tta < 0, 
                  0, 
                  dist_m_since_tta)) %>% 
  
  ## Compute elapsed time since TTA
  mutate(time_s_since_tta = time_s - time_s_at_tta + (3 + tta_s_min)) %>% 
  mutate(time_s_since_tta = 
           ifelse(time_s_since_tta < 0, 
                  0, 
                  time_s_since_tta)) %>% 
  mutate(time_s_since_tta_diff = c(0, diff(time_s_since_tta))) %>% 
  
  ## Compute speed profile using acceleration value
  mutate(preceding_speed_ms = 0) %>% 
  mutate(preceding_speed_ms = 4 * time_s_since_tta_diff) %>% 
  mutate(preceding_speed_ms = cumsum(preceding_speed_ms)) %>% 
  
  group_by(case, tta_s) %>% 
  #rowwise() %>% 
  mutate(preceding_speed_ms = 
           min(preceding_speed_ms, 
               sett_proc$speed_ms_preceding_vehicle)) %>% 
  
  ## Re-group-by after rowwise
  group_by(case) %>% 
  
  ## Compute (arbitrary) track position for ego
  mutate(ego_s_diff = itrace_speed_ms * time_s_since_tta_diff) %>% 
  mutate(ego_s = cumsum(ego_s_diff)) %>% 
  
  ## Compute (arbitrary) track position for preceding vehicle
  mutate(preceding_s_diff = preceding_speed_ms * time_s_since_tta_diff) %>% 
  # mutate(preceding_s_diff = 50 / 3.6 * time_s_since_tta_diff) %>% 
  #mutate(preceding_s = 0) %>% 
  mutate(preceding_s = cumsum(preceding_s_diff)) %>% 
  
  ## Compute gap
  mutate(preceding_dist = preceding_s - dist_m_since_tta + abs(sxx_exx_dti_m_at_tta)) %>% 
  mutate(preceding_dist = 
           ifelse(time_s_since_tta == 0, 
                  0, 
                  preceding_dist)) %>% 
  
  ## Compute headway time
  mutate(time_headway_s = preceding_dist / itrace_speed_ms) %>% 
  
  ## Compute difference per AM step
  mutate(preceding_dist_diff = c(0, diff(preceding_dist)))



# Write data on gap behavior to database ----------------------------------

# dbWriteTable(get(sett_dat$db_conn_name), 
#              "t_results_gap_behavior_baseline",
#              dat_gap,
#              overwrite = T,
#              row.names = F)



# Summarize values --------------------------------------------------------

## Summarize data by scenario
dat_gap_summary <- 
  dat_gap %>% 
  filter(!case %in% sett_proc$cases_to_exclude) %>% 
  #filter(scenario_id == 1) %>% 
  ungroup() %>% 
  group_by_("sxx_exx", sett_proc$col_name_group, sett_proc$col_name_am) %>% 
  summarize(preceding_dist_avg = mean(preceding_dist),
            preceding_dist_med = median(preceding_dist),
            preceding_dist_scope_avg = mean(preceding_dist_diff),
            preceding_dist_scope_med = median(preceding_dist_diff),
            time_headway_s_avg = mean(time_headway_s),
            time_headway_s_med = median(time_headway_s))

## Summarize over all baseline
dat_gap_summary_baseline <- 
  dat_gap %>% 
  filter(!case %in% sett_proc$cases_to_exclude) %>% 
  filter(grepl("s00", sxx_exx)) %>% 
  ungroup() %>% 
  group_by_(sett_proc$col_name_am) %>% 
  summarize(preceding_dist_avg = mean(preceding_dist),
            preceding_dist_med = median(preceding_dist),
            preceding_dist_scope_avg = mean(preceding_dist_diff),
            preceding_dist_scope_med = median(preceding_dist_diff),
            time_headway_s_avg = mean(time_headway_s),
            time_headway_s_med = median(time_headway_s))

# Summarize individual baseline
dat_gap_summary_baseline_individual <- 
  dat_gap %>% 
  filter(!case %in% sett_proc$cases_to_exclude) %>% 
  filter(grepl("s00", sxx_exx)) %>% 
  ungroup() %>% 
  group_by_("subject_id", sett_proc$col_name_am) %>% 
  summarize(preceding_dist_avg = mean(preceding_dist),
            preceding_dist_med = median(preceding_dist),
            preceding_dist_scope_avg = mean(preceding_dist_diff),
            preceding_dist_scope_med = median(preceding_dist_diff),
            time_headway_s_avg = mean(time_headway_s),
            time_headway_s_med = median(time_headway_s))



# Vizualisation: Profiles and key profiles --------------------------------

plot_dist <- 
  ggplot() + 
  ## Individual data
  geom_line(data = dat_gap,
            aes_string(x = sett_proc$col_name_am,
                       y = sett_proc$col_name_indicator,
                       group = "case",
                       color = sett_proc$col_name_group),
            size = 0.5,
            alpha = 0.35) + 
  geom_line(data = dat_gap %>%
              filter(case %in% sett_proc$cases_to_exclude),
            aes_string(x = sett_proc$col_name_am,
                       y = sett_proc$col_name_indicator,
                       group = "case"),
            size = 0.5,
            color = "red") + 
  ## Summarized data
  geom_line(data = dat_gap_summary,
            aes_string(x = sett_proc$col_name_am,
                       y = paste_(sett_proc$col_name_indicator, "avg"),
                       group = sett_proc$col_name_group,
                       color = sett_proc$col_name_group),
            size = 2) + 
  ## Summarized baseline
  geom_line(data = dat_gap_summary_baseline,
            aes_string(x = sett_proc$col_name_am,
                       y = paste_(sett_proc$col_name_indicator, "avg")),
            size = 2) + 
  facet_grid(.~sxx_exx) +
  coord_cartesian(ylim = c(0, 15)) + 
  ggtitle(paste("Average", sett_proc$col_name_indicator))

windows(); plot(plot_dist)



# Substract key values from individual profiles ---------------------------

dat_gap_diff_to_key_value <- 
  left_join(dat_gap %>% filter(!grepl("s00", sxx_exx)),
            dat_gap_summary_baseline) %>% 
  mutate_(.dots = setNames(
    list(interp(~ x - y,
         x = as.name(sett_proc$col_name_indicator),
         y = as.name(paste_(sett_proc$col_name_indicator, "avg")))),
    paste_(sett_proc$col_name_indicator, "diff_to_key_value")
  ))

dat_gap_diff_to_key_value_summary <- 
  dat_gap_diff_to_key_value %>% 
  group_by_("sxx_exx", sett_proc$col_name_group, sett_proc$col_name_am) %>% 
  summarize_(.dots = setNames(
    list(interp(~ mean(v),
                v = as.name(
                  paste_(sett_proc$col_name_indicator, "diff_to_key_value")
                ))),
    paste_(sett_proc$col_name_indicator, "avg")
  ))



# Visualization: Difference of invidiual profiles to key profiles ---------

plot_dist_diff_to_key_value <- 
  ggplot() + 
  ## Individual data
  geom_line(data = dat_gap_diff_to_key_value,
            aes_string(x = sett_proc$col_name_am,
                       y = paste_(sett_proc$col_name_indicator, "diff_to_key_value"),
                       group = "case",
                       color = sett_proc$col_name_group),
            alpha = 0.35) +
  geom_line(data = dat_gap_diff_to_key_value_summary,
            aes_string(x = sett_proc$col_name_am,
                       y = paste_(sett_proc$col_name_indicator, "avg"),
                       color = sett_proc$col_name_group),
            size = 2) + 
  facet_grid(.~sxx_exx) + 
  coord_cartesian(ylim = c(-5, 10)) +
  ggtitle(paste("Average", paste_(sett_proc$col_name_indicator, "diff_to_key_value")))

windows(); plot(plot_dist_diff_to_key_value)



# Substract individual key values from individual profiles ----------------

dat_gap_diff_to_key_value_individual <- 
  left_join(dat_gap %>% filter(!grepl("s00", sxx_exx)),
            dat_gap_summary_baseline_individual) %>% 
  mutate_(.dots = setNames(
    list(interp(~ x - y,
                x = as.name(sett_proc$col_name_indicator),
                y = as.name(paste_(sett_proc$col_name_indicator, "avg")))),
    paste_(sett_proc$col_name_indicator, "diff_to_key_value_individual")
  ))

dat_gap_diff_to_key_value_individual_summary <- 
  dat_gap_diff_to_key_value_individual %>% 
  group_by_("sxx_exx", sett_proc$col_name_group, sett_proc$col_name_am) %>% 
  summarize_(.dots = setNames(
    list(interp(~ mean(v, na.rm = T),
                v = as.name(
                  paste_(sett_proc$col_name_indicator, "diff_to_key_value_individual")
                ))),
    paste_(sett_proc$col_name_indicator, "avg")
  ))



# Vizualization: Individual key values from individual profiles -----------

plot_dist_diff_to_key_value_individual <- 
  ggplot() + 
  ## Individual data
  geom_line(data = dat_gap_diff_to_key_value_individual,
            aes_string(x = sett_proc$col_name_am,
                       y = paste_(sett_proc$col_name_indicator, "diff_to_key_value_individual"),
                       group = "case",
                       color = sett_proc$col_name_group),
            alpha = 0.35) +
  geom_line(data = dat_gap_diff_to_key_value_individual_summary,
            aes_string(x = sett_proc$col_name_am,
                       y = paste_(sett_proc$col_name_indicator, "avg"),
                       color = sett_proc$col_name_group),
            size = 2) + 
  facet_grid(.~sxx_exx) + 
  ggtitle(paste("Average", paste_(sett_proc$col_name_indicator, "diff_to_key_value_individual")))

windows(); plot(plot_dist_diff_to_key_value_individual)



# t-test ------------------------------------------------------------------

dat_gap_test <- 
  dat_gap_diff_to_key_value %>% 
  filter(sxx_exx == "s04_e01") %>% 
  #filter(interaction_type != "none" & sxx_exx == "s02_e03") %>% 
  filter(sxx_exx_dti_m >= 0) %>% 
  filter(sxx_exx_dti_m <= 50)


coll <- c()
for(i in unique(dat_gap_test$sxx_exx_dti_m)) {
  print(i)
  temp <- 
    dat_gap_test %>% 
    filter(sxx_exx_dti_m == i)
  
  temp1 <- temp %>% filter(interaction_type == "gesture") %>% pull(time_headway_s_diff_to_key_value)
  temp2 <- temp %>% filter(interaction_type == "touch") %>% pull(time_headway_s_diff_to_key_value)
  
  result1 <- t.test(temp1)
  result2 <- t.test(temp2)
  coll <- rbind(coll, c(i, result1$p.value, result2$p.value))
  
}

windows(); 
plot(coll[,1], coll[, 2], type = "l", col = "red", ylim = c(0, 1))
lines(coll[,1], coll[, 3], col = "blue")
abline(h = 0.05, lty = 3)

coll_long <- data.frame(coll)
names(coll_long) <- c("distance", "gesture", "touch")

coll_long <- 
  coll_long %>% 
  gather(variable, value, -distance) %>% 
  mutate(variable = factor(variable)) %>% 
  mutate(variable = factor(variable, labels = c("Gesture", "Touch")))

plot_time_headway_distance <- 
  ggplot() + 
  geom_line(data = coll_long,
            aes(x = distance,
                y = value,
                color = variable),
            size = 0.5) + 
  scale_color_manual("Interaction type", values = c("firebrick3", "dodgerblue4")) +
  coord_cartesian(ylim = c(0, 0.5)) +
  geom_hline(yintercept = 0.05) + 
  ggtitle("Time headway: Significance over distance") + 
  labs(x = "Distance (m)",
       y = "Probability") +
  theme_bw() +
  theme(title = element_text(size = 7, face = "bold")) + 
  theme(axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7)) + 
  theme(axis.text.x = element_text(size = 6, color = "black"),
        axis.text.y = element_text(size = 6, color = "black")) + 
  theme(legend.key.size = unit(0.2, "cm"),
        legend.margin = margin(rep(0.1, 4), unit='cm'),
        legend.title = element_text(size = 6, color = "black"),
        legend.text = element_text(size = 6, color = "black")) + 
  theme(legend.justification = c(1, 1), 
        legend.position = c(0.998, 0.998),
        legend.background = element_rect(color = "black",
                                         size = 0.2)) + 
  scale_x_continuous(expand = c(0, 0))

ggsave(filename = "time_headway_over_distance.png", 
       plot = plot_time_headway_distance,
       path = "plots",
       width = 8,
       height = 5,
       units = "cm",
       dpi = 600)




temp <- 
  dat_gap_test %>% 
  filter(sxx_exx_dti_m == 25)

temp1 <- temp %>% filter(interaction_type == "gesture") %>% pull(time_headway_s_diff_to_key_value)
temp2 <- temp %>% filter(interaction_type == "touch") %>% pull(time_headway_s_diff_to_key_value)

result1 <- t.test(temp1)
result2 <- t.test(temp2)

temp1 <- temp %>% filter(interaction_type == "gesture") %>% pull(time_headway_s)
temp2 <- temp %>% filter(interaction_type == "touch") %>% pull(time_headway_s)

t.test(temp1, temp2)





# Viz for kolloq ----------------------------------------------------------

dat_gap_diff_to_key_value_viz <- 
  dat_gap_diff_to_key_value %>% 
  ungroup() %>% 
  filter(sxx_exx == "s02_e03") %>% 
  filter(sxx_exx_dti_m >= 0 & sxx_exx_dti_m <= 50) %>% 
  mutate(interaction_type = factor(interaction_type)) %>% 
  mutate(interaction_type = factor(interaction_type, label = c("Gesture", "Touch")))

dat_gap_diff_to_key_value_summary_viz <- 
  dat_gap_diff_to_key_value_summary %>% 
  ungroup() %>% 
  filter(sxx_exx == "s02_e03") %>% 
  filter(sxx_exx_dti_m >= 0 & sxx_exx_dti_m <= 50) %>% 
  mutate(interaction_type = factor(interaction_type)) %>% 
  mutate(interaction_type = factor(interaction_type, labels = c("Gesture", "Touch")))

plot_dist_diff_to_key_value_viz <- 
  ggplot() + 
  ## Individual data
  geom_line(data = dat_gap_diff_to_key_value_viz,
            aes_string(x = sett_proc$col_name_am,
                       y = paste_(sett_proc$col_name_indicator, "diff_to_key_value"),
                       group = "case",
                       color = sett_proc$col_name_group),
            alpha = 0.05) +
  geom_line(data = dat_gap_diff_to_key_value_summary_viz,
            aes_string(x = sett_proc$col_name_am,
                       y = paste_(sett_proc$col_name_indicator, "avg"),
                       color = sett_proc$col_name_group),
            size = 0.5) + 
  #geom_hline(yintercept = 0, size = 0.5) +
  #facet_grid(.~sxx_exx) + 
  coord_cartesian(ylim = c(-2.5, 2.5), expand = F) +
  scale_color_manual("Interaction type",
                     values = c("firebrick3", "dodgerblue4")) + 
  ggtitle("Time headway", 
          subtitle = "Difference of time headway (TH) to averaged baselines") + 
  labs(x = "Distance (m)",
       y = "TH Interaction vs. TH Baseline (s)") +
  theme_bw() +
  theme(title = element_text(size = 7, face = "bold")) + 
  theme(axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7)) + 
  theme(axis.text.x = element_text(size = 6, color = "black"),
        axis.text.y = element_text(size = 6, color = "black")) + 
  theme(legend.key.size = unit(0.2, "cm"),
        legend.margin = margin(rep(0.1, 4), unit='cm'),
        legend.title = element_text(size = 6, color = "black"),
        legend.text = element_text(size = 6, color = "black")) + 
  theme(legend.justification = c(1, 1), 
        legend.position = c(0.998, 0.998),
        legend.background = element_rect(color = "black",
                                         size = 0.2))



#windows(); plot(plot_dist_diff_to_key_value_viz)


ggsave(filename = "time_headway.png", 
       plot = plot_dist_diff_to_key_value_viz,
       path = "plots",
       width = 8,
       height = 5,
       units = "cm",
       dpi = 600)
