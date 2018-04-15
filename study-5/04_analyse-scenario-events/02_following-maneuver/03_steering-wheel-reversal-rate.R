
# Preparatory settings ----------------------------------------------------

sett_proc <- c()
sett_proc$df_name <- "dat_study5_t_adtf_sxx_exx_exx_full"
sett_proc$thresholds$am_limit1 <- 0
sett_proc$thresholds$am_limit2 <- 100
#sett_proc$thresholds$steer_angle_deg <- 0.5
sett_proc$thresholds$steer_angle_deg_gap <- 0.5
sett_proc$thresholds$steer_angle_degs <- 3
sett_proc$thresholds$elapsed_time_s = 8
sett_proc$scenario <- "s02_e03"
sett_proc$col_name_group <- "interaction_type"
sett_proc$col_name_indicator <- "reversal_rate"



# Create plot template ----------------------------------------------------

plot_template <- 
  ggplot() + 
  ggtitle(label = "Reversal rate",
          subtitle = 
            paste("AM:", sett_proc$thresholds$am_limit1,
                  "to", sett_proc$thresholds$am_limit2))



# Data preparation --------------------------------------------------------

dat_reversals <- 
  get(sett_proc$df_name) %>% 
  filter(is_usable) %>% 
  
  ## Exclude third scenario
  ## Filter for steering angle
  #filter(steer_angle_deg >= sett_proc$thresholds$steer_angle_deg) %>% 
  # filter(sxx_exx_dti_m >= sett_proc$thresholds$am_limit1 &
  #          sxx_exx_dti_m <= sett_proc$thresholds$am_limit2) #%>% 
  #filter(sxx_exx_dti_m >= 0 & tta_s <= 5) %>% 
  filter(sxx_exx_dti_m >= 0) %>%
  group_by(case) %>% 
  filter(time_s <= (min(time_s) + sett_proc$thresholds$elapsed_time_s))



# Identify reversals ------------------------------------------------------

dat_reversals <- 
  dat_reversals %>% 
  
  ## Correct steer angle with sign
  mutate(steer_angle_deg_v2 = 
           ifelse(steer_angle_deg_sign == 1, 
                  steer_angle_deg * -1, 
                  steer_angle_deg)) %>% 
  
  ## Extract steering direction
  group_by(sxx_exx, case) %>% 
  mutate(steer_direction = 0) %>% 
  
  mutate(steer_direction = 
           ifelse(steer_angle_deg_v2 < lag(steer_angle_deg_v2, default = 0),
                  -1, 0)) %>% 
  mutate(steer_direction =
           ifelse(steer_angle_deg_v2 > lag(steer_angle_deg_v2, default = 0),
                  1, steer_direction)) %>% 
  mutate(steer_direction =
           ifelse(is.na(lag(steer_angle_deg_v2)),
                  0, steer_direction)) %>% 
  
  ## Connect steer directions if steer angle is not zero
  mutate(steer_direction_v2 =
           ifelse(steer_direction == 0 & steer_angle_deg_v2 != 0,
                  NA, steer_direction)) %>% 
  mutate(steer_direction_v2 = na.locf(steer_direction_v2, na.rm = F)) %>% 
  mutate(steer_direction_v2 = 
           ifelse(steer_direction_v2 == 0,
                  NA,
                  steer_direction_v2)) %>% 
  mutate(steer_direction_v2 = na.locf(steer_direction_v2, na.rm = F)) %>% 
  mutate(steer_direction_v2 = 
           ifelse(is.na(steer_direction_v2),
                  0,
                  steer_direction_v2)) %>% 

  ## Compute sequences
  mutate(row_nr = row_number()) %>%
  mutate(steer_direction_v2_cumsum = c(0, diff(steer_direction_v2))) %>%
  group_by(sxx_exx, case, steer_direction_v2) %>%
  mutate(row_nr_seq = row_number()) %>%
  mutate(seq_id = row_nr - row_nr_seq) %>%

  ## Variable selection
  select(sxx_exx, subject_id, case, interaction_type, sxx_exx_dti_m, sxx_exx_tti_s,
         time_s, steer_angle_deg, steer_angle_deg_v2, steer_direction, steer_direction_v2,
         steer_angle_speed_degs,
         seq_id,
         steer_direction_v2_cumsum)



# Visualize individual case -----------------------------------------------

dat_reversals_viz_ind <- 
  dat_reversals %>%
  filter(sxx_exx == "s00_e01") %>% 
  filter(subject_id == 516)

plot_reversal <-
  ggplot() +
  geom_line(data = dat_reversals_viz_ind,
            aes_string(x = "sxx_exx_dti_m",
                       y = "steer_angle_deg_v2",
                       group = "case")) +
  geom_line(data = dat_reversals_viz_ind,
            aes_string(x = "sxx_exx_dti_m",
                       y = "steer_direction",
                       group = "case"),
            col = "blue",
            size = 2) +
  geom_line(data = dat_reversals_viz_ind,
            aes_string(x = "sxx_exx_dti_m",
                       y = "steer_direction_v2",
                       group = "case"),
            col = "red",
            size = 1) +
  facet_grid(sxx_exx~.) + 
  coord_cartesian(ylim = c(-20, 20))

#windows(); plot(plot_reversal)



# Summarize reversals -----------------------------------------------------

dat_reversals_summary <- 
  dat_reversals %>% 
  ## Filter for non-zero steer angle sequences
  ## (= removing the first sequence)
  filter(steer_direction_v2 != 0) %>% 

  group_by(sxx_exx, case, subject_id, interaction_type, seq_id) %>% 
  summarize(steer_angle_deg_first = first(steer_angle_deg),
            steer_angle_deg_last = last(steer_angle_deg),
            steer_angle_deg_min = min(steer_angle_deg),
            steer_angle_deg_max = max(steer_angle_deg),
            steer_angle_speed_degs_first = first(steer_angle_speed_degs),
            steer_angle_speed_degs_last = last(steer_angle_speed_degs),
            steer_angle_speed_degs_min = min(steer_angle_speed_degs),
            steer_angle_speed_degs_max = max(steer_angle_speed_degs)) %>% 
  ## Compute gap for from last steering
  group_by(sxx_exx, subject_id, case, interaction_type) %>% 
  mutate(steer_angle_deg_last_lag = lag(steer_angle_deg_first, default = NA),
         steer_angle_deg_last_lag = ifelse(is.na(steer_angle_deg_last_lag), 
                                           steer_angle_deg_first,
                                           steer_angle_deg_last_lag),
         steer_angle_deg_gap = steer_angle_deg_first - steer_angle_deg_last_lag) 



# Compute reversal rate ---------------------------------------------------

dat_reversal_rate <- 
  dat_reversals_summary %>% 
  filter(abs(steer_angle_deg_gap) >= sett_proc$thresholds$steer_angle_deg_gap) %>% 
  filter(abs(steer_angle_speed_degs_max) >= sett_proc$thresholds$steer_angle_degs) %>% 
  ## Compute reversal rate
  group_by(sxx_exx, case, subject_id, interaction_type) %>% 
  summarize(n_reversals = n_distinct(seq_id)) %>% 
  mutate(reversal_rate = n_reversals / 5) %>% 
  ## Code outliers
  #mutate(is_outlier = codeOutliersZ(reversal_rate, zCutOff = 2.575)) 
  mutate(is_outlier = codeOutliersZ(reversal_rate)) 



# Visualize reversal rate ------------------------------------------------

dat_reversal_rate_plot <- dat_reversal_rate

plot_reversal_rate_boxplot <- 
  plot_template +
  geom_boxplot(data = dat_reversal_rate_plot,
               aes_string(x = sett_proc$col_name_group,
                          y = sett_proc$col_name_indicator)) + 
  geom_point(data = dat_reversal_rate_plot %>% 
               filter(is_outlier),
             aes_string(x = sett_proc$col_name_group,
                        y = sett_proc$col_name_indicator),
             color = "red") + 
  facet_grid(.~sxx_exx, scales = "free") +
  coord_cartesian(ylim = c(0, 5))

plot(plot_reversal_rate_boxplot)



# Analysis ----------------------------------------------------------------

## Extract baseline values
dat_reversal_rate_baseline <- 
  dat_reversal_rate %>% 
  filter(!is_outlier) %>% 
  filter(sxx_exx %in% c("s00_e01", "s00_e02")) %>% 
  filter(reversal_rate < 1.5) 

dat_reversal_rate_test_baseline <- 
  dat_reversal_rate_baseline %>% 
  ungroup() %>% 
  #group_by(subject_id) %>% 
  summarize(reversal_rate_baseline_avg = mean(reversal_rate)) 


## Extract values for interaction type
dat_reversal_rate_test <- 
  dat_reversal_rate #%>%
  # filter(!is_outlier) %>%
  # left_join(dat_reversal_rate_test_baseline) %>%
  # mutate(reversal_rate_diff = reversal_rate - reversal_rate_baseline_avg) %>%
  # mutate(is_outlier_reversal = codeOutliersZ(reversal_rate_diff, zCutOff = 1.96))

dat_reversal_rate_test <- 
  dat_reversal_rate_test %>% 
  filter(!is_outlier)

dat_reversals_count_test_gbi <- 
  dat_reversal_rate_test %>% 
  filter(sxx_exx == sett_proc$scenario) %>% 
  filter(interaction_type == "gesture") %>% 
  filter(reversal_rate < 1.5)
  #filter(reversal_rate_diff < 1.25) 
  #pull(reversal_rate)

dat_reversals_count_test_tbi <- 
  dat_reversal_rate_test %>% 
  filter(sxx_exx == sett_proc$scenario) %>% 
  filter(interaction_type == "touch")
  #filter(reversal_rate_diff > 0.5) 

## t-tests

# print(t.test(dat_reversals_count_test_gbi %>% 
#                pull(reversal_rate), 
#              dat_reversal_rate_test_baseline %>% 
#                pull(reversal_rate_baseline_avg), var.equal = F))

print(t.test(dat_reversals_count_test_gbi %>% 
               pull(reversal_rate), 
             mu = 0.5306931))

# print(t.test(dat_reversals_count_test_tbi %>% 
#                pull(reversal_rate), 
#              dat_reversal_rate_test_baseline %>% 
#                pull(reversal_rate_baseline_avg), var.equal = F))

print(t.test(dat_reversals_count_test_tbi %>% 
               pull(reversal_rate), 
             mu = 0.5306931))

## Viz

plot_template +
  geom_boxplot(data = dat_reversal_rate_test,
               aes_string(x = sett_proc$col_name_group,
                          y = "reversal_rate"),
               outlier.colour = "red") + 
  geom_point(data = dat_reversal_rate_test %>% 
               filter(is_outlier),
             aes_string(x = sett_proc$col_name_group,
                        y = "reversal_rate"),
             color = "red") + 
  facet_grid(.~sxx_exx, scales = "free") +
  coord_cartesian(ylim = c(0, 5))



# Vizualization: Bar chart ------------------------------------------------

## Prepare data
dat_reversal_rate_test_summary_baseline_avg <-
  dat_reversal_rate_baseline %>%
  group_by(interaction_type) %>% ## needed for rbind
  summarize(val_mean = mean(reversal_rate),
            val_sd = sd(reversal_rate),
            val_se = sd(reversal_rate) / sqrt(n()) ) #%>%
  #mutate(interaction_type = factor(interaction_type, label = c("Gesture", "Touch")))

dat_reversal_rate_test_summary <-
  dat_reversal_rate_test %>%
  filter(sxx_exx == "s02_e03") %>%
  filter(reversal_rate < 1.5) %>% 
  group_by(interaction_type) %>%
  summarize(val_mean = mean(reversal_rate),
            val_sd = sd(reversal_rate),
            val_se = sd(reversal_rate) / sqrt(n()) ) #%>%
  #mutate(interaction_type = factor(interaction_type, label = c("Gesture", "Touch")))

dat_reversal_rate_test_summary <- 
  rbind(dat_reversal_rate_test_summary,
        dat_reversal_rate_test_summary_baseline_avg) %>% 
  mutate(interaction_type = 
           factor(interaction_type, 
                  level = c("none", "gesture", "touch"),
                  label = c("Baseline", "Gesture", "Touch")))


## Main plot
plot_swrr <-
  ggplot() +
  geom_bar(data = dat_reversal_rate_test_summary,
           aes_string(x = "interaction_type",
                      y = "val_mean",
                      fill = "interaction_type"),
           stat = "identity") +
  geom_errorbar(data = dat_reversal_rate_test_summary,
                aes(x = interaction_type,
                    ymin = val_mean - val_se,
                    ymax = val_mean + val_se),
                width = 0.2) 



# Post-processing ---------------------------------------------------------

## Scales
plot_swrr <- 
  plot_swrr
  scale_fill_manual(values = c("grey50", "firebrick3", "dodgerblue4")) +
  coord_cartesian(ylim = c(0, 1.5)) +
  scale_y_continuous(expand = c(0, 0)) 
  
## Text
plot_swrr <- 
  plot_swrr +
  guides(fill = F, color = F) +
  ggtitle("Average steering wheel reversal rate") +
  labs(y = "SWRR / s")
  
## Theme
plot_swrr <- 
  plot_swrr + 
  theme_bw() +
  theme(title = element_text(size = 7, face = "bold")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7)) +
  theme(axis.text.x = element_text(size = 6, color = "black"),
        axis.text.y = element_text(size = 6, color = "black"))

plot(plot_swrr)



# Save plot ---------------------------------------------------------------

ggsave(filename = "steering_wheel_reversal.png",
       plot = plot_swrr,
       path = "plots",
       width = 8,
       height = 5,
       units = "cm",
       dpi = 600)
