
# Preparatory settings ----------------------------------------------------

sett_proc <- c()
sett_proc$df_name <- "dat_study5_t_adtf_sxx_exx_exx_full"
sett_proc$thresholds$am_limit1 <- 0
sett_proc$thresholds$am_limit2 <- 90
sett_proc$thresholds$steer_angle_deg <- 0.5
sett_proc$scenario <- "s02_e03"
#sett_proc$scenario <- "s04_e01"
sett_proc$col_name_group <- "interaction_type"
sett_proc$col_name_indicator <- "reversal_rate"

plot_template <- 
  ggplot() + 
  ggtitle(label = "Reversal rate",
          subtitle = paste("AM:", sett_proc$thresholds$am_limit1,
                           "to", sett_proc$thresholds$am_limit2))



# Data preparation --------------------------------------------------------

dat_lat <- 
  get(sett_proc$df_name) %>% 
  filter(is_usable) %>% 

  ## Exclude third scenario
  #filter(!grepl("s03", sxx_exx)) %>%
  ## Filter for steering angle
  filter(steer_angle_deg >= sett_proc$thresholds$steer_angle_deg) %>% 
  filter(sxx_exx_dti_m >= sett_proc$thresholds$am_limit1 &
           sxx_exx_dti_m <= sett_proc$thresholds$am_limit2) #%>% 
  #filter(sxx_exx_dti_m >= 0 & tta_s <= 5) 

# dat_lat <- dat_lat %>% filter(subject_id == 551) 


# Identify reversals ------------------------------------------------------

dat_lat_reversal_rate <- 
  dat_lat %>% 
  
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

dat_lat_reversal_rate_viz_ind <- 
  dat_lat_reversal_rate %>%
  filter(sxx_exx == "s02_e03") %>% 
  filter(subject_id == 525)

plot_reversal <-
  ggplot() +
  geom_line(data = dat_lat_reversal_rate_viz_ind,
            aes_string(x = "sxx_exx_dti_m",
                       y = "steer_angle_deg_v2",
                       group = "case")) +
  geom_line(data = dat_lat_reversal_rate_viz_ind,
            aes_string(x = "sxx_exx_dti_m",
                       y = "steer_direction",
                       group = "case"),
            col = "blue",
            size = 2) +
  geom_line(data = dat_lat_reversal_rate_viz_ind,
            aes_string(x = "sxx_exx_dti_m",
                       y = "steer_direction_v2",
                       group = "case"),
            col = "red",
            size = 1) +
  facet_grid(sxx_exx~.)

windows(); plot(plot_reversal)



# Compute reversal rate ---------------------------------------------------

dat_lat_reversal_rate_count_by_subject <- 
  dat_lat_reversal_rate %>% 
  
  ## Compute elapsed time in data section
  group_by(sxx_exx, case, interaction_type) %>% 
  mutate(elapsed_time_s = max(time_s) - min(time_s)) %>% 
  
  ## Filter for non-zero steer angle sequences
  filter(steer_direction_v2 != 0) %>% 
  
  ## Count sequences
  group_by(sxx_exx, case, interaction_type) %>% 
  summarize(n_reversals = n_distinct(seq_id),
            elapsed_time_s = max(elapsed_time_s),
            steer_angle_speed_degs = max(steer_angle_speed_degs)) %>% 
  
  ## Compute reversal rate
  mutate(reversal_rate = n_reversals / elapsed_time_s) %>% 

  ## Code outliers
  group_by(sxx_exx, interaction_type) %>% 
  mutate(is_outlier = codeOutliersZ(reversal_rate)) 
  


# Visualize reversale rate ------------------------------------------------

plot_reversal_rate_boxplot <- 
  plot_template +
  geom_boxplot(data = dat_lat_reversal_rate_count_by_subject,
               aes_string(x = sett_proc$col_name_group,
                          y = sett_proc$col_name_indicator)) + 
  geom_point(data = dat_lat_reversal_rate_count_by_subject %>% 
               filter(is_outlier),
             aes_string(x = sett_proc$col_name_group,
                        y = sett_proc$col_name_indicator),
             color = "red") + 
  facet_grid(.~sxx_exx, scales = "free") 

plot(plot_reversal_rate_boxplot)



# Analysis ----------------------------------------------------------------

dat_lat_reversal_rate_count_by_subject_test <- 
  dat_lat_reversal_rate_count_by_subject %>% 
  #mutate(reversal_rate = ifelse(reversal_rate > 100, 0, reversal_rate)) %>% 
  filter(!is_outlier) %>% 
  filter(sxx_exx != "s00_e01") %>% 
  filter(sxx_exx != "s00_e02") #%>% 
  #filter(sxx_exx != "s04_e01") 

dat_lat_reversal_rate_count_test1 <- 
  dat_lat_reversal_rate_count_by_subject_test %>% 
  filter(sxx_exx == sett_proc$scenario) %>% 
  filter(interaction_type == "gesture") %>% 
  pull(reversal_rate)

dat_lat_reversal_rate_count_test2 <- 
  dat_lat_reversal_rate_count_by_subject_test %>% 
  filter(sxx_exx == sett_proc$scenario) %>% 
  filter(interaction_type == "touch") %>% 
  pull(reversal_rate)



print(
  t.test(dat_lat_reversal_rate_count_test1, 
         dat_lat_reversal_rate_count_test2, 
         var.equal = F)
)


dat_reversal_rate_summary_key_viz <- 
  dat_lat_reversal_rate_count_by_subject %>% 
  filter(sxx_exx == "s02_e03") %>% 
  group_by(interaction_type) %>% 
  summarize(val_mean = mean(reversal_rate),
            val_sd = sd(reversal_rate),
            val_se = sd(reversal_rate) / sqrt(n()) ) %>% 
  mutate(interaction_type = factor(interaction_type, label = c("Gesture", "Touch")))


plot_steering_wheel_velocity <- 
  ggplot() + 
  geom_bar(data = dat_reversal_rate_summary_key_viz,
           aes_string(x = "interaction_type",
                      y = "val_mean",
                      fill = "interaction_type"),
           stat = "identity") + 
  geom_errorbar(data = dat_reversal_rate_summary_key_viz,
                aes(x = interaction_type,
                    ymin = val_mean - val_se,
                    ymax = val_mean + val_se),
                width = 0.2) + 
  scale_fill_manual(values = c("firebrick3", "dodgerblue4")) + 
  coord_cartesian(ylim = c(0, 1.5)) +
  scale_y_continuous(expand = c(0, 0)) +
  guides(fill = F, color = F) + 
  ggtitle("Average steering wheel reversal rate") + 
  labs(y = "Steering wheel reversal rate (SWR/s)") +
  theme_bw() +
  theme(title = element_text(size = 7, face = "bold")) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7)) + 
  theme(axis.text.x = element_text(size = 6, color = "black"),
        axis.text.y = element_text(size = 6, color = "black"))

plot(plot_steering_wheel_velocity)

ggsave(filename = "steering_wheel_reversal.png", 
       plot = plot_steering_wheel_velocity,
       path = "plots",
       width = 8,
       height = 5,
       units = "cm",
       dpi = 600)




# Viz reversal rate -------------------------------------------------------

dat_lat_reversal_rate_viz <- 
  dat_lat_reversal_rate %>% 
  filter(sxx_exx == "s02_e03") %>% 
  filter(subject_id == 551)

ggplot() +
  geom_line(data = dat_lat_reversal_rate_viz,
            aes_string(x = "sxx_exx_dti_m",
                       y = "steer_angle_deg_v2",
                       group = "case")) +
  geom_line(data = dat_lat_reversal_rate,
            aes_string(x = "sxx_exx_tti_s",
                       y = "steer_direction",
                       group = "case"),
            col = "blue",
            size = 2) +
  geom_line(data = dat_lat_reversal_rate,
            aes_string(x = "sxx_exx_tti_s",
                       y = "steer_direction_v2",
                       group = "case"),
            col = "red",
            size = 1) +
  facet_grid(sxx_exx~.)
