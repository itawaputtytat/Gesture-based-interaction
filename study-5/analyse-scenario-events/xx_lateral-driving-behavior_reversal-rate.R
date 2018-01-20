
# Preparatory settings ----------------------------------------------------

sett_proc <- c()
sett_proc$df_name <- "dat_study5_t_adtf_sxx_exx_exx_full"
sett_proc$thresholds$am_limit1 <- 0
sett_proc$thresholds$am_limit2 <- 90
sett_proc$thresholds$steer_angle_deg <- 0

plot_template <- 
  ggplot() + 
  ggtitle(label = "Reversal rate",
          subtitle = paste("AM:", sett_proc$thresholds$am_limit1,
                           "to", sett_proc$thresholds$am_limit2))



# Data preparation --------------------------------------------------------

dat_lat <- 
  get(sett_proc$df_name) %>% 
  #filter(subject_id == 526) %>% 
  ## Exclude third scenario
  filter(!grepl("s03", sxx_exx)) %>%
  ## Filter for steering angle
  filter(steer_angle_deg >= sett_proc$thresholds$steer_angle_deg) %>% 
  # filter(sxx_exx_dti_m >= sett_proc$thresholds$am_limit1 & 
  #          sxx_exx_dti_m <= sett_proc$thresholds$am_limit2) %>% 
  group_by(sxx_exx, case) %>% 
  filter(sxx_exx_dti_m >= 0 & tta_s <= 5) %>% 
  filter(is_usable)



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
  mutate(steer_direction = 
           ifelse(steer_angle_deg_v2 < lag(steer_angle_deg_v2, default = 0),
                  -1, 0)) %>% 
  mutate(steer_direction = 
           ifelse(steer_angle_deg_v2 > lag(steer_angle_deg_v2, default = 0),
                  1, steer_direction)) %>% 
  mutate(steer_direction_v2 = 
           ifelse(steer_direction == 0 & steer_angle_deg_v2 != 0, 
                  NA, steer_direction)) %>% 
  mutate(steer_direction_v2 = na.locf(steer_direction_v2, na.rm = F)) %>% 
  mutate(steer_direction_v2 = 
           ifelse(steer_direction_v2 == 1 & lag(steer_direction_v2 == -1),
                  0, steer_direction_v2)) %>% 
  
  ## Compute sequences
  mutate(row_nr = row_number()) %>% 
  mutate(steer_direction_v2_cumsum = c(0, diff(steer_direction_v2))) %>% 
  group_by(sxx_exx, case, steer_direction_v2) %>%
  mutate(row_nr_seq = row_number()) %>%
  mutate(seq_id = row_nr - row_nr_seq) %>%

  ## Variable selection
  select(sxx_exx, case, interaction_type, sxx_exx_dti_m,
         time_s, steer_angle_deg, steer_angle_deg_v2, steer_direction, steer_direction_v2,
         seq_id, 
         steer_direction_v2_cumsum) 



# Visualize individual case -----------------------------------------------

# plot(dat_lat_reversal_rate$sxx_exx_dti_m, dat_lat_reversal_rate$steer_angle_deg_v2, type = "l")
# lines(dat_lat_reversal_rate$sxx_exx_dti_m, dat_lat_reversal_rate$steer_direction_v2, col = "red")



# Compute reversal rate ---------------------------------------------------

dat_lat_reversal_rate_count_by_subject <- 
  dat_lat_reversal_rate %>% 
  
  ## Compute elapsed time in data section
  group_by(sxx_exx, case, interaction_type, seq_id) %>% 
  mutate(elapsed_time_s = max(time_s) - min(time_s)) %>% 
  
  ## Filter for non-zero steer angle sequences
  filter(steer_direction_v2 != 0) %>% 
  
  ## Count sequences
  group_by(sxx_exx, case, interaction_type) %>% 
  summarize(n_reversals = n_distinct(seq_id),
            elapsed_time_s = max(elapsed_time_s) - min(elapsed_time_s)) %>% 
  
  ## Compute reversal rate
  mutate(reversal_rate = n_reversals / elapsed_time_s) %>% 
  
  group_by(sxx_exx, interaction_type) %>% 
  mutate(is_outlier = codeOutliersZ(reversal_rate)) 



# Visualize reversale rate ------------------------------------------------

plot_reversal_rate_boxplot <- 
  plot_template +
  geom_boxplot(data = dat_lat_reversal_rate_count_by_subject,
               aes(x = interaction_type,
                   y = reversal_rate)) + 
  geom_point(data = dat_lat_reversal_rate_count_by_subject %>% 
               filter(is_outlier),
             aes(x = interaction_type,
                 y = reversal_rate),
             color = "red") + 
  facet_grid(.~sxx_exx, scales = "free")

plot(plot_reversal_rate_boxplot)
  


# Analysis ----------------------------------------------------------------

dat_lat_reversal_rate_count_by_subject_test <- 
  dat_lat_reversal_rate_count_by_subject %>% 
  filter(!is_outlier) %>% 
  filter(sxx_exx != "s00_e01") %>% 
  filter(sxx_exx != "s00_e02") %>% 
  filter(sxx_exx != "s04_e01") 

dat_lat_reversal_rate_count_test1 <- 
  dat_lat_reversal_rate_count_by_subject_test %>% 
  filter(sxx_exx == "s02_e03") %>% 
  filter(interaction_type == "gesture") %>% 
  pull(reversal_rate)

dat_lat_reversal_rate_count_test2 <- 
  dat_lat_reversal_rate_count_by_subject_test %>% 
  filter(sxx_exx == "s02_e03") %>% 
  filter(interaction_type == "touch") %>% 
  pull(reversal_rate)



print(
  t.test(dat_lat_reversal_rate_count_test1, 
         dat_lat_reversal_rate_count_test2, 
         var.equal = F,
         paired = F)
)


