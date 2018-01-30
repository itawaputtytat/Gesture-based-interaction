

# Preparatory settings ----------------------------------------------------

sett_proc <- c()
sett_proc$df_name <- "dat_study5_t_adtf_sxx_exx_exx_full"
sett_proc$thresholds$am_limit1 <- -2
sett_proc$thresholds$am_limit2 <- 7.5
#sett_proc$col_name_am <- "sxx_exx_tti_s"
sett_proc$thresholds$am_limit1 <- 0
sett_proc$thresholds$am_limit2 <- 50
#sett_proc$col_name_am <- "sxx_exx_dti_m"
sett_proc$col_name_am <- "sxx_exx_tti_s"
#ett_proc$col_name_of_interest <- "itrace_yaw"
#sett_proc$col_name_of_interest <- "brake_pressure_bar"

#sett_proc$col_name_of_interest <- "itrace_acc_y"

sett_proc$col_name_of_interest <- "steer_angle_deg"
#sett_proc$col_name_of_interest <- "steer_angle_deg_v2"
sett_proc$col_name_of_interest <- "steer_angle_speed_degs"
#sett_proc$col_name_of_interest <- "steer_angle_acc_degs2"



# Data preparation --------------------------------------------------------

dat_lat <- 
  get(sett_proc$df_name) %>%  
  filter(is_usable) #%>% filter(subject_id == 520)

dat_lat <- 
  dat_lat %>% 
  mutate(steer_angle_deg_v2 = 
           ifelse(steer_angle_deg_sign == 1,
                  steer_angle_deg * -1,
                  steer_angle_deg)) %>% 
  mutate(steer_angle_speed_degs_v2 = 
           ifelse(steer_angle_deg_sign == 1,
                  steer_angle_speed_degs * -1,
                  steer_angle_speed_degs))

dat_lat <- 
  dat_lat %>% 
  group_by(case) %>% 
  filter_(paste(sett_proc$col_name_am, ">=", sett_proc$thresholds$am_limit1, "&",
                  sett_proc$col_name_am, "<=", sett_proc$thresholds$am_limit2)) %>% 
  mutate(time_s_diff = c(0, diff(time_s))) %>%
  mutate(steer_angle_speed_degs_diff = c(0, diff(steer_angle_speed_degs_v2))) %>% 
  mutate(steer_angle_acc_degs2 = steer_angle_speed_degs_diff / time_s_diff) %>% 
  #mutate(testi = smoothWithLoess(itrace_acc_y, 1/20, 1,  onlyfitted = T)) %>% 
  #mutate(testi_roll = rollAvg(steer_angle_speed_degs, k = 150, align = "center"))
  mutate_(.dots = setNames(
    list(interp(~rollAvg(v, k = 5, align = "center"),
                v = as.name(sett_proc$col_name_of_interest))),
    paste_(sett_proc$col_name_of_interest, "rollavg"))) 

dat_lat_summary <- 
  dat_lat %>% 
  filter_(paste(sett_proc$col_name_of_interest, "!= 0")) %>% 
  group_by(sxx_exx, interaction_type, sxx_exx_dti_m) %>% 
  summarize_(.dots = c(
    setNames(
      list(interp(~ mean(v, na.rm = T),
                  v = as.name(paste_(sett_proc$col_name_of_interest, "rollavg")))),
      paste_(paste_(sett_proc$col_name_of_interest, "rollavg"), "avg")
    ),
    setNames(
      list(interp(~ sd(v, na.rm = T),
                  v = as.name(paste_(sett_proc$col_name_of_interest, "rollavg")))),
      paste_(paste_(sett_proc$col_name_of_interest, "rollavg"), "sd")
    ),
    setNames(
      list(interp(~ mean(v),
                  v = as.name(sett_proc$col_name_of_interest))),
      paste_(sett_proc$col_name_of_interest, "avg")
    ),
    setNames(
      list(interp(~ max(v),
                  v = as.name(sett_proc$col_name_of_interest))),
      paste_(sett_proc$col_name_of_interest, "max")
    ),
    setNames(
      list(interp(~ sd(v),
                  v = as.name(sett_proc$col_name_of_interest))),
      paste_(sett_proc$col_name_of_interest, "sd")
    )
  )) 





# Visualize ---------------------------------------------------------------

plot_var <- 
  ggplot() +
  # geom_line(data = dat_lat,
  #           aes_string(x = sett_proc$col_name_am,
  #                      y = sett_proc$col_name_of_interest,
  #                      group = "case"),
  #           alpha = 0.5) +
  geom_line(data = dat_lat,
            aes_string(x = sett_proc$col_name_am,
                       #y = paste_(sett_proc$col_name_of_interest, "rollavg"),
                       y = sett_proc$col_name_of_interest,
                       group = "case",
                       color = "interaction_type"),
            alpha = 0.35) +
  # geom_line(data = dat_lat_summary,
  #           aes_string(x = sett_proc$col_name_am,
  #                      y = paste_(paste_(sett_proc$col_name_of_interest, "rollavg"), "avg"),
  #                      group = "interaction_type",
  #                      color = "interaction_type"),
  #           size = 2) +
  facet_grid(sxx_exx ~ interaction_type)
  #coord_cartesian(ylim = c(0, 20))
  #coord_cartesian(ylim = c(-100, 100))

windows(); plot(plot_var)




# t-test ------------------------------------------------------------------

dat_lat_summary_ind <- 
  dat_lat %>% 
  filter(sxx_exx_dti_m >= 0 & sxx_exx_dti_m <= 50) %>% 
  filter_(paste(sett_proc$col_name_of_interest, "!= 0")) %>% 
  group_by(sxx_exx, subject_id, interaction_type) %>% 
  summarize_(.dots = c(
    setNames(
      list(interp(~ max(v),
                  v = as.name(sett_proc$col_name_of_interest))),
      paste_(sett_proc$col_name_of_interest, "max")
    )
  )) 

ggplot() + 
  geom_boxplot(data = dat_lat_summary_ind,
               aes(x = interaction_type,
                   y = steer_angle_speed_degs_max)) + 
  facet_grid(.~sxx_exx) + 
  coord_cartesian(ylim = c(0, 100))

temp1 <- 
  dat_lat_summary_ind %>% 
  filter(sxx_exx == "s04_e01") %>% 
  filter(interaction_type == "gesture") %>% 
  mutate(is_outlier = codeOutliersZ(steer_angle_speed_degs_max)) %>% 
  filter(!is_outlier) %>% 
  pull(steer_angle_speed_degs_max)

temp2 <- 
  dat_lat_summary_ind %>% 
  filter(sxx_exx == "s04_e01") %>% 
  filter(interaction_type == "touch") %>% 
  mutate(is_outlier = codeOutliersZ(steer_angle_speed_degs_max)) %>% 
  filter(!is_outlier) %>% 
  pull(steer_angle_speed_degs_max)

t.test(temp1, temp2, paired = F, var.equal = F)



dat_lat_summary_key <- 
  dat_lat_summary_ind %>% 
  group_by(sxx_exx, interaction_type) %>% 
  summarize_(.dots = c(
    setNames(
      list(interp(~mean(v),
                  v = as.name(paste_(sett_proc$col_name_of_interest, "max")))),
      "val_mean"
    ),
    setNames(
      list(interp(~sd(v),
                  v = as.name(paste_(sett_proc$col_name_of_interest, "max")))),
      "val_sd"
    ),
    setNames(
      list(interp(~ (sd(v)/sqrt(n()) ),
                  v = as.name(paste_(sett_proc$col_name_of_interest, "max")))),
      "val_se"
    )
  ) 
  )
        
print(dat_lat_summary_key)


dat_lat_summary_key_viz <- 
  dat_lat_summary_key %>% 
  filter(sxx_exx == "s02_e03") %>% 
  mutate(interaction_type = factor(interaction_type, label = c("Gesture", "Touch")))

plot_steering_wheel_velocity <- 
  ggplot() + 
  geom_bar(data = dat_lat_summary_key_viz,
           aes_string(x = "interaction_type",
                      y = "val_mean",
                      fill = "interaction_type"),
           stat = "identity") + 
  geom_errorbar(data = dat_lat_summary_key_viz,
                aes(x = interaction_type,
                    ymin = val_mean - val_se,
                    ymax = val_mean + val_se),
                width = 0.2) + 
  scale_fill_manual(values = c("firebrick3", "dodgerblue4")) + 
  coord_cartesian(ylim = c(0, 50)) +
  scale_y_continuous(expand = c(0, 0)) +
  guides(fill = F, color = F) + 
  ggtitle("Average steering wheel velocity") + 
  labs(y = "Steering wheel velocity (deg/s)") +
  theme_bw() +
  theme(title = element_text(size = 7, face = "bold")) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7)) + 
  theme(axis.text.x = element_text(size = 6, color = "black"),
        axis.text.y = element_text(size = 6, color = "black"))

windows(); plot(plot_steering_wheel_velocity)

ggsave(filename = "steering_wheel_velocity.png", 
       plot = plot_steering_wheel_velocity,
       path = "plots",
       width = 8,
       height = 5,
       units = "cm",
       dpi = 600)


# Entropy -----------------------------------------------------------------

# test <- data.frame(a = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
# 
# test <- 
#   test %>% 
#   mutate(b = 
#            lag(a, 1) + 
#            (lag(a, 1) - lag(a, 2)) + 
#            1/2*( (lag(a, 1) - lag(a, 2)) - (lag(a, 2) - lag(a, 3)) )
#          ) %>% 
#   mutate(c = 
#            5/2 * lag(a, 1) - 
#            2 * lag(a, 2) + 
#            1/2 * lag(a, 3))
