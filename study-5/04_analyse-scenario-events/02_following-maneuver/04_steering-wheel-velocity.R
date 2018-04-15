
# Preparatory settings ----------------------------------------------------

## Data
sett_proc <- c()
sett_proc$df_name <- "dat_study5_t_adtf_sxx_exx_exx_full"
sett_proc$scenario <- "s02_e03"

## Thresholds
sett_proc$thresholds$am_limit1 <- 0
sett_proc$thresholds$am_limit2 <- 50
sett_proc$thresholds$elapsed_time_s = 8

sett_proc$scenario <- "s02_e03"

## Column names
#sett_proc$col_name_am <- "sxx_exx_dti_m"
#sett_proc$col_name_am <- "sxx_exx_tti_s"
sett_proc$col_name_group <- "interaction_type"
sett_proc$col_name_of_interest <- "steer_angle_speed_degs"
#sett_proc$col_name_of_interest <- "steer_angle_acc_degs2"



# Data preparation --------------------------------------------------------

dat_swv <- 
  get(sett_proc$df_name) %>% 
  filter(is_usable) %>% 
  filter(sxx_exx_dti_m >= 0) %>% 
  group_by(case) %>% 
  filter(time_s <= (min(time_s) + sett_proc$thresholds$elapsed_time_s))
  #filter_(paste(sett_proc$col_name_am, "<=", sett_proc$thresholds$am_limit2))

## Duplicate steering angle and steering angle velocity with correct sign
dat_swv <- 
  dat_swv %>% 
  mutate(steer_angle_deg_v2 = 
           ifelse(steer_angle_deg_sign == 1,
                  steer_angle_deg * -1,
                  steer_angle_deg)) %>% 
  mutate(steer_angle_speed_degs_v2 = 
           ifelse(steer_angle_deg_sign == 1,
                  steer_angle_speed_degs * -1,
                  steer_angle_speed_degs))



# Data processing ---------------------------------------------------------

## Compute steering wheel acceleration
## Smooth values

dat_swv <- 
  dat_swv %>% 
  group_by(case) %>% 
  mutate(time_s_diff = c(0, diff(time_s))) %>%
  mutate(steer_angle_speed_degs_diff = c(0, diff(steer_angle_speed_degs_v2))) %>% 
  mutate(steer_angle_acc_degs2 = steer_angle_speed_degs_diff / time_s_diff) %>% 
  mutate_(.dots = setNames(
    list(interp(~rollAvg(v, k = 5, align = "center"),
                v = as.name(sett_proc$col_name_of_interest))),
    paste_(sett_proc$col_name_of_interest, "rollavg"))) 



# Extract key measures ----------------------------------------------------

dat_swv_measure <- 
  dat_swv %>% 
  #filter_(paste(sett_proc$col_name_of_interest, "!= 0")) %>% 
  group_by(subject_id, sxx_exx, interaction_type) %>% 

  summarize_(.dots = c(
    setNames(
      list(interp(~ mean(v, na.rm = T),
                  v = as.name(sett_proc$col_name_of_interest))),
      paste_(sett_proc$col_name_of_interest, "avg")
    ),
    setNames(
      list(interp(~ sd(v, na.rm = T),
                  v = as.name(sett_proc$col_name_of_interest))),
      paste_(sett_proc$col_name_of_interest, "max")
    ),
    setNames(
      list(interp(~ max(v, na.rm = T),
                  v = as.name(sett_proc$col_name_of_interest))),
      paste_(sett_proc$col_name_of_interest, "sd")
    ),
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
      list(interp(~ max(v, na.rm = T),
                  v = as.name(paste_(sett_proc$col_name_of_interest, "rollavg")))),
      paste_(paste_(sett_proc$col_name_of_interest, "rollavg"), "max")
    )
  )) 



# Vizualize: Boxplot ------------------------------------------------------

plot_swr_summary_by_subject <- 
  ggplot() + 
  geom_boxplot(data = dat_swv_measure,
               aes_string(x = "interaction_type",
                          y = paste_(sett_proc$col_name_of_interest, "max")),
               outlier.colour = "red") + 
  facet_grid(.~sxx_exx) 

plot(plot_swr_summary_by_subject)



# Analysis ----------------------------------------------------------------

dat_swv_measure <- 
  dat_swv_measure %>% 
  mutate_(.dots = setNames(
    list(interp(~ codeOutliersZ(v),
                v = as.name(paste_(sett_proc$col_name_of_interest, "max")))),
         paste("is_outlier")))

## Extract baseline values
dat_swv_measure_baseline <- 
  dat_swv_measure %>% 
  filter(!is_outlier) %>% 
  filter(sxx_exx %in% c("s00_e01", "s00_e02")) %>% 
  ungroup() %>% 
  summarize_(.dots = c(
    setNames(
      list(interp(~ mean(v, na.rm = T),
                  v = as.name(paste_(sett_proc$col_name_of_interest, "max")))),
      paste_(paste_(sett_proc$col_name_of_interest, "max"), "avg")
    )))
    
dat_swv_measure_gbi <- 
  dat_swv_measure %>% 
  filter(sxx_exx == sett_proc$scenario) %>% 
  filter(interaction_type == "gesture")

dat_swv_measure_tbi <- 
  dat_swv_measure %>% 
  filter(sxx_exx == sett_proc$scenario) %>% 
  filter(interaction_type == "touch")

print(t.test(dat_swv_measure_gbi %>% 
               pull(steer_angle_speed_degs_max), 
             mu = 5.349261))

print(t.test(dat_swv_measure_tbi %>% 
               pull(steer_angle_speed_degs_max), 
             mu = 5.349261))




# Summarize data ----------------------------------------------------------

sett_proc$col_name_of_interest_suffix <- "max"

dat_swv_summary <- 
  dat_swv_measure %>% 
  filter(sxx_exx %in% c("s00_e01", "s00_e02", "s02_e03")) %>% 
  mutate_(.dots = setNames(
    list(interp(~codeOutliersZ(v),
                v = as.name(paste_(sett_proc$col_name_of_interest, 
                                   sett_proc$col_name_of_interest_suffix)))),
    "is_outlier")) %>%
  filter(!is_outlier) %>% 
  group_by(interaction_type) %>% 
  summarize_(.dots = c(
    setNames(
      list(interp(~mean(v),
                  v = as.name(paste_(sett_proc$col_name_of_interest, 
                                     sett_proc$col_name_of_interest_suffix)))),
      "val_mean"
    ),
    setNames(
      list(interp(~sd(v),
                  v = as.name(paste_(sett_proc$col_name_of_interest, 
                                     sett_proc$col_name_of_interest_suffix)))),
      "val_sd"
    ),
    setNames(
      list(interp(~ (sd(v)/sqrt(n()) ),
                  v = as.name(paste_(sett_proc$col_name_of_interest, 
                                     sett_proc$col_name_of_interest_suffix)))),
      "val_se"
    ))) 



# Vizualize: Bar chart ----------------------------------------------------

## Prepare data
dat_swv_summary_plot <- 
  dat_swv_summary %>% 
  mutate(interaction_type = 
         factor(interaction_type, 
                level = c("none", "gesture", "touch"),
                label = c("Baseline", "Gesture", "Touch")))

## Plot
plot_swv_avg <- 
  ggplot() + 
  geom_bar(data = dat_swv_summary_plot,
           aes_string(x = "interaction_type",
                      y = "val_mean",
                      fill = "interaction_type"),
           stat = "identity") + 
  geom_errorbar(data = dat_swv_summary_plot,
                aes(x = interaction_type,
                    ymin = val_mean - val_se,
                    ymax = val_mean + val_se),
                width = 0.2)

plot(plot_swv_avg)



# Post-processing ---------------------------------------------------------

## Scales
plot_swv_avg <- 
  plot_swv_avg + 
  scale_fill_manual(values = c("grey50", "firebrick3", "dodgerblue4")) +
  coord_cartesian(ylim = c(0, 10)) +
  scale_y_continuous(expand = c(0, 0))

## Text
plot_swv_avg <- 
  plot_swv_avg +
  guides(fill = F, color = F) +
  ggtitle("Average steering wheel velocity") + 
  labs(y = "Steering wheel velocity (deg/s)") 

## Theme
plot_swv_avg <- 
  plot_swv_avg + 
  theme_bw() +
  theme(title = element_text(size = 7, face = "bold")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7)) +
  theme(axis.text.x = element_text(size = 6, color = "black"),
        axis.text.y = element_text(size = 6, color = "black"))

plot(plot_swv_avg)



# Save plot ---------------------------------------------------------------

ggsave(filename = "steering_wheel_velocity.png", 
       plot = plot_swv_avg,
       path = "plots",
       width = 8,
       height = 5,
       units = "cm",
       dpi = 600)
