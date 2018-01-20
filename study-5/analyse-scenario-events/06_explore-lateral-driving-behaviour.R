
# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$df_name <- paste_(sett_query$df_name, "intrpld")
#sett_dat$df_name <- paste_(sett_query$df_name)
sett_dat$col_name_am <- "sxx_exx_dti_m"
#sett_dat$col_name_am <- "sxx_exx_tti_s"
#sett_dat$col_name_am <- "tta_s"
sett_dat$col_name_case <- "case"
sett_dat$col_name_group <- "interaction_type"

sett_proc <- c()
sett_proc$plot_cases <- T
sett_proc$treshold_steer_angle_deg <- 15
sett_proc$treshold_steer_angle_degs <- 30
#ett_proc$col_name_of_interest <- "itrace_yaw"
#sett_proc$col_name_of_interest <- "steer_angle_deg"
#sett_proc$col_name_of_interest <- "steer_angle_deg_v2"
#sett_proc$col_name_of_interest <- "brake_pressure_bar"
#sett_proc$col_name_of_interest <- "steer_angle_speed_degs"
sett_proc$col_name_of_interest <- "itrace_acc_y"

#dat_pedal_summary_first_acc_after_braking



# Prepare data ------------------------------------------------------------

dat_proc <- 
  get(sett_dat$df_name) %>% 
  filter(is_usable) %>% 
  data.frame()

dat_proc$steer_angle_deg_v2 <- dat_proc$steer_angle_deg
dat_proc$steer_angle_deg_v2[dat_proc$steer_angle_deg_sign == 1] <- 
  dat_proc$steer_angle_deg[dat_proc$steer_angle_deg_sign == 1] * -1

dat_proc_summary <- 
  dat_proc %>% 
  group_by_("sxx_exx",
            sett_dat$col_name_group, 
            sett_dat$col_name_am) %>% 
  summarize_(.dots = setNames(
    list(interp(~ mean(v),
                v = as.name(sett_proc$col_name_of_interest))),
    sett_proc$col_name_of_interest)) %>% 
  arrange_(sett_dat$col_name_group)

## Code pedal activity
codePedalActivity(dat_proc,
                  colname_acc_pedal_pos = "acc_pedal_pos_perc",
                  colname_brake_status = "brake_pressure_status",
                  colname_brake_press = "brake_pressure_bar")



# Visualize variable of interest ------------------------------------------

test <- 
  dat_proc %>% 
  group_by(case, condition_code) %>% 
  summarise(tta_s = min(tta_s))

plot_var <- 
  ggplot() + 
  geom_line(data = dat_proc,
            aes_string(x = sett_dat$col_name_am,
                       y = sett_proc$col_name_of_interest,
                       group = sett_dat$col_name_case,
                       color = sett_dat$col_name_group),
            alpha = 0.35) +
  # geom_vline(data = test,
  #            aes_string(xintercept = sett_dat$col_name_am,
  #                       group = sett_proc$col_name_case,
  #                       color = sett_proc$colname_group),
  #            size = 1,
  #            alpha = 0.25)#+
  geom_line(data = dat_proc_summary,
            aes_string(x = sett_dat$col_name_am,
                       y = sett_proc$col_name_of_interest,
                       color = sett_dat$col_name_group),
            size = 2) + 
  #coord_cartesian(ylim = c(0, 200)) +
  facet_grid(interaction_type~sxx_exx)

windows(); plot(plot_var)



# Analyze TTA vs. variable of interest for each subject_id ----------------

## Scale speed in range 0 to 1
dat_proc$itrace_speed_y_scaled <- 
  (dat_proc$itrace_speed_y - min(dat_proc$itrace_speed_y)) / 
  (max(dat_proc$itrace_speed_y) - min(dat_proc$itrace_speed_y))

dat_dti_speed_min <- 
  dat_proc %>% 
  group_by(case) %>% 
  filter(itrace_speed_y == min(itrace_speed_y)) %>% 
  group_by(subject_id, case, condition_code) %>% 
  mutate(row_nr = row_number()) %>% 
  filter(row_nr == 1) %>% 
  ungroup() %>% 
  select(subject_id, sxx_exx_dti_m)

dat_proc$acc_pedal_status <- dat_proc$acc_pedal_pos_perc > 0

dat_proc <- 
  dat_proc %>% 
  mutate(pedal_act_color = ifelse(pedal_act == -1, "red",
                                  ifelse(pedal_Act == 0, "orange", "green")))


if (sett_proc$plot_cases) {
  
  ## Open window
  windows()
  
  ## Visualize each case
  #for(i in unique(dat_proc$subject_id)) {
  for (i in uniques) {
    #i <- 555
    plot_temp <- 
      ggplot() + 
      ## Deviation rectangle for steering angle
      geom_rect(aes(xmin = min(dat_proc[, sett_dat$col_name_am]),
                    xmax = max(dat_proc[, sett_dat$col_name_am]),
                    ymin = sett_proc$treshold_steer_angle_deg * -1,
                    ymax = sett_proc$treshold_steer_angle_deg),
                alpha = 0.25) +#yintercept = 10) + 
      ## Plot variable of interest
      geom_line(data = dat_proc %>% 
                  filter(subject_id == i),
                aes_string(x = sett_dat$col_name_am,
                           y = sett_proc$col_name_of_interest,
                           group = "subject_id"),
                #color = "factor(brake_pressure_status)"),
                #alpha = 0.5,
                size = 1) +
      geom_line(data = dat_proc %>% 
                  filter(subject_id == i),
                aes_string(x = sett_dat$col_name_am,
                           y = "steer_angle_speed_degs",
                           group = "subject_id"),
                #color = "factor(brake_pressure_status)"),
                alpha = 0.5,
                color = "blueviolet",
                size = 1) +
      geom_hline(yintercept = sett_proc$treshold_steer_angle_degs, color = "blueviolet") + 
      geom_hline(yintercept = sett_proc$treshold_steer_angle_deg) + 
      ## Plot line for TTA
      geom_vline(data = dat_tta_1st %>% filter(subject_id == i),
                 aes_string(xintercept = sett_dat$col_name_am,
                            group = "subject_id"),
                 size = 1,
                 color = "red2",
                 linetype = "dashed") +
      ## Plot line for first braking after TTA
      geom_vline(data = dat_pedal_summary_braking_first %>% 
                   filter(subject_id == i),
                 aes_string(#xintercept = sett_dat$col_name_am,
                   xintercept = "sxx_exx_dti_m_start",
                   group = "subject_id",
                 ),#color = factor(subject_id)),
                 size = 1,
                 color = "red2"
                 #alpha = 0.25) 
      ) + 
      # geom_line(data = dat_proc %>% 
      #             filter(subject_id == i),
      #           aes_string(x = sett_dat$col_name_am,
      #                      y = "itrace_speed_y_scaled",
      #                      group = "subject_id"),
      #           #alpha = 0.5,
      #           size = 1) +
      ## Plot speed with coding for pedal activity
      geom_line(data = dat_proc %>% 
                  filter(subject_id == i),
                aes_string(x = sett_dat$col_name_am,
                           y = "itrace_speed_ms * 3.6",
                           group = "subject_id",
                           color = "factor(pedal_act)"),
                #alpha = 0.5,
                size = 2) +
      scale_color_manual(values = c("red3", "orange", "green3")) +
      guides(color = F) +
      geom_vline(data = dat_dti_speed_min %>% 
                   filter(subject_id == i),
                 aes(xintercept = sxx_exx_dti_m),
                 color = "green3") +
      #coord_cartesian(ylim = c(0, 20)) + 
      #coord_cartesian(ylim = c(-5, 5)) + 
      #coord_cartesian(ylim = c(0, 600)) + 
      #coord_cartesian(ylim = c(-500, 500)) + 
      ggtitle(i)
    
    plot_temp2 <- 
      ggplot() + 
      geom_line(data = dat_proc %>% 
                  filter(subject_id == i),
                aes_string(x = sett_dat$col_name_am,
                           y = "itrace_acc_y",
                           group = "subject_id"),
                #color = "factor(brake_pressure_status)"),
                #alpha = 0.5,
                color = "darkgoldenrod2",
                size = 1)
    
      plot(
        grid.arrange(plot_temp, plot_temp2, nrow = 2)
      )
    
    pauseAndContinue()
  }
}


# Evasion maneuver --------------------------------------------------------

test <- dat_pedal

# codePedalActivity(test,
#                   colname_acc_pedal_pos = "acc_pedal_pos_perc",
#                   colname_brake_status = "brake_pressure_status",
#                   colname_brake_press = "brake_pressure_bar",
#                   colname_group = "case")

# test <- 
#   left_join(test,
#           dat_pedal_summary_first_acc_after_braking %>% 
#             select_(sett_dat$col_name_case, 
#                     "pedal_act_nr_first_acc_after_braking"),
#           by = sett_dat$col_name_case)

test <- 
  test %>% 
  group_by(case) %>% 
  filter(pedal_act_nr < pedal_act_nr_first_acc_after_braking) %>% 
  filter(itrace_speed_ms > 5/3.6) %>% 
  filter(steer_angle_speed_degs >= sett_proc$treshold_steer_angle_degs) %>% 
  filter(steer_angle_deg > sett_proc$treshold_steer_angle_deg)

uniques <- unique(test$subject_id)











# Analyze point-based key value -------------------------------------------

dat_proc_max <- 
  dat_proc %>% 
  filter(sxx_exx_dti_m >= 0) %>% 
  group_by_("sxx_exx", "case", "interaction_type") %>% 
  #summarize(steer_max = max(steer_angle_deg))
  #summarize(steer_max = max(steer_angle_speed_degs))
  summarize_(.dots = setNames(
    list(interp(~ max(v),
                v = as.name(sett_proc$col_name_of_interest))),
    sett_proc$col_name_of_interest))

dat_proc_max_summary <- 
  dat_proc_max %>%
  group_by_("sxx_exx", "interaction_type") %>% 
  summarize_(.dots = setNames(
    list(interp(~ mean(v),
                v = as.name(sett_proc$col_name_of_interest))),
    sett_proc$col_name_of_interest))

print(dat_proc_max_summary)

dat_proc_max$is_outlier <-
  codeOutliersZ(unlist(dat_proc_max[, sett_proc$col_name_of_interest]))

plot_max <- 
  ggplot() +
  geom_boxplot(data = dat_proc_max,
               aes_string(x = "interaction_type",
                          y = sett_proc$col_name_of_interest)) + 
  geom_point(data = dat_proc_max %>% 
               filter(is_outlier),
             aes_string(x = "interaction_type",
                        y = sett_proc$col_name_of_interest),
             color = "red") +
  facet_grid(.~sxx_exx) +
  coord_cartesian(ylim = c(0, 100)) +
  expand_limits(y = 0)

plot(plot_max)

## Remove outliers
dat_proc_max <-
  dat_proc_max %>%
  filter(!is_outlier)

row_finder_gesture <- dat_proc_max$interaction_type == "gesture"
row_finder_touch <- dat_proc_max$interaction_type == "touch"

print(
  t.test(unlist(dat_proc_max[row_finder_gesture, sett_proc$col_name_of_interest]),
         unlist(dat_proc_max[row_finder_touch, sett_proc$col_name_of_interest]),
         paired = F)
)




# Significance over time --------------------------------------------------

dat_proc_test <- 
  dat_proc %>% 
  filter(sxx_exx_dti_m >= -50 & sxx_exx_dti_m <= 0)

dat_coll_p <- c()

for (i in unique(dat_proc_test$sxx_exx_dti_m)) {
  print(i)
  
  dat_test <- 
    dat_proc_test %>% 
    mutate(sxx_exx_dti_m = round(sxx_exx_dti_m, 1)) %>% 
    filter(sxx_exx_dti_m == i) %>% 
    select_(sett_dat$col_name_group, 
            sett_proc$col_name_of_interest)
  
  dat_test_gesture <- 
    dat_test %>% 
    filter_(paste(sett_dat$col_name_group, "== \"gesture\""))
  
  dat_test_touch <- 
    dat_test %>% 
    filter_(paste(sett_dat$col_name_group, "== \"touch\""))
  
  ttest_result <- 
    t.test(dat_test_gesture[, sett_proc$col_name_of_interest],
           dat_test_touch[, sett_proc$col_name_of_interest],
           paired = F)
  
  dat_coll_p <- c(dat_coll_p, ttest_result$p.value)
}

plot_p_over_time <- 
  ggplot() + 
  geom_line(data = data.frame(sxx_exx_dti_m = unique(dat_proc_test$sxx_exx_dti_m),
                              p = dat_coll_p),
            aes(x = sxx_exx_dti_m,
                y = p)) + 
  geom_hline(yintercept = 0.05)

plot(plot_p_over_time)


# fsdfsd ------------------------------------------------------------------

dat_pedal_act_steer_first <- 
  dat_proc %>% 
  filter(tta_s >= sett_proc$threshold$tta_s * -1) %>% 
  #filter_(paste(sett_proc$col_name_dti, "<= 0")) %>% 
  filter(steer_angle_deg >= 25) %>% 
  group_by_(sett_dat$col_name_case, sett_dat$col_name_group) %>% 
  mutate(row_nr = row_number()) %>% 
  filter(row_nr == 1) %>% 
  select_("tta_s", "time_s", sett_dat$col_name_am)

test_diff <- 
  left_join(dat_tta_1st %>% 
              select(case, condition_code, time_s),
            dat_pedal_act_steer_first %>% 
              select(case, time_s_min)) %>% 
  mutate(time_s_diff = time_s_min - time_s) %>% 
  filter(time_s_diff >= 0.2)

test_diff$is_outlier <- codeOutliersZ(test_diff$time_s_diff)

test_diff_summary <- 
  test_diff %>%
  group_by(condition_code) %>% 
  summarize(time_s_diff = mean(time_s_diff))

print(test_diff_summary)

plot_diff <- ggplot() +
  geom_boxplot(data = test_diff,
               aes(x = condition_code,
                   y = time_s_diff)) + 
  geom_point(data = test_diff %>% filter(is_outlier),
             aes(x = condition_code,
                 y = time_s_diff),
             color = "red") + 
  #coord_cartesian(ylim = c(0, NA))
  expand_limits(y = 0)

plot(plot_diff)

# test_diff <-
#   test_diff %>%
#   filter(!is_outlier)

print(
  t.test(test_diff$time_s_diff[test_diff$condition_code == "gesture"],
         test_diff$time_s_diff[test_diff$condition_code == "touch"],
         paired = F)
)



