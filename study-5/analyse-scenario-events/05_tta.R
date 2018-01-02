
# Preparatory settings ----------------------------------------------------

sett_proc <- c()
sett_proc$df_name <- "dat_study5_t_adtf_sxx_exx_exx_full_intrpld"
sett_proc$col_name_dti <- "sxx_exx_dti_m"
sett_proc$threshold$tta_s <- 3
sett_proc$threshold$brake_pressure_bar <- 0.1
sett_proc$threshold$brake_pressure_bar <- 10
if (sett_query$sxx_exx == "s02_e02") {
  sett_proc$threshold$tta_s <- 8
}
#sett_proc$threshold$tta_s <- 3

sett_dat <- c()
sett_dat$db_conn_name <- dbFindConnObj(sett_query$db_name, output = F)


# Compute TTA -------------------------------------------------------------

dat_test <- 
  left_join(get(sett_proc$df_name),
  # left_join(get(sett_proc$df_name) %>% mutate(sxx_exx_dti_m = sxx_exx_dti_m + 23),
  # left_join(get(sett_proc$df_name) %>% mutate(sxx_exx_dti_m = sxx_exx_dti_m + 75),
            dat_conditions %>% select(subject_id, condition_scenario)) %>% 
  mutate(itrace_speed_ms = abs(itrace_speed_y)) %>% 
  mutate_(.dots = setNames(list(
    interp(~ (v) / abs(w),
           v = as.name(sett_proc$col_name_dti),
           w = as.name("itrace_speed_ms"))),
    "tta_s"))




# Visualize distance vs. TTA ----------------------------------------------

ggplot() +
  geom_line(data = dat_test,
            aes_string(x = sett_proc$col_name_dti,
                       y = "tta_s",
                       group = "subject_id",
                       color = "condition_scenario"),
            alpha = 0.5) + 
  coord_cartesian(ylim = c(-20, 20)) +
  geom_hline(yintercept = sett_proc$threshold$tta_s * -1)



# Visualize speed profiles ------------------------------------------------

ggplot() + 
  geom_line(data = dat_test,
            aes_string(x = sett_proc$col_name_dti,
                       y = "abs(itrace_speed_y)",
                       group = "subject_id",
                       color = "condition_scenario"),
            alpha = 0.5,
            size = 1)



# Viz bar ttc -------------------------------------------------------------

## Filter for TTA first falling below -3
dat_test_tta_1st <- 
  dat_test %>% 
  filter(tta_s >= sett_proc$threshold$tta_s * -1) %>% 
  #filter(tta >= -8.1) %>% 
  filter_(paste(sett_proc$col_name_dti, "<= 0")) %>% 
  group_by(subject_id, case, condition_scenario) %>% 
  mutate(row_nr = row_number()) %>% 
  filter(row_nr == 1) %>% 
  select(tta_s, time_s, sxx_exx_dti_m, itrace_speed_ms)
  #filter(brake_pressure_status == 1) %>% 
  # group_by(subject_id, case, condition_scenario) %>% 
  # summarize(tta_min = min(tta),
  #           time_s_min_tta3 = min(time_s),
  #           sxx_exx_dti_m_min_tta3 = min(sxx_exx_dti_m),
  #           itrace_speed_ms)

## Check if some cases have not been captured
finder <- unique(dat_test$subject_id) %in% dat_test_tta_1st$subject_id
finder <- unique(dat_test$subject_id)[!finder]
print(finder)

ggplot() + 
  geom_line(data = dat_test,
            aes_string(x = "sxx_exx_dti_m",
                       y = "itrace_speed_ms",
                       group = "subject_id",
                       color = "condition_scenario"),
            alpha = 0.5,
            size = 1) +
  geom_vline(data = dat_test_tta_1st,
             aes(xintercept = sxx_exx_dti_m,
                 group = subject_id,
                 color = condition_scenario),
             size = 1,
             alpha = 0.25) 
  # geom_line(data = dat_test %>% 
  #             filter(subject_id %in% finder),
  #           aes_string(x = "itrace_speed_ms",
  #                      y = "speed_kmh"),
  #           color = "red",
  #           size = 1)



# Compute difference TTA und first braking --------------------------------

dat_pedal_act_break_first <- 
  dat_test %>% 
  filter(tta_s >= sett_proc$threshold$tta_s * -1) %>% 
  #filter_(paste(sett_proc$col_name_dti, "<= 0")) %>% 
  filter(brake_pressure_status == 1) %>% 
  filter(brake_pressure_bar >= sett_proc$threshold$brake_pressure_bar) %>% 
  group_by(subject_id, case, condition_scenario) %>% 
  mutate(row_nr = row_number()) %>% 
  filter(row_nr == 1) %>% 
  select(tta_s, time_s_min = time_s, sxx_exx_dti_m, itrace_speed_ms)
#filter(brake_pressure_status == 1)



# Analyze TTA vs. first braking for each subject_id -----------------------

windows()

for(i in unique(dat_test$subject_id)) {
  #i = 523
  plot_temp <- ggplot() + 
    geom_line(data = dat_test %>% filter(subject_id == i),
              aes_string(x = "sxx_exx_dti_m",
                         y = "itrace_speed_ms",
                         group = "subject_id",
                         color = "factor(brake_pressure_status)"),
              #alpha = 0.5,
              size = 1) +
    geom_vline(data = dat_test_tta_1st %>% filter(subject_id == i),
               aes(xintercept = sxx_exx_dti_m,
                   group = subject_id),
               size = 1,
               color = "green4") +
    geom_vline(data = dat_pedal_act_break_first %>% filter(subject_id == i),
               aes(xintercept = sxx_exx_dti_m,
                   group = subject_id,
               ),#color = factor(subject_id)),
               size = 1
               #alpha = 0.25) 
    ) + 
    coord_cartesian(ylim = c(0, 20)) + 
    ggtitle(i)
  
  plot(plot_temp)
  
  pauseAndContinue()
}


# Compute time diff -------------------------------------------------------




test_diff <- 
  left_join(dat_test_tta_1st %>% 
              select(case, condition_scenario, time_s),
            dat_pedal_act_break_first %>% 
              select(case, time_s_min)) %>% 
  mutate(time_s_diff = time_s_min - time_s) %>% 
  filter(time_s_diff >= 0.2)
  #filter(time_s_diff <= 3)

## Remove outliers by hand
#finder <- which(test_diff$condition_scenario == "touch" & test_diff$time_s_diff >= 2.25)
## S01_E01
# finder <- which(test_diff$condition_scenario == "gesture" & test_diff$time_s_diff >= 2.25)
# test_diff <- test_diff[-finder, ]

## S01_E03
# finder <- which(test_diff$condition_scenario == "gesture" & test_diff$time_s_diff >= 1.4)
# test_diff <- test_diff[-finder, ]

## S02_E02 (bringt aber nichts)
# finder <- which(test_diff$condition_scenario == "gesture" & test_diff$time_s_diff >= 6)
# test_diff <- test_diff[-finder, ]
# # finder <- which(test_diff$condition_scenario == "gesture" & test_diff$time_s_diff <= 4)
# # test_diff <- test_diff[-finder, ]
# finder <- which(test_diff$condition_scenario == "touch" & test_diff$time_s_diff >= 6)
# test_diff <- test_diff[-finder, ]
# finder <- which(test_diff$condition_scenario == "touch" & test_diff$time_s_diff <= 4)
# test_diff <- test_diff[-finder, ]

# finder <- which(test_diff$time_s_diff >= 2.25)
# test_diff <- test_diff[-finder, ]

## S04_E02 (bringt aber nichts)
# finder <- which(test_diff$condition_scenario == "gesture" & test_diff$time_s_diff >= 1.2)
# test_diff <- test_diff[-finder, ]

test_diff_summary <- 
  test_diff %>%
  group_by(condition_scenario) %>% 
  summarize(time_s_diff = mean(time_s_diff))

print(test_diff_summary)

ggplot() +
  geom_boxplot(data = test_diff,
           aes(x = condition_scenario,
               y = time_s_diff))

print(
  t.test(test_diff$time_s_diff[test_diff$condition_scenario == "gesture"],
       test_diff$time_s_diff[test_diff$condition_scenario == "touch"],
       paired = F)
)


# # fsdfdsf -----------------------------------------------------------------
# 
# ggplot() + 
#   geom_bar(data = 
#              dat_pedal_act_summary %>% 
#              filter(pedal_act == -1) %>% 
#              group_by(case, condition_scenario) %>% 
#              summarize(tta_start = max(tta_start)),
#            aes(x = condition_scenario,
#                y = tta_start),
#            stat = "identity")

# plot_test <-
#   plot_pedal_act_brake_first + 
#   geom_vline(data = dat_test_tta_1st, 
#              aes(xintercept = sxx_exx_tti_s_rnd1))
# 
# plot(plot_test)
# 
# windows(); plot(plot_pedal_act_brake_first)
#      
