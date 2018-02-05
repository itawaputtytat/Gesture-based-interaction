
# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$db_conn_name <- dbFindConnObj(sett_query$db_name, output = F)

sett_proc <- c()
sett_proc$df_name <- "dat_study5_t_adtf_sxx_exx_exx_full_intrpld"
sett_proc$col_name_dti <- "sxx_exx_dti_m"
sett_proc$plot_cases <- F

## Braking pressure thresholds
#sett_proc$threshold$brake_pressure_bar <- 0.1
sett_proc$threshold$brake_pressure_bar <- 8 #s01_e01, s02_e02, s04_e03
if (sett_query$sxx_exx == "s03_e02") {
  sett_proc$threshold$brake_pressure_bar <- 0 #s03e02
}

# # TTA tresholds
sett_proc$threshold$tta_s <- 3
if (sett_query$sxx_exx == "s02_e02") {
  sett_proc$threshold$tta_s <- 8
}

## Correct reaction time
## Only evasive maneuver must be adjusted
## Evasing vehicle needs 4 seconds until it begins to changes its lane
## This time will be substracted from the reaction time
## ... as this marks the time, where subject could actually see something and react
sett_proc$correct_rt <- 0
if (sett_query$sxx_exx == "s02_e02") {
  sett_proc$correct_rt <- -4
}


# Get data ----------------------------------------------------------------

dat_brake <- 
  get(sett_proc$df_name) %>% 
  filter(usable) %>% 
  filter_(paste(sett_proc$col_name_dti, ">= -100", "&",
                sett_proc$col_name_dti, "<= 25"))



# Compute difference TTA und first braking --------------------------------

dat_brake_1st <-
  dat_brake %>%
  filter(tta_s >= sett_proc$threshold$tta_s * -1) %>%
  #filter_(paste(sett_proc$col_name_dti, "<= 0")) %>%
  filter(brake_pressure_status == 1) %>%
  filter(brake_pressure_bar >= sett_proc$threshold$brake_pressure_bar) %>%
  group_by(subject_id, case, condition_scenario) %>%
  mutate(row_nr = row_number()) %>%
  filter(row_nr == 1) %>%
  select(tta_s, time_s_min = time_s, sxx_exx_dti_m, sxx_exx_tti_s, itrace_speed_ms)

# dat_brake_1st <-
#   dat_pedal_summary_last_acc_before_braking %>%
#   filter(tta_s_min >= sett_proc$threshold$tta_s * -1) %>%
#   #filter_(paste(sett_proc$col_name_dti, "<= 0")) %>%
#   #filter(brake_pressure_status == 1) %>%
#   #filter(brake_pressure_bar >= sett_proc$threshold$brake_pressure_bar) %>%
#   group_by(subject_id, case, condition_scenario) %>%
#   mutate(row_nr = row_number()) %>%
#   filter(row_nr == 1) %>%
#   select(tta_s_min, time_s_min = time_s_min, sxx_exx_dti_m = sxx_exx_dti_m_start)




# Analyze TTA vs. first braking for each subject_id -----------------------

if (sett_proc$plot_cases) {
  
  ## Open window
  windows()
  
  ## Visualize each case
  for(i in unique(dat_brake$subject_id)) {
    #i = 523
    plot_temp <- 
      ggplot() + 
      geom_line(data = dat_brake %>% filter(subject_id == i),
                aes_string(x = "sxx_exx_dti_m",
                           y = "itrace_speed_ms",
                           group = "subject_id",
                           color = "factor(brake_pressure_status)"),
                #alpha = 0.5,
                size = 1) +
      geom_vline(data = dat_brake_tta_1st %>% filter(subject_id == i),
                 aes(xintercept = sxx_exx_dti_m,
                     group = subject_id),
                 size = 1,
                 color = "green4") +
      geom_vline(data = dat_brake_1st %>% filter(subject_id == i),
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
}



# Compute time diff -------------------------------------------------------

dat_time_diff <- 
  left_join(dat_tta_1st %>% 
              select(case, condition_scenario, time_s),
            dat_brake_1st %>% 
              select(case, time_s_min)) %>% 
  mutate(time_s_diff = time_s_min - time_s + sett_proc$correct_rt) %>%
  filter(time_s_diff >= 0.4)

## Code outliers
#dat_time_diff$is_outlier <- codeOutliersZ(dat_time_diff$time_s_diff)
dat_time_diff <- 
  dat_time_diff %>% 
  group_by(condition_scenario) %>% 
  mutate(is_outlier = codeOutliersZ(time_s_diff))

dbWriteTable(get(sett_dat$db_conn_name),
             paste_("t_results_time_s_tta_to_brake_1st", sett_query$sxx_exx),
             dat_time_diff,
             row.names = F,
             overwrite = T)


## Remove outliers by hand
#finder <- which(dat_time_diff$condition_scenario == "touch" & dat_time_diff$time_s_diff >= 2.25)
## S01_E01
# finder <- which(dat_time_diff$condition_scenario == "gesture" & dat_time_diff$time_s_diff >= 2.25)
# dat_time_diff <- dat_time_diff[-finder, ]

## S01_E03
# finder <- which(dat_time_diff$condition_scenario == "gesture" & dat_time_diff$time_s_diff >= 1.4)
# dat_time_diff <- dat_time_diff[-finder, ]

## S02_E02 (bringt aber nichts)
# finder <- which(dat_time_diff$time_s_diff >= 6)
# dat_time_diff <- dat_time_diff[-finder, ]
# finder <- which(dat_time_diff$condition_scenario == "touch" & dat_time_diff$time_s_diff <= 3)
# dat_time_diff <- dat_time_diff[-finder, ]

# # finder <- which(dat_time_diff$condition_scenario == "gesture" & dat_time_diff$time_s_diff <= 4)
# # dat_time_diff <- dat_time_diff[-finder, ]
# finder <- which(dat_time_diff$condition_scenario == "touch" & dat_time_diff$time_s_diff >= 6)
# dat_time_diff <- dat_time_diff[-finder, ]
# finder <- which(dat_time_diff$time_s_diff >= 2.25)
# dat_time_diff <- dat_time_diff[-finder, ]

## S04_E02 (bringt aber nichts)
# finder <- which(dat_time_diff$condition_scenario == "gesture" & dat_time_diff$time_s_diff >= 1.2)
# dat_time_diff <- dat_time_diff[-finder, ]

dat_time_diff_summary <- 
  dat_time_diff %>%
  filter(!is_outlier) %>% 
  group_by(condition_scenario) %>% 
  summarize(time_s_diff = mean(time_s_diff))

print(dat_time_diff_summary)

plot_time_diff <- 
  ggplot() +
  geom_boxplot(data = dat_time_diff,
               aes(x = condition_scenario,
                   y = time_s_diff)) + 
  geom_point(data = dat_time_diff %>% filter(is_outlier),
             aes(x = condition_scenario,
                 y = time_s_diff),
             color = "red") + 
  #coord_cartesian(ylim = c(0, NA))
  expand_limits(y = 0)

plot(plot_time_diff)

dat_time_diff <-
  dat_time_diff %>%
  filter(!is_outlier)

print(
  t.test(dat_time_diff$time_s_diff[dat_time_diff$condition_scenario == "gesture"],
       dat_time_diff$time_s_diff[dat_time_diff$condition_scenario == "touch"],
       paired = F,
       alternative = "less")
)


