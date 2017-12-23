
# Preparatory settings ----------------------------------------------------

sett_proc$col_name_dti <- "sxx_exx_tti_s_rnd1"



# Compute TTA -------------------------------------------------------------

dat_test <- 
  left_join(get(sett_proc$df_name),
            dat_conditions) %>% 
  mutate(speed_kmh_v2 = abs(itrace_speed_y) * 3.6) %>% 
  mutate_(.dots = setNames(list(
    interp(~v / abs(w),
           v = as.name(sett_proc$col_name_dti),
           w = as.name("itrace_speed_y"))),
    "tta"))



# Visualize distance vs. TTA ----------------------------------------------

ggplot() +
  geom_line(data = dat_test,
            aes_string(x = "sxx_exx_tti_s_rnd1",
                       y = "tta",
                       group = "subject_id",
                       color = "condition_scenario"),
            alpha = 0.5) + 
  coord_cartesian(ylim = c(-20, 20)) +
  geom_hline(yintercept = -3)



# Visualize speed profiles ------------------------------------------------

ggplot() + 
  geom_line(data = dat_pedal_act,
            aes_string(x = sett_proc$col_name_dti,
                       y = "speed_kmh",
                       group = "subject_id",
                       color = "condition_scenario"),
            alpha = 0.5,
            size = 1)



# Viz bar ttc -------------------------------------------------------------

## Filter for TTA first falling below -3
dat_test_tta_1st <- 
  dat_test %>% 
  group_by(subject_id) %>% 
  filter(tta >= -3) %>% 
  filter(sxx_exx_dti_m <= 0 & sxx_exx_dti_m >= -50) %>% 
  # mutate(tta_diff = abs(tta+3)) %>% 
  # # filter(tta_diff <= 0.055) %>% 
  # filter(tta_diff <= 0.5) %>% 
  #filter(sxx_exx_tti_s_rnd1 == min(sxx_exx_tti_s_rnd1))
  filter(brake_pressure_status == 1) %>% 
  group_by(subject_id, condition_scenario) %>% 
  summarize(sxx_exx_tti_s_rnd1 = min(sxx_exx_tti_s_rnd1),
            sxx_exx_dti_m = min(sxx_exx_dti_m))

## Check if some cases have not been captured
finder <- unique(dat_test$subject_id) %in% dat_test_tta_1st$subject_id
finder <- unique(dat_test$subject_id)[!finder]
print(finder)

ggplot() + 
  geom_line(data = dat_test,
            aes_string(x = "sxx_exx_dti_m",
                       y = "speed_kmh",
                       group = "subject_id",
                       color = "condition_scenario"),
            alpha = 0.5,
            size = 1) +
  geom_vline(data = dat_test_tta_1st,
             aes(xintercept = sxx_exx_dti_m,
                 group = subject_id,
                 color = condition_scenario),
             size = 1) + 
  geom_line(data = dat_test %>% 
              filter(subject_id %in% finder),
            aes_string(x = "sxx_exx_dti_m",
                       y = "speed_kmh"),
            color = "red",
            size = 1)



# Compute difference TTA und first braking --------------------------------

test_diff <- 
  left_join(dat_test_tta_1st %>% 
              select(case, condition_scenario, time_s),
            dat_pedal_act_break_first %>% 
              select(case, time_s_min)) %>% 
  mutate(time_s_diff = time_s_min- time_s) %>% 
  filter(time_s_diff >= 0)

test_diff %>%
  group_by(condition_scenario) %>% 
  summarize(time_s_diff = mean(time_s_diff))

ggplot() +
  geom_bar(data = test_diff,
           aes(x = condition_scenario,
               y = time_s_diff),
           stat = "identity")

t.test(test_diff$time_s_diff[test_diff$condition_scenario == "gesture"],
       test_diff$time_s_diff[test_diff$condition_scenario == "touch"],
       paired = F)


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

plot_test <-
  plot_pedal_act_brake_first + 
  geom_vline(data = dat_test_tta_1st, 
             aes(xintercept = sxx_exx_tti_s_rnd1))

plot(plot_test)

windows(); plot(plot_pedal_act_brake_first)
     
