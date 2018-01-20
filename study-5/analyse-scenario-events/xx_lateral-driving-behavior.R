


# Smooth ------------------------------------------------------------------

dat_lat <- 
  dat_lat %>% 
  filter(is_usable) %>% 
  group_by(case) %>% 
  #mutate(testi = smoothWithLoess(itrace_acc_y, 1/20, 1,  onlyfitted = T)) %>% 
  #mutate(testi_roll = rollAvg(steer_angle_speed_degs, k = 150, align = "center"))
  mutate(testi_roll = rollAvg(steer_angle_deg, k = 150, align = "center"))
#mutate(testi_roll = rollAvg(steer_angle_deg, k = 150, align = "center"))



# t-test ------------------------------------------------------------------



test_analysis <-
  test %>% 
  filter(sxx_exx_dti_m >= 10 & sxx_exx_dti_m <= 60) %>% 
  group_by(case, interaction_type) %>% 
  summarize(var_sd = sd(testi_roll))

test_analysis$is_outlier <- codeOutliersZ(test_analysis$var_sd)

plot_boxplot_steer <- 
  ggplot() +
  geom_boxplot(data = test_analysis,
               aes(x = interaction_type,
                   y = var_sd)) + 
  geom_point(data = test_analysis %>% filter(is_outlier),
             aes(x = interaction_type,
                 y = var_sd),
             color = "red") + 
  #coord_cartesian(ylim = c(0, NA))
  expand_limits(y = 0)

windows(); plot(plot_boxplot_steer)

test_analysis <-
  test_analysis %>%
  filter(!is_outlier)

print(
  t.test(test_analysis$var_sd[test_analysis$interaction_type == "gesture"],
         test_analysis$var_sd[test_analysis$interaction_type == "touch"],
         paired = F)
)




# Viz speed ---------------------------------------------------------------

ggplot() + 
  geom_line(data = dat_study5_t_adtf_sxx_exx_exx_full_intrpld,
            aes(x = sxx_exx_dti_m,
                y = itrace_speed_ms,
                group = case,
                color = condition_scenario)) + 
  geom_vline(data = dat_tta_1st,
             aes_string(xintercept = "sxx_exx_dti_m",
                        group = "subject_id"),
             size = 1,
             color = "red2",
             linetype = "dashed")


ggplot() + 
  geom_line(data = test %>% filter(subject_id == 526),
            aes(x = sxx_exx_dti_m,
                y = steer_angle_speed_degs,
                group = case),
            alpha = 0.5) + 
  geom_line(data = test %>% filter(subject_id == 526),
            aes(x = sxx_exx_dti_m,
                y = testi_roll,
                group = case),
            color = "red",
            size = 2) + 
  geom_vline(data = dat_tta_1st %>% filter(subject_id == 526),
             aes_string(xintercept = "sxx_exx_dti_m",
                        group = "subject_id"),
             size = 1,
             color = "red2",
             linetype = "dashed") + 
  facet_grid(condition_scenario ~ .) #+
#coord_cartesian(ylim = c(-5, 5))


ggplot() + 
  geom_line(
    data = test,
    aes(x = sxx_exx_dti_m,
        #y = itrace_yaw,
        #y = itrace_acc_y, 
        #y = testi,
        y = testi_roll,
        group = case,
        color = condition_scenario)) + 
  geom_vline(data = dat_tta_1st,
             aes_string(xintercept = "sxx_exx_dti_m",
                        group = "subject_id"),
             size = 1,
             color = "red2",
             linetype = "dashed") + 
  facet_grid(condition_scenario ~ .) #+
#coord_cartesian(ylim = c(-5, 5))

