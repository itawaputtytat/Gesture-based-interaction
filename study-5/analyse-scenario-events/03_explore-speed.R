
# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$db_conn_name <- dbFindConnObj(sett_query$db_name, output = F)
sett_dat$df_name <- "dat_study5_t_adtf_sxx_exx_exx_full_intrpld"

sett_proc <- c()
sett_proc$case <- sett_query$sxx_exx
#sett_proc$col_name_am <- "sxx_exx_dti_m_rnd1"
#sett_proc$col_name_am <- "sxx_exx_tti_s_rnd1"
#sett_proc$col_name_am <- "sxx_exx_tti_s"
sett_proc$col_name_am <- "sxx_exx_dti_m"
sett_proc$col_name_speed <- "itrace_speed_ms"

sett_proc$colname_subject <- "subject_id"
sett_proc$colname_case <- "case"
#sett_proc$colname_group <- "condition_code"
sett_proc$colname_group <- "sxx_exx"



# Summarize speed profiles ------------------------------------------------

dat_adtf_cond_summary <-
  get(sett_dat$df_name) %>%
  group_by_(sett_proc$colname_group, sett_proc$col_name_am) %>%
  summarize_(.dots = setNames(
    list(interp(~ mean(v),
                v = as.name(sett_proc$col_name_speed))),
    sett_proc$col_name_speed)) %>%
  arrange_(sett_proc$colname_group)

# dat_adtf_cond_summary <- 
#   get(sett_dat$df_name) %>% 
#   #group_by_("condition_code", sett_proc$col_name_am) %>% 
#   group_by(case) %>% 
#   mutate(time_s_v2 = time_s - min(time_s)) %>% 
#   group_by_("condition_code", "time_s_v2") %>% 
#   summarize(sxx_exx_dti_m = min(sxx_exx_dti_m),
#             itrace_speed_y = mean(itrace_speed_y * -1)) %>% 
#   arrange(condition_code)



# Visualize speed profiles ------------------------------------------------

plot_speed <-
  ggplot() +
  geom_line(#data = get(sett_proc$df_name),
    data = get(sett_dat$df_name),
    aes_string(x = sett_proc$col_name_am,
               y = sett_proc$col_name_speed,
               group = sett_proc$colname_case,
               color = sett_proc$colname_group),
    alpha = 0.5) + 
  geom_line(data = get(sett_dat$df_name) %>% filter(!is_usable),
            aes_string(x = sett_proc$col_name_am,
                       y = sett_proc$col_name_speed,
                       group = "case"),
            color = "red",
            size = 1) + 
  geom_line(data = dat_adtf_cond_summary,
            aes_string(x = sett_proc$col_name_am,
                       y = sett_proc$col_name_speed,
                       color = sett_proc$colname_group),
            size = 2) + 
  ggtitle(sett_proc$case)

windows(); plot(plot_speed)



# Add marker for individual TTA -------------------------------------------

plot_speed_tta <- 
  plot_speed + 
  geom_vline(data = dat_tta_1st,
             aes_string(xintercept = sett_proc$col_name_am,
                        group = sett_proc$colname_subject,
                        color = sett_proc$colname_group),
             size = 1,
             alpha = 0.25)

plot(plot_speed_tta)
