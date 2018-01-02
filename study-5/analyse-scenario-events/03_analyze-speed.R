
# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$db_conn_name <- dbFindConnObj(sett_query$db_name, output = F)

sett_proc <- c()
sett_proc$df_name <- "dat_study5_t_adtf_sxx_exx_exx_full_intrpld"
#sett_proc$col_name_am <- "sxx_exx_dti_m_rnd1"
#sett_proc$col_name_am <- "sxx_exx_tti_s_rnd1"
sett_proc$col_name_am <- "sxx_exx_tti_s"
sett_proc$col_name_am <- "sxx_exx_dti_m"

sett_proc$colname_id    <- "case"
sett_proc$colname_group <- "condition_scenario"



# Summarize speed profiles ------------------------------------------------

dat_adtf_cond_summary <- 
  get(sett_proc$df_name) %>% 
  group_by_("condition_scenario", sett_proc$col_name_am) %>% 
  summarize(itrace_speed_y = mean(itrace_speed_y * -1)) %>% 
  arrange(condition_scenario)



# Visualize speed profiles ------------------------------------------------

plot_speed <-
  ggplot() +
  geom_line(#data = get(sett_proc$df_name),
    data = left_join(dat_study5_t_adtf_sxx_exx_exx_full, 
                     dat_conditions %>% select(subject_id, condition_scenario)),
            aes_string(x = sett_proc$col_name_am,
                       y = "itrace_speed_y * -1",
                       group = sett_proc$colname_id,
                       color = sett_proc$colname_group),
            alpha = 0.5) + 
  geom_line(data = dat_adtf_cond_summary,
            aes_string(x = sett_proc$col_name_am,
                       y = "itrace_speed_y",
                       color = sett_proc$colname_group),
            size = 2) + 
  ggtitle(sett_query$sxx_exx)

windows(); plot(plot_speed)



