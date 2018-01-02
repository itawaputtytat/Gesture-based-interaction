intrpldf_batch(sett_query$df_name, 
               col_name_ref = sett_query$col_name_am, 
               col_name_group = "case",
               stepsize = 0.1,
               binary_vars = c("brake_pressure_status"),
               suffix = "intrpld", 
               outputFlag = T)

test1 <- test0 %>% select(sxx_exx_dti_m, itrace_speed_y)

test3 <- 
  test1 %>% 
  mutate(sxx_exx_dti_m = round(sxx_exx_dti_m, 1)) %>% 
  group_by(sxx_exx_dti_m) %>% 
  summarize_all("max")

template2 <- data.frame(sxx_exx_dti_m = seq(min(test3$sxx_exx_dti_m), max(test3$sxx_exx_dti_m), 0.1))
template2$sxx_exx_dti_m <- as.character(template2$sxx_exx_dti_m)
test3 <- left_join(template2, test3 %>% mutate(sxx_exx_dti_m = as.character(sxx_exx_dti_m)))

test2 <- intrpldf(test1, "sxx_exx_dti_m", stepsize = 0.1)
#print(head(test2))

## Add conditions

col_finder <- 
  paste0("condition_scenario_t", 
         substr(sett_query$sxx_exx, start = 3, 3))

## Load conditions
dat_conditions <- 
  dbGetSrc(sett_dat$db_conn_name, "t_link_subjects_and_conditions") %>% 
  mutate(condition_scenario_t1 = 
           ifelse(substr(condition_code, 1, 1) == "G", 
                  "gesture", 
                  "touch")) %>% 
  mutate(condition_scenario_t2 = 
           ifelse(substr(condition_code, 4, 4) == "G", 
                  "gesture", 
                  "touch")) %>% 
  mutate(condition_scenario_t3 = 
           ifelse(substr(condition_code, 7, 7) == "G", 
                  "gesture", 
                  "touch")) %>% 
  mutate(condition_scenario_t4 = 
           ifelse(substr(condition_code, 10, 10) == "G", 
                  "gesture", 
                  "touch")) %>% 
  mutate_(.dots = setNames(list(interp(~v, v = as.name(col_finder))),
                           "condition_scenario")) %>% 
  filter(!is.na(condition_code))

dat_study5_t_adtf_sxx_exx_exx_full_intrpld <- 
  left_join(get(paste_(sett_query$df_name, "intrpld")),
          # left_join(get(sett_proc$df_name) %>% mutate(sxx_exx_dti_m = sxx_exx_dti_m + 23),
          # left_join(get(sett_proc$df_name) %>% mutate(sxx_exx_dti_m = sxx_exx_dti_m + 75),
          dat_conditions %>% select(subject_id, condition_scenario))
