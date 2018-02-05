
# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$df_name <- sett_query$df_name
sett_dat$db_conn_name <- dbFindConnObj(sett_query$db_name, output = F)

sett_proc <- c()
sett_proc$cases <- sett_query$sxx_exx
sett_proc$scenario_ids <- as.numeric(substr(sett_proc$cases, 3, 3))
sett_proc$event_ids <- as.numeric(substr(sett_proc$cases, 7, 7))
sett_proc$col_name_am <- sett_query$col_name_am
sett_proc$col_name_dti <- "sxx_exx_dti_m"
sett_proc$col_name_case <- "case"
sett_proc$col_name_group <- "condition_code"
sett_proc$plot <- F

## TTA for critical events: 3 s
sett_proc$threshold$tta_s <- 3

## ... except for s02_e02 (evasion maneuver of opposing traffic)
if ( (2 %in% sett_proc$scenario_ids & 2 %in% sett_proc$event_ids) ) {
  sett_proc$threshold$tta_s <- 8
}

## TTA for following events: 3.5 s
if ( (1 %in% sett_proc$scenario_ids & 2 %in% sett_proc$event_ids) | 
     (2 %in% sett_proc$scenario_ids & 3 %in% sett_proc$event_ids) |
     (3 %in% sett_proc$scenario_ids & 1 %in% sett_proc$event_ids) |
     (4 %in% sett_proc$scenario_ids & 1 %in% sett_proc$event_ids) ) {
  
  sett_proc$threshold$tta_s <- 3.5
  
}



# Add column event_id -----------------------------------------------------

assign(sett_dat$df_name,
       get(sett_dat$df_name) %>% 
         mutate(event_id = as.numeric(substr(sxx_exx, 7, 7))))



# Add information on experimental condition -------------------------------

dat_study5_t_adtf_sxx_exx_exx_full_intrpld$interaction_type <- NULL
dat_study5_t_adtf_sxx_exx_exx_full_intrpld$interaction_type.x <- NULL
dat_study5_t_adtf_sxx_exx_exx_full_intrpld$interaction_type.y <- NULL
dat_study5_t_adtf_sxx_exx_exx_full_intrpld$interaction_complexity <- NULL
dat_study5_t_adtf_sxx_exx_exx_full_intrpld$interaction_complexity.x <- NULL
dat_study5_t_adtf_sxx_exx_exx_full_intrpld$interaction_complexity.y <- NULL
dat_study5_t_adtf_sxx_exx_exx_full_intrpld$condition_code <- NULL
dat_study5_t_adtf_sxx_exx_exx_full_intrpld$condition_code.x <- NULL
dat_study5_t_adtf_sxx_exx_exx_full_intrpld$condition_code.y <- NULL

## Load condition data and extract information
dat_conditions <- 
  dbGetSrc(sett_dat$db_conn_name, 
           "t_link_subjects_and_conditions") %>% 
  separate(condition_code, into = paste(1:4), sep = "-") %>% 
  gather(key = "scenario_id", value = "condition_code", -subject_id) %>% 
  mutate(scenario_id = as.numeric(scenario_id)) %>% 
  mutate(interaction_type = substr(condition_code, 1, 1),
         interaction_complexity = substr(condition_code, 2, 2)) %>% 
  mutate(interaction_type = 
           ifelse(interaction_type == "G", 
                  "gesture", 
                  interaction_type),
         interaction_type = 
           ifelse(interaction_type == "T", 
                  "touch", 
                  interaction_type)) %>% 
  mutate(interaction_complexity = 
           ifelse(interaction_complexity == "S", 
                  "simple", 
                  interaction_complexity),
         interaction_complexity =
           ifelse(interaction_complexity == "C", 
                  "complex", 
                  interaction_complexity))
#filter(!is.na(condition_code))

dat_conditions_baseline <-
  get(sett_query$df_name) %>%
  distinct(subject_id) %>%
  mutate(scenario_id = 0,
         condition_code = "B",
         interaction_type = "none",
         interaction_complexity = "none")

dat_conditions <- 
  rbind(dat_conditions,
        dat_conditions_baseline)

assign( sett_dat$df_name,
        left_join(get(sett_dat$df_name),
                  dat_conditions, #%>% 
                  # select("subject_id", 
                  #        "scenario_id",
                  #        "interaction_type",
                  #        "interaction_complexity")
        by = c("subject_id", "scenario_id")) )

# 
# 
# if (0 %in% sett_proc$scenario_ids) {
#   
#   assign(sett_dat$df_name,
#          get(sett_dat$df_name) %>% 
#            group_by(scenario_id) %>% 
#            mutate(condition_code = 
#                     ifelse(condition_code == NA, "B", condition_code),
#                   interaction_type = 
#                     ifelse(interaction_type == NA, "none", interaction_type),
#                   interaction_complexity = 
#                     ifelse(interaction_complexity == NA, "none", interaction_complexity)))
#   
#   # ## Code as baseline
#   # dat_conditions <- 
#   #   get(sett_query$df_name) %>% 
#   #   distinct(subject_id) %>% 
#   #   mutate(scenario_id = 0,
#   #          condition_code = "B",
#   #          interaction_type = "none",
#   #          interaction_complexity = "none")
# 
# } 

# assign( sett_dat$df_name,
#         left_join(get(sett_dat$df_name),
#                   dat_conditions, #%>% 
#                   # select("subject_id", 
#                   #        "scenario_id",
#                   #        "interaction_type",
#                   #        "interaction_complexity")
#                   by = c("subject_id", "scenario_id")) )



# Add information on usable events ----------------------------------------

dat_usable_events <- 
  dbGetSrc(sett_dat$db_conn_name, "v_adtf_useable_events") %>% 
  select(-file_name) %>% 
  gather("event_id", "is_usable", -subject_id, -scenario_id, -discard) %>% 
  mutate(event_id = as.numeric(substr(event_id, 8, 8)))

assign(sett_dat$df_name,
       left_join(get(sett_dat$df_name),
                 dat_usable_events,
                 by = c("subject_id", "scenario_id", "event_id")))


# Interpolation -----------------------------------------------------------

intrpldf_batch(sett_dat$df_name,
               col_name_ref = sett_proc$col_name_am,
               col_name_group = sett_proc$col_name_case,
               stepsize = 0.1,
               binary_vars = c("brake_pressure_status", "steer_angle_deg_sign", "is_usable"),
               suffix = "intrpld",
               outputFlag = T)

## Change settings for dataframe name to interpolated data
sett_dat$df_name <- paste_(sett_dat$df_name, "intrpld")


# Add info on indicator vehicle -------------------------------------------

dat_positions_indicator <- 
  dbGetSrc(sett_dat$db_conn_name, "v_vtd_coordinates_gps_events_start") %>% 
  select("scenario_id", 
         "event_id", 
         "conflict_vs_player_gps_dist_m")

assign(sett_dat$df_name,
       get(sett_dat$df_name) %>% 
         left_join(dat_positions_indicator) %>% 
         ungroup() %>% 
         group_by_("sxx_exx") %>% 
         mutate(dist_to_indicator_m = 
                  sxx_exx_dti_m - conflict_vs_player_gps_dist_m) )



# Compute TTA -------------------------------------------------------------

assign(sett_dat$df_name,
       get(sett_dat$df_name) %>% 
         mutate(itrace_speed_ms = abs(itrace_speed_y)) %>% 
         ungroup() %>% 
         group_by_("sxx_exx") %>% 
         mutate_(.dots = setNames(list(
           interp(~ (v) / abs(w),
                  v = as.name(sett_proc$col_name_dti),
                  w = as.name("itrace_speed_ms"))),
           "tta_s"))
)



# Visualize distance vs. TTA ----------------------------------------------

plot_distance_vs_tta <- 
  ggplot() +
  geom_line(data = get(sett_dat$df_name), #%>% filter(subject_id == 524),
            aes_string(x = sett_proc$col_name_dti,
                       y = "tta_s",
                       group = sett_proc$col_name_case,
                       color = sett_proc$col_name_group),
            alpha = 0.5) + 
  coord_cartesian(ylim = c(-20, 20)) +
  geom_hline(yintercept = sett_proc$threshold$tta_s * -1)

if (sett_proc$plot) {
  
  plot(plot_distance_vs_tta)
  
}



# First data falling below TTA threshold ----------------------------------

## As result of the interpolation it's possible to get more insight to the data

dat_tta_1st <- 
  get(sett_dat$df_name) %>% 
  #filter(tta_s >= round(sett_proc$threshold$tta_s, 0) * -1) %>% 
  filter(tta_s >= sett_proc$threshold$tta_s * -1) %>% 
  filter_(paste(sett_proc$col_name_dti, "<= 0")) %>% 
  group_by_("subject_id", 
            "case") %>% 
  mutate(row_nr = row_number()) %>% 
  filter(row_nr == 1) 

## Check if some cases have not been captured
finder <- 
  !(distinct_(get(sett_dat$df_name), "subject_id") %>% pull()) %in% 
  dat_tta_1st$subject_id

finder <- (distinct_(get(sett_dat$df_name), "subject_id") %>% pull())[finder]
print(finder)
