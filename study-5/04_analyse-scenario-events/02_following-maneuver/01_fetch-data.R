
# Query settings ----------------------------------------------------------

sett_query <- c()
sett_query$db_name <- "GBI_Study-5"
sett_query$db_conn_name <- dbFindConnObj(sett_query$db_name, output = F)
sett_query$src_name_prefix <- "t_adtf"
sett_query$src_name_suffix <- "full"
#sett_query$src_name_suffix <- "full_tti_rnd1"
sett_query$df_name_prefix <- "study5"

sett_query$sxx_exx <- c(
  "s00_e01",
  "s00_e02",
  "s01_e02",
  "s02_e03",
  "s03_e01",
  "s04_e01"
  )

#sett_query$am_limit1 <- -150
sett_query$am_limit1 <- -50
sett_query$am_limit2 <- 125

sett_query$subject <- c(500:600)
sett_query$col_name_am_suffix <- "dti_m"
# sett_query$col_name_am_suffix <- "tti_s_rnd1"
sett_query$col_name_am <- paste_("sxx_exx", sett_query$col_name_am_suffix)

# sett_query$am_limit2 <- 5
sett_query$am_buffer <- 0
sett_query$col_names_session <-
  c("subject_id",
    "scenario_id",
    "time_s",
    "dist_m")

sett_query$col_names_data <-
  c("gps_lon",
    "gps_lat",
    "speed_kmh",
    "itrace_speed_y",
    "itrace_speed_z",
    "itrace_acc_y",
    "itrace_acc_z",
    "acc_pedal_pos_perc",
    "brake_pressure_status",
    "brake_pressure_bar",
    "steer_angle_deg",
    "steer_angle_deg_sign",
    "steer_angle_speed_degs",
    "itrace_yaw")

sett_query$filter$sets <-
  list(
    list("subject_id", sett_query$subject, "=", "OR"),
    list(sett_query$col_name_am, 
         sett_query$am_limit1 - sett_query$am_buffer, ">="),
    list(sett_query$col_name_am, 
         sett_query$am_limit2 + sett_query$am_buffer, "<=")
  )
sett_query$filter$bool_op_between <- c("OR")
sett_query$filter$bool_op_between <- c("AND")




# Data fetching -----------------------------------------------------------

dbGetQuery_batch(sett_query$db_conn_name, sett_query, bind_rows = T)


