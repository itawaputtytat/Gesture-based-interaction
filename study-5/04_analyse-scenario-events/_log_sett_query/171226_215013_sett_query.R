writeSelfAsLog("sett_query")

# Query settings ----------------------------------------------------------

sett_query <- c()
sett_query$db_name <- "GBI_Study-5"
sett_query$db_conn_name <- dbFindConnObj(sett_query$db_name, output = F)
sett_query$src_name_prefix <- "t_adtf"
sett_query$src_name_suffix <- "full"
sett_query$df_name_prefix <- "study5"
#sett_query$sxx_exx <- "s01_e01" ## could be significant look for brake pressure bar > 3 and look for outliers
#sett_query$sxx_exx <- "s01_e03" ## not s.
#sett_query$sxx_exx <- "s02_e01" 
#sett_query$sxx_exx <- "s02_e02" ## propably not significant
#sett_query$sxx_exx <- "s03_e02" ## significant
#sett_query$sxx_exx <- "s03_e03" ## not significant
sett_query$sxx_exx <- "s04_e02"
#sett_query$sxx_exx <- "s04_e03"
sett_query$subject <- c(500:600)
sett_query$col_name_am_suffix <- "dti_m"
sett_query$col_name_am <- paste_("sxx_exx", sett_query$col_name_am_suffix)
sett_query$am_limit1 <- -75
sett_query$am_limit2 <- 25
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
    "brake_pressure_bar")

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



# Data processing ---------------------------------------------------------

dbGetQuery_batch(sett_query$db_conn_name, sett_query, bind_rows = T)


