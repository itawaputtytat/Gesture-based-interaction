writeSelfAsLog("sett_query")

# Query settings ----------------------------------------------------------

sett_query <- c()
sett_query$db_name <- "GBI_Study-5"
sett_query$db_conn_name <- dbFindConnObj(sett_query$db_name, output = F)
sett_query$src_name_prefix <- "t_adtf"
sett_query$src_name_suffix <- "full_dti_rnd1"
sett_query$df_name_prefix <- "study5"
sett_query$sxx_exx <- "s02_e02"
sett_query$subject <- c(500:600)
sett_query$col_name_am_suffix <- "dti_m_rnd1"
sett_query$col_name_am <- paste_("sxx_exx", sett_query$col_name_am_suffix)
sett_query$am_limit1 <- -100
sett_query$am_limit2 <- 50
sett_query$am_buffer <- 50
sett_query$col_names_session <-
  c("subject_id",
    "scenario_id",
    "time_s",
    "dist_m")

sett_query$col_names_data <-
  c("speed_kmh",
    "brake_pressure_status")

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
