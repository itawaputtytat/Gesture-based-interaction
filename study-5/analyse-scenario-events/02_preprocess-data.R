intrpldf_batch(sett_query$df_name, 
               col_name_ref = sett_query$col_name_am, 
               col_name_group = "case",
               stepsize = 0.1,
               binary_vars = c("brake_pressure_status"),
               suffix = "intrpld", 
               outputFlag = T)
