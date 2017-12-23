dbCreateQueryString <- function(sxx,
                                sett_q = sett_query, 
                                sett_i = sett_id_names) {
  
  outputFunProc(R)
  
  ## Create sxx as character index (e.g. 1 as "s01")
  sxx_txt <- sxx
  
  ## SELECT
  SELECT <-
    c(sett_q$col_names_session, 
      paste0(sxx_txt, 
             createVector_var_sxx(sett_q$col_name_am_suffix)),
      sett_q$col_names_data)
  SELECT <- paste(SELECT, collapse = ",\n")
  SELECT <- paste("SELECT", SELECT, sep = "\n")
  
  ## FROM
  #FROM <- paste("FROM", sett_q$src, sep = "\n")
  FROM <- paste("FROM", paste_(sett_q$src_name_prefix, sxx_txt), sep = "\n")
  if (!is.null(sett_query$src_name_suffix))
    FROM <- paste_(FROM, sett_q$src_name_suffix)
  # if (!is.null(sett_q$col_name_am_suffix))
  #   FROM <- paste(FROM,sett_q$col_name_am_suffix, sep = "_")
  
  # ## WHERE
  WHERE_subject_id <-
    paste(paste(sett_i$active$subject, "=", sett_q$subject),
          collapse = " OR\n")
  
  ## Add buffer for selected distance criteria
  ## ... to enable correct adjustments
  ## (e.g. flawed DTI or TTI due to GPS anomalies)
  am_limit1_temp <- sett_q$am_limit1 - sett_q$am_buffer
  am_limit2_temp <- sett_q$am_limit2 + sett_q$am_buffer
  
  WHERE_am2sxx <-
    paste(
      paste0(sxx_txt, "_", sett_q$col_name_am_suffix, " >= ", am_limit1_temp),
      paste0(sxx_txt, "_", sett_q$col_name_am_suffix, " <= ", am_limit2_temp),
      sep = " AND\n")
  
  WHERE <- c(WHERE_subject_id, WHERE_am2sxx)
  WHERE <- paste0("(\n", WHERE, "\n)", collapse = " AND ")
  WHERE <- paste("WHERE", WHERE, sep = "\n")
  
  ## Final string
  query <- unlist(paste(SELECT, FROM, WHERE, sep = "\n", collapse = "\n\n"))
  
  outputDone()
  
  return(query)
}
