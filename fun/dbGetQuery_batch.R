dbGetQuery_batch <- function(db_conn_name,
                             sett_q = sett_query, 
                             sett_i = sett_id_names,
                             bind_rows = T, 
                             df_name = NULL,
                             df_name_prefix = "dat",
                             ceate_df_name_by_sxx_exx = T,
                             show_query_string = F,
                             ...) {
  
  outputFunProc(R)
  ptm <- proc.time()
  
  db_conn <- get(db_conn_name)
  
  ## Remember argument for settings for saving df_name
  sett_name <- deparse(substitute(sett_q))
  
  ## In case of row-binding of queried data is set to TRUE (default)
  ## ... initialise object for data collection
  if (bind_rows) 
    dat_coll <- c() 
  
  invisible( 
    lapply(sett_q$sxx_exx, function(sxx_exx, ...) {
      
      outputString(paste0("* Fetching ", sxx_exx, " ... "), linebreak = F)
      
      ## Create query string
      query <- dbCreateQueryString(sxx_exx, sett_q, sett_i)
      if (show_query_string)
        messageWithSepLine(query)
      
      ## Query data
      dat <- 
        dbGetQuery(db_conn, query, stringsAsFactors = F)
      outputDone(T)
      
      ## Add query settings to dat attributes
      eval_string <- paste0("attr(dat, \"sett_query\") <- sett_q")
      eval(parse(text = eval_string))
      
      ## Add query string to dat attributes
      eval_string <- paste0("attr(dat, \"query\") <- query")
      eval(parse(text = eval_string))
      
      ## In case of row-binding of queried data is set to TRUE (default)
      ## ... row-bind data into a single data.frame
      ## ... otherwise single objects will be created
      if (bind_rows) { 
        case <- 
          paste_(sxx_exx, sprintf("s%02d", dat[, sett_i$active$subject]))
        dat <- cbind(case, sxx_exx, dat, stringsAsFactors = F)
        dat <- renameVar_sxx_exx(dat)
        dat_coll <<- rbind(dat_coll, dat, stringsAsFactors = F) 
        
      } else {
        
        ## Create object name for final data
        if (ceate_df_name_by_sxx_exx)
          df_name <- paste_(sett_q$src_name_prefix, sxx_exx)
        
        if (!is.null(sett_q$df_name_prefix))
          df_name <- paste_(sett_q$df_name_prefix, df_name)
        
        if (!is.null(df_name_prefix))
          df_name <- paste_(df_name_prefix, df_name)
        
        assign(df_name, dat, envir = .GlobalEnv)
        outputString(paste("* New object:", df_name))
        rm(dat)
      }
    }) ## lapply
  ) ## invisible
  
  ## In case of row-binding of queried data is set to TRUE (default)
  ## ... assign final data to global environment
  if (bind_rows) {
    
    ## Create df name
    df_name <- paste_(sett_q$src_name_prefix, "sxx_exx_exx")
    
    if (!is.null(sett_q$src_name_suffix))
      df_name <- paste_(df_name, sett_q$src_name_suffix)
    
    if (!is.null(sett_q$df_name_prefix))
      df_name <- paste_(sett_q$df_name_prefix, df_name)
    
    if (!is.null(df_name_prefix))
      df_name <- paste_(df_name_prefix, df_name)
    
    sett_q$df_name <- df_name
    assign(sett_name, sett_q, envir = .GlobalEnv)
    
    assign(df_name, dat_coll, envir = .GlobalEnv)
    rm(dat_coll)
    
    ## Add sett_q to dat attributes
    eval_string <- paste0("attr(", df_name, ", \"sett_query\") <- sett_query")
    eval(parse(text = eval_string), envir = .GlobalEnv)
    
    outputString(paste("* New object:", df_name))
    outputString(paste("** (see attributes $sett_query for query settings)"))
  }
  
  
  outputProcTime(ptm)
  outputDone()
}
