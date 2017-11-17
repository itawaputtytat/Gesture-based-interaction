dbGetSrc_rbind <- function (name4dbconn, prefix, suffix, name4df) {
  
  outputFunProc(R)
  
  ## Load tables created from suffix variable (= list of tables)
  dat2proc <- lapply(suffix, function(x)
    dbGetSrc(name4dbconn, paste(prefix, x, sep = "_")) )

  
  ## Rbind list of tables
  ## ... and assign new column with table names for indicating origin
  ## ... amd assign new columng for experimental focus of interest (expfocus)
  table_vector <- rep(paste(prefix, suffix, sep = "_"), lapply(dat2proc, nrow))
  expfocus_vector <- rep(suffix, lapply(dat2proc, nrow))

  dat2proc <- 
    cbind(do.call("rbind", dat2proc),
          table = table_vector,
          expfocus =  expfocus_vector,
          stringsAsFactors = F)

  assign(name4df, dat2proc, env = .GlobalEnv)
  outputString(paste("* Created object:", name4df))
  outputDone()
}