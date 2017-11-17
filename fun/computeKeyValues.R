
# Processing --------------------------------------------------------------

computeKeyValues <- function (name4df, 
                              var2group = NULL) {
  
  outputFunProc(R)
  
  ## Compute values
    dat2proc <- 
      get(paste(name4df, ".long", sep = "")) %>%
      group_by_(.dots = lapply(c(var2group, "variable"), as.symbol)) %>%
      summarise(median = as.numeric(median(value, na.rm = T)),
                mean   = mean(value, na.rm = T),
                sd     = sd(value, na.rm = T)) %>% 
      mutate(se = sd / sqrt(n())) %>% 
      mutate(median_jitter = jitter(median))
  
  #if (!is.null(var2group))
    name4df_new <- paste(name4df, ".long.stats_", var2group, sep = "") #else
      #name4df_new <- paste(name4df, ".long.stats", sep = "")
  
  ## Re-assign data with processed data
  assign(name4df_new, dat2proc, env = .GlobalEnv)
  
  outputString(paste("* Created object:", name4df_new))
  outputDone()
} 