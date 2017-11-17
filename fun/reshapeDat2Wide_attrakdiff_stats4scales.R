reshapeDat2Wide_attrakdiff_stats4scales <- function (name4df, suffix, groupvar) {

  outputFunProc(R)
  
  name4df <- paste(name4df, suffix, sep = "")
  dat2proc <- get(name4df) %>% data.frame()
  dat2proc <- melt(dat2proc, id = c(groupvar, "scale"))
  dat2proc <- dcast(dat2proc, as.formula(paste(groupvar, "~ scale + variable", sep = "")))
  
  name4df_new <- paste(name4df, "wide", sep = "_")
  assign(name4df_new, dat2proc, env = .GlobalEnv)
  
  cat("** Created variable: ", name4df_new, "\n", sep = "")
  cat("*** Done! *** \n\n")
  
}