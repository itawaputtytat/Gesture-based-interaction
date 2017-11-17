dcastData4AttrakDiff_stats4scale <- function (srcname, name4df, groupvar) {

  cat("* Processing with function: dcastData4AttrakDiff_stats4scale \n")
  
  name4df <- 
    paste(srcname$prefix, "_", name4df$df_long_main, groupvar, 
          "_stats4scale", sep = "")
  
  dat2proc <- get(name4df) %>% data.frame()
  dat2proc <- melt(dat2proc, id = c(groupvar, "scale"))
  
  dat2proc <- dcast(dat2proc, as.formula(paste(groupvar, "~ scale + variable", sep = "")))
  
  name4df_new <- paste(name4df, "wide", sep = "_")
  
  assign(name4df_new,
         dat2proc,
         env = .GlobalEnv)
  
  cat("** Created variable: ", name4df_new, "\n", sep = "")
  cat("*** Done! *** \n\n")
  
}