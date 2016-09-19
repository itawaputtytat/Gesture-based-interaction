dcastData4AttrakDiff_stats4scale <- function (srcname, dataname, groupvar) {

  cat("* Processing with function: dcastData4AttrakDiff_stats4scale \n")
  
  dataname <- 
    paste(srcname$prefix, "_", dataname$df_long_main, groupvar, 
          "_stats4scale", sep = "")
  
  data2process <- get(dataname)
  
  data2process <- melt(data2process, id = c(groupvar, "scale"))
  
  data2process <- dcast(data2process, as.formula(paste(groupvar, "~ scale + variable", sep = "")))
  
  dataname_new <- paste(dataname, "wide", sep = "_")
  
  assign(dataname_new,
         data2process,
         env = .GlobalEnv)
  
  cat("** Created variable: ", dataname_new, "\n", sep = "")
  cat("*** Done! *** \n\n")
  
}