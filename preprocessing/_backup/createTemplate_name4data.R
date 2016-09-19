createTemplate_name4data <- function (srcname) {
  
  ## Settings for data names
  dataname <- c()
  dataname$df <- paste(srcname$prefix, srcname$name4data_main, sep = "_")
  dataname$main_long <- paste(dataname$df, "long", sep = "_")
  dataname$main_stats <- paste(dataname$df, "stats", sep = "_")
  
  assign("name4data",
         ## To avoid return data.frame as list
         data.frame(dataname, stringsAsFactors = F),
         env = .GlobalEnv)
  
}