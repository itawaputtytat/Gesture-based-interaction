reset_name4data <- function () {
  
  dataname <- c()
  dataname$df_main <- "dat"
  dataname$df_long_main <- paste(dataname$df_main, "_long4", sep = "")
  
  assign("name4data", dataname, env = .GlobalEnv)
  
}