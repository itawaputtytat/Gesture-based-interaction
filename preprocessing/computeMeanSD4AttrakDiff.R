computeMeanSD4AttrakDiff <- function (srcname, dataname, var2group) {
  
  cat("* Processing with function computeMeanSD4AttrakDiff \n")
  
  dataname <- 
    paste(srcname$prefix, "_", dataname$df_long_main, var2group, sep = "")
  
  data2process <- get(dataname)
  
  data2process <- 
    data2process %>%
    group_by_(var2group, "scale") %>%
    summarise(id_n = length(unique(id)),
              score_mean = mean(value, na.rm = T),
              score_sd   = sd(value, na.rm = T)) %>%
    mutate(score_error = qnorm(0.975) * score_sd / sqrt(id_n))
  
  dataname_new <- paste(dataname, "stats4scale", sep = "_")
  
  assign(dataname_new,
         data2process,
         env = .GlobalEnv) 
  
  cat("** Created variable: ", dataname_new, "\n", sep = "")
  cat("*** Done! *** \n\n")
  
}