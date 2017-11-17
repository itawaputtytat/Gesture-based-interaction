computeMeansSD_attrakdiff <- function (name4df, suffix, var2group) {
  
  cat("* Processing with function computeMeanSD4AttrakDiff \n")
  
  name4df <- paste(name4df, suffix, sep = "")
  dat2proc <- get(name4df) %>% data.frame()
  
  dat2proc <- 
    dat2proc %>%
    group_by_(var2group, "scale") %>%
    summarise(id_n = length(unique(id)),
              score_mean = mean(value, na.rm = T),
              score_sd   = sd(value, na.rm = T)) %>%
    mutate(score_error = qnorm(0.975) * score_sd / sqrt(id_n))
  
  name4df_new <- paste(name4df, ".stats4scales", sep = "")
  
  assign(name4df_new,
         dat2proc,
         env = .GlobalEnv) 
  
  cat("** Created variable: ", name4df_new, "\n", sep = "")
  cat("*** Done! *** \n\n")
  
}