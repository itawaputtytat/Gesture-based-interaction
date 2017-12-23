## Create vector for selection arrival measures in sett_query
createVector_var_sxx <- function(var_dist) {
  paste0("_", 
         c(var_dist, 
           ifelse(grepl("dti_m", var_dist), "tti_s", "dti_m")))
}
