
# Objective ---------------------------------------------------------------

## Compute statistic key values for data grouped by groupvar



# Processing --------------------------------------------------------------

dataStats4groupVar <- function (dataname, var2group) {
  
  cat("* Processing with function: dataStats4groupVar \n")
  
  ## Get data
  data4stats <- get(paste(dataname, "_long4", var2group, sep = ""))
  
  ## For consistent naming
  data4stats$group <- data4stats[, var2group]
  
  ## Compute values
  data4stats <- data4stats %>%
    group_by(group, variable) %>%
    summarise(median = as.numeric(median(value, na.rm = T)),
              mean   = mean(value, na.rm = T))
  
  ## Re-rename group variable to original var2group
  colnames(data4stats)[which(colnames(data4stats) == "group")] = var2group
  
  dataname_new <- paste(paste(dataname, "_long4", var2group, sep = ""), "stats", sep = "_")
  
  ## Re-assign data with processed data
  assign(dataname_new, 
         data4stats,
         env = .GlobalEnv)
  
  cat("** Created variable: ", dataname_new, "\n", sep = "")
  cat("*** Done! *** \n\n")
      
} 