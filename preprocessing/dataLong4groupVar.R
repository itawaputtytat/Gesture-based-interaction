
# Objective ---------------------------------------------------------------
# Reshape data to long format
# Reverse factor levels


# Function ----------------------------------------------------------------

dataLong4groupVar <- function(dataname, var2group) {
  
  cat("* Processing with function: dataLong4groupVar \n")
  
  data_long <- get(dataname)[, c("id", var2group, varnames4items)]
  data_long <- melt(data_long, id = c("id", var2group))
  
  dataname_new <- paste(dataname, "_long4", var2group, sep = "")
  
  # Reverse factor levels of items for profile plotting
  assign(dataname_new,
         as.data.frame(revLevels(data_long, "variable")),
         env = .GlobalEnv)
  
  cat("** Created variable: ", dataname_new, "\n", sep = "")
  cat("*** Done! *** \n\n")
  
}
