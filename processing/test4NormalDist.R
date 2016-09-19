test4NormalDist <- function(data, varnames4items, itemset) {
  
  cat("Shapiro-Wilk test for normal distribution ... \n\n")
  
  remember_sig <- c()
  
  # Loop through each condition
  for(c in unique(data$cond)) {
    
    cat(paste("Test items in condition-level: ", c, "\n\n", sep = ""))
    
    # Filter data for condition
    temp_data <- 
      data %>% 
      filter(cond == c)
    
    # Loop through each item
    for(item in 1:length(varnames4items)) {
      
      # Print current item
      cat(paste("Item: ", varnames4items[item], " = ", itemset[item], 
                "\n", sep = ""))
      
      # Filter data_temp for curren item
      temp_item <- temp_data[, varnames4items[item]]
      
      # Run Shapiro-Wilk test
      temp_test <- shapiro.test(temp_item)
      
      # Print test statistics
      cat(paste("W = ", round(temp_test$statistic, 3), "\n",
                "p = ", round(temp_test$p.value, 4),
                sep = ""))
      cat("\n\n")
      
      # Collect names of signifikanct cases
      if (temp_test$p.value < 0.05) {
        remember_sig <- c(remember_sig,
                          paste("in cond. ", c, 
                                " in item: ", varnames4items[item],
                                " (p = ", round(temp_test$p.value, 4), ")", 
                                sep = ""))
        
      } # Print test statistics
    } # item
    cat(rep("*", 50), sep = "")
    cat("\n\n")
  } # cond
  
  # Print collection of significant cases
  if(length(remember_sig != 0)) {
    cat("Significant values (normality assumption violated):")
    cat("\n\n")
    cat(remember_sig, sep = "\n")
    cat("\n")
    cat(paste(length(remember_sig), "sign. values from overall", 
              length(unique(data$cond)) * length(varnames4items), "items"),
        sep = "")
  }
} 