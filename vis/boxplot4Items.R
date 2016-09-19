## Boxplot for each item

boxplot4Items <- function() {
  
  cat("* Visualising with function: boxplot4Item \n")
  
  if (showboxplot4items == T) {
    invisible(lapply(varnames4items, function (x)
      plotBoxplot(name4data$df, x, x, ylim4items, "expfocus")))
  } else
    cat("** Skipped: Plotting of boxplots for each item \n") 
  
  cat("*** Done! ***")
  
}