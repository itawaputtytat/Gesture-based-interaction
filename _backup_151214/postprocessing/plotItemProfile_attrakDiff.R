plotItemProfile <- function(data, yvar, colset, scale_y) {
  
  cat(paste("Visualize item profile (", yvar, ") ... ", sep =""))
  
  data$variable <- factor(data$variable)
  neworderedlevels <- levels(data$variable)[rev(itemset_attrakdiff_ordered)]
  
  plotdata <- 
    ggplot(data = data,
         aes_string(x = "variable",
                    y = yvar,
                    group = "cond",
                    col = "cond")) + 
    geom_point() + 
    geom_line() +
    scale_color_manual(values = colset) +
    scale_x_discrete(limits = neworderedlevels)+
    ggtitle(yvar) +
    labs(x = "",
         y = "scale") +
    coord_flip(ylim = scale_y)
  
  plot(plotdata)
  
  return(plotdata)
  
  cat("Done! \n")
  
}