plotMatrix4attrakdiff <- function (dataname, groupvar) {
  
  cat("* Visualising with function: plotMatrix4attrakdiff \n")
  
  dataname <- paste(dataname, "_long4", groupvar, "_stats4scale_wide", sep = "")
  data2process <- get(dataname)
  
  # Create adjusted colour set
  #data2process[, groupvar] <- as.factor(data2process[, groupvar])
  level_order <- levels(data2process[, groupvar])
  colset4groupvar <- unlist(colset[[groupvar]])
  colset4groupvar <- colset4groupvar[as.character(level_order)]
  
  # Create adjusted shape set
  shapeset4groupvar <- unlist(shapeset[[groupvar]])
  shapeset4groupvar <- shapeset4groupvar[as.character(level_order)]
  
  cat(" ** Using template: plotMatrix4attrakdiff_template \n")
  
  plotdata <- plotMatrix4attrakdiff_template + 

    # Plot data for confidence intervals for each condition
    geom_rect(data = data2process,
              aes_string(xmin = "pq_score_mean - pq_score_error",
                         xmax = "pq_score_mean + pq_score_error",
                         ymin = "hq_score_mean - hq_score_error",
                         ymax = "hq_score_mean + hq_score_error",
                         fill = groupvar,
                         colour = groupvar),
              fill = colset4groupvar,
              alpha = c(rep(0.25, length(colset4groupvar))) ) +
    
    # Plot mean scores for each condition
    geom_point(data = data2process, 
               aes_string(x = "pq_score_mean",
                   y = "hq_score_mean",
                   colour = groupvar,
                   shape = groupvar),
               size = 7) +
    scale_colour_manual(values = colset4groupvar, 
                        labels = createLabels4PlotLegend(dataname, groupvar)) + 
    scale_shape_manual(values = shapeset4groupvar, 
                       labels = createLabels4PlotLegend(dataname, groupvar))
  
  plot(plotdata)
  
  return(plotdata)
  
  cat("*** Done! *** \n")
  
  pauseAndContinue()
  
}