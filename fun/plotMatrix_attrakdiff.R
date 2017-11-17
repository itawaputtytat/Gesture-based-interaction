plotMatrix_attrakdiff <- function (name4df, suffix, groupvar) {
  
  cat("* Visualising with function: plotMatrix4attrakdiff \n")
  
  name4df <- paste(name4df, suffix, sep = "")
  dat2proc <- get(name4df)
  
  # Create adjusted colour set
  #dat2proc[, groupvar] <- as.factor(dat2proc[, groupvar])
  level_order <- unique(dat2proc[, groupvar])
  colset4groupvar <- unlist(set4plot$colour[[groupvar]])
  colset4groupvar <- colset4groupvar[as.character(level_order)]
  
  # Create adjusted shape set
  shapeset4groupvar <- unlist(set4plot$shape[[groupvar]])
  shapeset4groupvar <- shapeset4groupvar[as.character(level_order)]
  
  cat(" ** Using template: plotMatrix4attrakdiff_template \n")
  
  plotdat <- 
    plotMatrix_attrakdiff_template + 

    # Plot data for confidence intervals for each condition
    geom_rect(data = dat2proc,
              aes_string(xmin = "pq_score_mean - pq_score_error",
                         xmax = "pq_score_mean + pq_score_error",
                         ymin = "hq_score_mean - hq_score_error",
                         ymax = "hq_score_mean + hq_score_error",
                         fill = groupvar,
                         colour = groupvar),
              fill = colset4groupvar,
              size = 0.25,
              alpha = c(rep(0.25, length(colset4groupvar))) ) +
    
    # Plot mean scores for each condition
    geom_point(data = dat2proc, 
               aes_string(x = "pq_score_mean",
                   y = "hq_score_mean",
                   colour = groupvar,
                   shape = groupvar),
               size = 3) +
    scale_colour_manual("Interaction type\n[97.5 % CI]",
                        values = colset4groupvar, 
                        labels = createLabels4PlotLegend(name4df, groupvar)) + 
    scale_shape_manual("Interaction type\n[97.5 % CI]",
                       values = shapeset4groupvar, 
                       labels = createLabels4PlotLegend(name4df, groupvar))
  
  return(plotdat)
}