plotItemProfile4attrakdiff <- function (dataname, statkeyvalue, groupvar) {
  
  cat("* Visualising with function: plotItemProfile4attrakdiff \n")
  
  cat(paste("Visualize item profile (", statkeyvalue, ") ", 
            "for group: ", groupvar, " ... \n", sep =""))
  
  # Load data
  dataname <- paste(dataname, "_long4", groupvar, "_stats", sep = "")
  cat("* Using data:", dataname)
  data2process <- get(dataname)
  data2process <- data.frame(data2process)
  
  # Load item labels
  labels <- labels[[name4dbsrc$q]]
  data2process$variable <- factor(data2process$variable)
  
  # Reverese levels of data variable for correct dispay (because coord_flip)
  neworderedlevels <- rev(unique(data2process$variable))
  rev_itemnr <- rev(itemnr_attrakdiff_ordered)
  
  # Create adjusted colour set
  data2process[, groupvar] <- as.factor(data2process[, groupvar])
  level_order <- unique(data2process[, groupvar])
  colset4groupvar <- unlist(colset[[groupvar]])
  colset4groupvar <- colset4groupvar[as.character(level_order)]
  
  cat("\n", colset4groupvar,"\n")
  
  # Create adjusted shape set
  shapeset4groupvar <- unlist(shapeset[[groupvar]])
  shapeset4groupvar <- shapeset4groupvar[as.character(level_order)]
  
  plotdata <- plotItemProfile4attrakdiff_template + 
    geom_point(data = data2process,
               aes_string(x = "variable",
                          y = statkeyvalue,
                          group = groupvar,
                          col = groupvar,
                          shape = groupvar),
               size = 3) + 
    geom_line(data = data2process,
              aes_string(x = "variable",
                         y = statkeyvalue,
                         group = groupvar,
                         col = groupvar)) +
    scale_color_manual(values = colset4groupvar, 
                       labels = createLabels4PlotLegend(dataname, groupvar)) +
    scale_shape_manual(values = shapeset4groupvar, 
                       labels = createLabels4PlotLegend(dataname, groupvar)) + 
    scale_x_discrete(limits = neworderedlevels,
                     labels = labels[rev_itemnr]) +
    scale_y_continuous(breaks = seq(-3, 3, 1)) + 
    coord_flip(xlim = c(0 - 1, length(unique(data2process$variable))) + 1,
               ylim = set4plot$ylim4items[[name4dbsrc$q]])
  
  return(plotdata)
  
  cat("*** Done! *** \n")

  
}