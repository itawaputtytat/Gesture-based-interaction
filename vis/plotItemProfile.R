plotItemProfile <- function (name4df, statkeyvalue, groupvar) {
  
  outputFunProc(R)
  
  cat(paste("Visualize item profile (", statkeyvalue, ") ", 
            "for group: ", groupvar, " ... \n", sep =""))
  
  # Load data
  name4df <- paste(name4df, ".long", ".stats_", groupvar, sep = "")
  data2process <- get(name4df)
  data2process <- data.frame(data2process)
  
  cat("* Using data: ", name4df, "\n", sep = "")
  
  # Load item labels
  labels4plot <- labels[[name4dbsrc$q]]
  data2process$variable <- factor(data2process$variable)
  
  # Reverese levels of data variable for correct dispay (because coord_flip)
  neworderedlevels <- rev(levels(data2process$variable))
  rev_itemnr <- rev(c(1:length(levels(data2process$variable))))
  
  # Create adjusted colour set
  #data2process[, groupvar] <- factor(data2process[, groupvar],
  #                                   levels = name4dbsrc$suffix)
  data2process[, groupvar] <- factor(data2process[, groupvar],
                                     levels = unique(data2process[, groupvar]))
  level_order <- levels(data2process[, groupvar])
  colset4groupvar <- unlist(colset[[groupvar]])
  colset4groupvar <- colset4groupvar[as.character(level_order)]
  
  # Create adjusted shape set
  shapeset4groupvar <- unlist(shapeset[[groupvar]])
  shapeset4groupvar <- shapeset4groupvar[as.character(level_order)]
  
  plotdat <- 
    ggplot(data = data2process,
         aes_string(x = "variable",
                    y = statkeyvalue,
                    group = groupvar,
                    col = groupvar,
                    shape = groupvar),
         size = 3) + 
    geom_line() +
    geom_point() + 
    scale_color_manual(values = colset4groupvar, 
                       labels = createLabels4PlotLegend(name4df, groupvar)) +
    scale_shape_manual(values = shapeset4groupvar, 
                       labels = createLabels4PlotLegend(name4df, groupvar)) + 
    scale_x_discrete(limits = neworderedlevels,
                     labels = labels4plot[rev_itemnr]) +
    scale_y_continuous(breaks = set4plot$ybreaks4profile[[name4dbsrc$q]]) + 
    coord_flip(ylim = set4plot$ylim4items[[name4dbsrc$q]])
  
  return(plotdat)
  
  cat("*** Done! *** \n")
  
}