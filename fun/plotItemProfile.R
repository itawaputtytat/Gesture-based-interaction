plotItemProfile <- function (name4df, statkeyvalue = NULL, groupvar = NULL, q, yscale_pos = "left") {
  
  outputFunProc(R)
  
  # Load data
  #if (!is.null(groupvar))
    name4df <- paste(name4df, ".long.stats_", groupvar, sep = "") #else
      #name4df <- paste(name4df, ".long.stats", sep = "")
  cat("* Using data: ", name4df, "\n", sep = "")
  dat2proc <- get(name4df)
  dat2proc <- data.frame(dat2proc)
  
  # Load item labels
  labels4plot <- set4items[[q]]$labels
  dat2proc$variable <- factor(dat2proc$variable)
  
  # Reverse levels of data variable for correct dispay (because coord_flip)
  neworderedlevels <- rev(levels(dat2proc$variable))
  rev_itemnr <- rev(c(1:length(levels(dat2proc$variable))))

  if (!is.null(groupvar)) {
    
    cat(paste("Visualize item profile (", statkeyvalue, ") ", 
              "for group: ", groupvar, " ... \n", sep =""))
    
    # Create adjusted colour set
    #dat2proc[, groupvar] <- factor(dat2proc[, groupvar],
    #                                   levels = name4dbsrc$suffix)
    dat2proc[, groupvar] <- factor(dat2proc[, groupvar],
                                   levels = unique(dat2proc[, groupvar]))
    
    level_order <- levels(dat2proc[, groupvar])
    colset4groupvar <- unlist(set4plot$colours[[groupvar]])
    colset4groupvar <- colset4groupvar[as.character(level_order)]
   
    # Create adjusted shape set
    shapeset4groupvar <- unlist(set4plot$shape[[groupvar]])
    shapeset4groupvar <- shapeset4groupvar[as.character(level_order)]
    
    plotdat <- 
      ggplot(data = dat2proc,
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
                       labels = labels4plot[rev_itemnr],
                       position = yscale_pos) +
      scale_y_continuous(breaks = set4plot$ybreaks4profile[[q]]) + 
      coord_flip(ylim = set4plot$ylim4items[[q]])
     
  } else {
    
    plotdat <- 
      ggplot(data = dat2proc,
             aes_string(x = "variable",
                        y = statkeyvalue,
                        group = 1), ## Workaround for only one observation
             size = 3) + 
      geom_line() +
      geom_point() + 
      scale_x_discrete(limits = neworderedlevels,
                       labels = labels4plot[rev_itemnr],
                       position = yscale_pos) +
      scale_y_continuous(breaks = set4plot$ybreaks4profile[[q]]) + 
      coord_flip(ylim = set4plot$ylim4items[[q]])
    
  }

  
  return(plotdat)
  
  cat("*** Done! *** \n")
  
}