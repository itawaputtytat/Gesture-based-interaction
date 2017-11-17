plotItemProfile_taeg <- function (name4df, statkeyvalue, groupvar) {
  
  outputFunProc(R)
  
  cat(paste("Visualize item profile (", statkeyvalue, ") ", 
            "for group: ", groupvar, " ... \n", sep =""))
  
  # Load data
  name4df <- paste(name4df, ".long.stats_", groupvar, sep = "")
  cat("* Using data:", name4df)
  dat2proc <- 
    get(name4df) %>% 
    data.frame()
  
  # Load item labels
  labels <- set4items[["taeg"]]$labels
  #dat2proc$variable <- factor(dat2proc$variable, levels = dat2proc$variable[set4items[["taeg"]]$itemnrs4scales_ordered])
  dat2proc$variable <- factor(dat2proc$variable)
  
  # Reverese levels of data variable for correct dispay (because coord_flip)
  neworderedlevels <- rev(unique(dat2proc$variable))
  rev_itemnr <- rev(set4items[["taeg"]]$itemnrs4scales_ordered)
  
  if (!is.null(groupvar)) {
    
    # Create adjusted colour set
    dat2proc[, groupvar] <- as.factor(dat2proc[, groupvar])
    level_order <- unique(dat2proc[, groupvar])
    colset4groupvar <- unlist(set4plot$colour[[groupvar]])
    colset4groupvar <- colset4groupvar[as.character(level_order)]
    
    # Create adjusted shape set
    shapeset4groupvar <- unlist(set4plot$shape[[groupvar]])
    shapeset4groupvar <- shapeset4groupvar[as.character(level_order)]
    
    plotdat <- plotItemProfile_taeg_template + 
      geom_point(data = dat2proc,
                 aes_string(x = "variable",
                            y = statkeyvalue,
                            group = groupvar,
                            col = groupvar,
                            shape = groupvar),
                 size = 3) + 
      geom_line(data = dat2proc,
                aes_string(x = "variable",
                           y = statkeyvalue,
                           group = groupvar,
                           col = groupvar)) +
      scale_color_manual(values = colset4groupvar, 
                         labels = createLabels4PlotLegend(name4df, groupvar)) +
      scale_shape_manual(values = shapeset4groupvar, 
                         labels = createLabels4PlotLegend(name4df, groupvar)) + 
      scale_x_discrete(#limits = neworderedlevels,
                       limits = set4items$taeg$varnames[rev_itemnr],
                       labels = labels[rev_itemnr]) +
      scale_y_continuous(breaks = seq(-3, 3, 1)) + 
      coord_flip(xlim = c(0 - 1, length(unique(dat2proc$variable))) + 1,
                 ylim = set4plot$ylim4items[[set4proc$q]])
    
  } else {
    
    plotdat <- plotItemProfile_taeg_template + 
      geom_point(data = dat2proc,
                 aes_string(x = "variable",
                            y = statkeyvalue),
                 size = 3) + 
      geom_line(data = dat2proc,
                aes_string(x = "variable",
                           y = statkeyvalue,
                           group = 1)) +
      scale_x_discrete(#limits = neworderedlevels,
                       limits = set4items$taeg$varnames[rev_itemnr],
                       labels = labels[rev_itemnr]) +
      scale_y_continuous(breaks = seq(0, 5, 1)) + 
      coord_flip(xlim = c(0 - 1, length(unique(dat2proc$variable))) + 1,
                 ylim = set4plot$ylim4items[[set4proc$q]], 
                 expand = c(0, 0))
    
  }
  
  
  outputDone()
  return(plotdat)
}