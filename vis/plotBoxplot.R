plotBoxplot <- 
  
  function(dataname, yvar, yname, ylimiter, groupvar) {
  
  cat("* Visualising with function: plotBoxplot \n")
  cat("** Using data: ", dataname, "\n", sep = "")
  
  cat(paste("Boxplot (", yvar, " = ", yname, ") ... ", sep = ""))
  
  ## Get data
  data2process <- get(dataname)
  data2process <- data.frame(data2process)
  
  # Set right colours
  colset_order <- levels(data2process[, groupvar])
  colset4groupvar <- unlist(colset[[groupvar]])
  colset4groupvar <- as.character(colset4groupvar[colset_order])
  
  ## Plot data
  plotdata <- 
    ggplot(data = data2process,
           aes_string(x = groupvar,
                      y = yvar)) + 
    stat_boxplot(geom ='errorbar', size = 0.25) + 
    geom_boxplot(fill = "white",
                 notch = TRUE, 
                 notchwidth = .9,
                 outlier.size = 1,
                 outlier.shape = NA,
                 lwd = 0.25,
                 fatten = 1.5) +
    geom_boxplot(aes_string(fill = groupvar),
                 alpha = 0.5,
                 notch = TRUE, 
                 notchwidth = .9,
                 outlier.size = 1,
                 outlier.shape = NA,
                 lwd = 0.25,
                 fatten = 1.5) +
    geom_point(aes_string(col = groupvar),
               size = 1,
               position = position_jitter(w = 0.05, h = 0)) + 
    ## Set colours for groupvar and new labels
    scale_x_discrete(labels = createLabels4PlotLegend(dataname, groupvar)) + 
    scale_color_manual(values = colset4groupvar, 
                       labels = createLabels4PlotLegend(dataname, groupvar)) +
    scale_fill_manual(values = colset4groupvar, 
                      labels = createLabels4PlotLegend(dataname, groupvar))
  
  return(plotdata)
         
  cat("*** Done! *** \n")
  
}