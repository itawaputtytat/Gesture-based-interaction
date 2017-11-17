plotBoxplot <- function(name4df, yvar, groupvar, vector4order = NULL) {
  
  outputFunProc(R)
  
  ## Get data
  dat2proc <- get(name4df) 
  
  # Set right colours
  colset_order <- unique(dat2proc[, groupvar])
  colset4groupvar <- unlist(set4plot$colour[[groupvar]])
  colset4groupvar <- as.character(colset4groupvar[colset_order])
  
  if (is.null(vector4order))
    vector4order <- c(1:length(colset_order))
  
  shapeset4groupvar <- unlist(set4plot$shape[[groupvar]])
  shapeset4groupvar <- as.numeric(shapeset4groupvar[colset_order])
  print(shapeset4groupvar)
  
  
  ## Plot data
  plotdat <- 
    ggplot(data = dat2proc,
           aes_string(x = groupvar,
                      y = yvar)) + 
    stat_boxplot(geom = 'errorbar', size = 0.25) +
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
    geom_point(aes_string(col = groupvar, shape = groupvar),
               size = 0.35,
               position = position_jitter(w = 0.05, h = 0)) + 
    # Set colours for groupvar and new labels
    scale_x_discrete(labels = createLabels4PlotLegend(name4df, groupvar, vector4order),
                     limits = colset_order[vector4order]) +
    scale_color_manual(values = colset4groupvar,
                       labels = createLabels4PlotLegend(name4df, groupvar, vector4order)) +
    scale_fill_manual(values = colset4groupvar,
                      labels = createLabels4PlotLegend(name4df, groupvar, vector4order)) + 
    scale_shape_manual(values = shapeset4groupvar,
                      labels = createLabels4PlotLegend(name4df, groupvar, vector4order)) +
    ggtitle(yvar)
  
  return(plotdat)
  
  outputDone()
  
}