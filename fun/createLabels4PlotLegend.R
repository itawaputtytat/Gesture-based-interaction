createLabels4PlotLegend <- function (name4df, groupvar, vector4order = NULL) {
  
  dat2proc <- data.frame(get(name4df))
  levels2find <- unique(dat2proc[, groupvar])
  if (!is.null(vector4order))
    levels2find <- levels2find[vector4order]
  rowfinder <- match(levels2find, set4plot$labels4legend$tablename_suffix)
  labels <- set4plot$labels4legend$condlabel[rowfinder]
  return(labels)
}