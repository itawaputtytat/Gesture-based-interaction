createLabels4PlotLegend <- function (dataname, groupvar) 
{
  
  data2process <- data.frame(get(dataname))
  
  levels2find <- levels(data2process[, groupvar])
  
  rowfinder <- match(levels2find, set4plot$labels4legend$tablename_suffix)
  
  labels2return <- set4plot$labels4legend$condlabel[rowfinder]
  
  return(labels2return)
  
}