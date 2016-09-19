
makePrintablePlot_boxPlot <- function(plotdata) {
  
  plotdata <- 
    last_plot() +
    
    ggtitle("") +
    
    labs(x = "",
         y = set4plot$axis.title.x.score[[name4dbsrc$q]],
         face = "bold",
         shape = set4plot$legend.title[[name4dbsrc$q]],
         colour = set4plot$legend.title[[name4dbsrc$q]]) + 
  
    scale_y_continuous(breaks = set4plot$ybreaks4score[[name4dbsrc$q]]) + 
    
    coord_cartesian(ylim = set4plot$ylim4score[[name4dbsrc$q]]) + 

  return(plotdata)
  
}


