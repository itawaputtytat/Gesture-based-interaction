
makePrintablePlot_boxplot <- function(plotdat, q) {
  
  plotdat <- 
    plotdat +
    ggtitle("") +
    labs(x = "",
         y = set4plot$axis.title.x.score[[q]],
         face = "bold",
         shape = set4plot$legend.title[[q]],
         colour = set4plot$legend.title[[q]]) + 
    scale_y_continuous(breaks = set4plot$ybreaks4score[[q]]) + 
    coord_cartesian(ylim = set4plot$ylim4score[[q]])

  return(plotdat)
  
}


