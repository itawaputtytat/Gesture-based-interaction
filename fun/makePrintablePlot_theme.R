makePrintablePlot_theme <- function(plotdat) {
  
  plotdat <- 
    plotdat + 
    theme_bw() + 
    theme(plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), units="line"),
          #plot.title = element_text(size = 10, face = "bold"),
          plot.title = element_blank(),
          panel.border = element_rect(colour = "black"),
          axis.title.y = element_text(size = 6, face = "bold"),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 5),
          axis.text.y = element_text(size = 5),
          ## Remove legend in boxplots
          legend.position = "NULL")
  
  return(plotdat)
  
  
}
 