
makePrintablePlot_ItemProfile <- function(plotdat, q) {

  plotdat <- 
    plotdat + 
    
    ggtitle("") +
    
    labs(x = "",
         y = "",
         shape = set4plot$legend.title[[q]],
         colour = set4plot$legend.title[[q]])
  
  
  plotdat <- 
    
    plotdat +
    
    theme_bw() + 
    theme(plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), units = "line"),
          #plot.title = element_text(size = 10, face = "bold"),
          #plot.title = element_blank(),
          panel.border = element_rect(colour = "black"),
          axis.title.x = element_text(size = 8.5, face = "bold"),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7),
          legend.background = element_blank(),
          #legend.position = "right",
          #legend.position = c(0.1, 0.9),
          #legend.margin = unit(-1, "lines"),
          legend.key = element_blank(),
          legend.key.height = unit(0.85, "line"),
          #legend.title = element_blank(),
          legend.title = element_text(size = 7),
          legend.text = element_text(size = 7))


  return(plotdat)

}