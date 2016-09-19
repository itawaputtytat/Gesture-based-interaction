
makePrintablePlot_ItemProfile <- function(plotdata) {

  plotdata <- 
    plotdata + 
    
    ggtitle("") +
    
    labs(x = "",
         y = "Scale",
         shape = set4plot$legend.title[[name4dbsrc$q]],
         colour = set4plot$legend.title[[name4dbsrc$q]])
  
  
  plotdata <- 
    
    plotdata +
    
    theme_bw() + 
    theme(plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), units = "line"),
          #plot.title = element_text(size = 10, face = "bold"),
          plot.title = element_blank(),
          panel.border = element_rect(colour = "black"),
          axis.title.x = element_text(size = 8.5, face = "bold"),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7),
          legend.position = "top",
          legend.margin = unit(-1, "lines"),
          legend.key = element_blank(),
          legend.key.height = unit(0.85, "line"),
          #legend.title = element_blank(),
          legend.title = element_text(size = 7),
          legend.text = element_text(size = 7))


  return(plotdata)

}