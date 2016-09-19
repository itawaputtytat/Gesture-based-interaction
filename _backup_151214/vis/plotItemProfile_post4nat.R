## Add coloured bars on left side

plotItemProfile_post4nat <- function () {
  
  cat("* Post-Visualisaiong with function: plotItemProfile_post4nat \n")
  
  plotdata <- 
    
    last_plot() +  
    
    annotate("rect",
             xmin = 35+0.4,
             xmax = 35-4.4,
             ymin = -0.5,
             ymax = -0.4,
             fill = "royalblue",
             alpha = 1) +
    annotate("rect",
             xmin = 30+0.4,
             xmax = 30-4.4,
             ymin = -0.5,
             ymax = -0.4,
             fill = "salmon",
             alpha = 1) +
    annotate("rect",
             xmin = 25+0.4,
             xmax = 25-4.4,
             ymin = -0.5,
             ymax = -0.4,
             fill = "springgreen",
             alpha = 1) +
    annotate("rect",
             xmin = 20+0.4,
             xmax = 20-4.4,
             ymin = -0.5,
             ymax = -0.4,
             fill = "gold",
             alpha = 1) +
    annotate("rect",
             xmin = 15+0.4,
             xmax = 15-4.4,
             ymin = -0.5,
             ymax = -0.4,
             fill = "deeppink",
             alpha = 1) +
    annotate("rect",
             xmin = 10+0.4,
             xmax = 10-4.4,
             ymin = -0.5,
             ymax = -0.4,
             fill = "slateblue",
             alpha = 1) +
    annotate("rect",
             xmin = 5+0.4,
             xmax = 5-4-0.4,
             ymin = -0.5,
             ymax = -0.4,
             fill = "orange",
             alpha = 1)
  
  plot(plotdata)
  
  return(plotdata)
  
  cat("*** Done! *** \n")
  
  pauseAndContinue()
  
}