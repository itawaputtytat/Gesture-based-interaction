# Objective ---------------------------------------------------------------
# Draw template matrix plot for AttrakDiff 2


# Dummy -------------------------------------------------------------------
plotItemProfile4attrakdiff_template <- 
  ggplot() +   
  #dummy
  geom_point(data = NULL,
             aes(x = 0,
                 y = 0),
             size = 0)


# Split-Line at Zero ------------------------------------------------------
plotItemProfile4attrakdiff_template <- 
  plotItemProfile4attrakdiff_template + 
  geom_hline(aes(yintercept = 0),
             colour = "black",
             size = 0.5)


# Rectangles for Subscales PQ, HQI, HQS and ATT ---------------------------
plotItemProfile4attrakdiff_template <- 
  plotItemProfile4attrakdiff_template +
  
  annotate("rect",
           xmin = 28+0.4,
            xmax = 28-6.4,
            ymin = -3.5,
            ymax = -3.4,
            fill = "green1",
            alpha = 1) +
  annotate("rect",
           xmin = 21+0.4,
                xmax = 21-6.4,
                ymin = -3.5,
                ymax = -3.4,
            fill = "deepskyblue1",
            alpha = 1) +
  annotate("rect",
           xmin = 14+0.4,
           xmax = 14-6.4,
           ymin = -3.5,
           ymax = -3.4,
           fill = "cadetblue2",
           alpha = 1) +
  annotate("rect",
           xmin = 7+0.4,
           xmax = 7-6-0.4,
           ymin = -3.5,
           ymax = -3.4,
           fill = "yellow1",
           alpha = 1)
