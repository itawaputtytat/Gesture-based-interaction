# Objective ---------------------------------------------------------------
# Draw template matrix plot for AttrakDiff 2


# Dummy -------------------------------------------------------------------

plotItemProfile_attrakdiff_template <- 
  #ggplot() + geom_point(data = NULL, aes(x = 0, y = 0), size = 0)
  ggplot() + geom_blank()


# Split-Line at Zero ------------------------------------------------------

plotItemProfile_attrakdiff_template <- 
  plotItemProfile_attrakdiff_template + 
  
  geom_hline(aes(yintercept = 0),
             colour = "black",
             size = 0.5) + 
  
  geom_vline(aes(xintercept = 21.5),
             colour = "grey60",
             size = 0.5) +
  
  geom_vline(aes(xintercept = 14.5),
             colour = "grey60",
             size = 0.5) +
  
  geom_vline(aes(xintercept = 7.5),
             colour = "grey60",
             size = 0.5) 


# Rectangles for Subscales PQ, HQI, HQS and ATT ---------------------------

plotItemProfile_attrakdiff_template <- 
  plotItemProfile_attrakdiff_template +
  
  annotate("rect",
           xmin = 28+0.45,
           xmax = 28-6.45,
           ymin = -3.5,
           ymax = -3.4,
           fill = "green1",
           alpha = 1) +
  annotate("rect",
           xmin = 21+0.45,
           xmax = 21-6.45,
           ymin = -3.5,
           ymax = -3.4,
           fill = "deepskyblue1",
           alpha = 1) +
  annotate("rect",
           xmin = 14+0.45,
           xmax = 14-6.45,
           ymin = -3.5,
           ymax = -3.4,
           fill = "cadetblue2",
           alpha = 1) +
  annotate("rect",
           xmin = 7+0.45,
           xmax = 7-6-0.45,
           ymin = -3.5,
           ymax = -3.4,
           fill = "yellow1",
           alpha = 1) +
  
  geom_text(aes(x = 28-0.5, y = -3.3), 
            label = "Pragmatic quality", fontface = "bold", hjust = 0, vjust = 0, angle = 0) +
  geom_text(aes(x = 21-0.5, y = -3.3), 
            label = "Hedonic quality \n(identity)", fontface = "bold", hjust = 0, vjust = 0.6, angle = 0) +
  geom_text(aes(x = 14-0.5, y = -3.3), 
            label = "Hedonic quality \n(stimulation)", fontface = "bold", hjust = 0, vjust = 0.6, angle = 0) +
  geom_text(aes(x = 7-0.5, y = -3.3), 
            label = "Attractiveness", fontface = "bold", hjust = 0, vjust = 0, angle = 0)

